# Comment these out for the defaults
#OVERRIDE_REPORT_YEAR <- 2017 # Default: current academic year's fall term
#WRITE_OUTPUT <- FALSE # Default: TRUE
#CLEANUP      <- FALSE # Default: TRUE
#TEST         <- TRUE  # Default: FALSE

project_path <- "C:/"
input_path <- file.path(project_path, "input")
output_path <- file.path(project_path, "output")

require(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
require(magrittr) # pipes
require(stringr) # string manipulation
require(lubridate)
require(dbplyr)
require(odbc)
require(yaml)
require(optparse)

# Now, load the campus configuration
scripts_path <- file.path(project_path, "scripts")
if (str_sub(scripts_path, -1) == "/") {
    scripts_path = str_sub(scripts_path, 1, -2 )
}

sessionInfo()

if (!exists("WRITE_OUTPUT")) {
    WRITE_OUTPUT <- NA_integer_
}

###
### Define some local variables
###
report_year <- as.numeric(format(Sys.time(), "%Y"))

# Set report_month to actual month to allow adjustment of report_year based on month
# Otherwise, set to -1
report_month <- as.numeric(format(Sys.time(), "%m"))

# Fix report year to be the year of the Fall term
report_year <- report_year - if_else(report_month < 7, 1, 0)

report_year <- ifelse(!is.na(OVERRIDE_REPORT_YEAR),OVERRIDE_REPORT_YEAR,report_year)

report_data_year_start <- as.Date(str_c(report_year-1,"07-01",sep="-"))
report_data_year_end <- as.Date(str_c(report_year,"06-30",sep="-"))

fn_report_code <- "e12"

fn_E12 <- str_c("ipeds_",report_year,"_",fn_report_code,".txt")


#######################################
#
# HERE IS WHERE THE DATA IS RETRIEVED
#
#######################################

#
# Get terms data from DB
#
terms <- getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
    select( Term_ID,
            Term_Index,
            Term_Name = Semester,
            Semester = Term_Abbreviation,
            Term_Start_Date,
            Term_Census_Date,
            Term_End_Date,
            Term_Reporting_Year = Reporting_Year_FSS,
            Academic_Year = Reporting_Academic_Year_FSS ) %>%
    collect() %>%
    mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) )

reporting_terms <- terms %>%
    filter( Term_Reporting_Year == report_year )

person <- getColleagueData( "PERSON" ) %>%
    filter( FIRST.NAME != "" ) %>%
    select( Student_ID = ID, 
            First_Name = FIRST.NAME, 
            Last_Name = LAST.NAME, 
            Birth_Date = BIRTH.DATE,
            Gender = GENDER, 
            ETHNIC, 
            CITIZENSHIP, 
            RESIDENCE.COUNTRY, 
            VISA.TYPE, 
            PER.ETHNICS, 
            PER.RACES, 
            X.ETHNICS.RACES ) %>%
    collect() %>%
    mutate( CITIZENSHIP = if_else( CITIZENSHIP == "USA", "", CITIZENSHIP ) ) %>%
    mutate( IPEDS_Race = case_when(
        X.ETHNICS.RACES == "FOR" ~ "Nonresident Alien",
        X.ETHNICS.RACES == "HIS" ~ "Hispanic/Latino",
        X.ETHNICS.RACES == "MULTI" ~ "Two or more races",
        X.ETHNICS.RACES == "AN" ~ "American Indian or Alaska Native",
        X.ETHNICS.RACES == "AS" ~ "Asian",
        X.ETHNICS.RACES == "BL" ~ "Black or African American",
        X.ETHNICS.RACES == "HP" ~ "Native Hawaiian or Other Pacific Islander",
        X.ETHNICS.RACES == "WH" ~ "White",
        TRUE ~ "Race or ethnicity unknown"
    ),
    IPEDS_Race_Gender_Code = case_when(
        X.ETHNICS.RACES == "FOR" & Gender == "M" ~ "FYRACE01",
        X.ETHNICS.RACES == "FOR" & Gender == "F" ~ "FYRACE02",
        X.ETHNICS.RACES == "HIS" & Gender == "M" ~ "FYRACE25",
        X.ETHNICS.RACES == "HIS" & Gender == "F" ~ "FYRACE26",
        X.ETHNICS.RACES == "MULTI" & Gender == "M" ~ "FYRACE37",
        X.ETHNICS.RACES == "MULTI" & Gender == "F" ~ "FYRACE38",
        X.ETHNICS.RACES == "AN" & Gender == "M" ~ "FYRACE27",
        X.ETHNICS.RACES == "AN" & Gender == "F" ~ "FYRACE28",
        X.ETHNICS.RACES == "AS" & Gender == "M" ~ "FYRACE29",
        X.ETHNICS.RACES == "AS" & Gender == "F" ~ "FYRACE30",
        X.ETHNICS.RACES == "BL" & Gender == "M" ~ "FYRACE31",
        X.ETHNICS.RACES == "BL" & Gender == "F" ~ "FYRACE32",
        X.ETHNICS.RACES == "HP" & Gender == "M" ~ "FYRACE33",
        X.ETHNICS.RACES == "HP" & Gender == "F" ~ "FYRACE34",
        X.ETHNICS.RACES == "WH" & Gender == "M" ~ "FYRACE35",
        X.ETHNICS.RACES == "WH" & Gender == "F" ~ "FYRACE36",
        TRUE ~ if_else( Gender == "M", "FYRACE13", "FYRACE14" )
    )
    ) %>%
    select( Student_ID, First_Name, Last_Name, Birth_Date, Gender, IPEDS_Race_Gender_Code )

student_enrollment <- term_enrollment( report_year ) %>%
    rename( Student_ID = ID ) %>%
    left_join( person )

race_gender_cols <- c( FYRACE01=NA_character_, FYRACE02=NA_character_,
                       FYRACE25=NA_character_, FYRACE26=NA_character_,
                       FYRACE27=NA_character_, FYRACE28=NA_character_,
                       FYRACE29=NA_character_, FYRACE30=NA_character_,
                       FYRACE31=NA_character_, FYRACE32=NA_character_,
                       FYRACE33=NA_character_, FYRACE34=NA_character_,
                       FYRACE35=NA_character_, FYRACE36=NA_character_,
                       FYRACE37=NA_character_, FYRACE38=NA_character_,
                       FYRACE13=NA_character_, FYRACE14=NA_character_ )
e12_a_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 SLEVEL=NA_character_, Filler_1=NA_character_, 
                 race_gender_cols )
e12_b_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 Filler_1=NA_character_, CREDHRSU=NA_character_, CONTHRS=NA_character_ )

ipeds_e12_a <- student_enrollment %>%
    select( Student_ID, IPEDS_Race_Gender_Code ) %>%
    distinct() %>%
    group_by( IPEDS_Race_Gender_Code ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    spread( IPEDS_Race_Gender_Code, Students, fill="000000" ) %>%
    ungroup() %>%
    add_column( !!!race_gender_cols[!names(race_gender_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(race_gender_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    mutate( UNITID = '198668',
            SURVSECT = toupper(fn_report_code),
            PART = 'A',
            SLEVEL = '1',
            Filler_1 = '        ' ) %>%
    select( all_of(names(e12_a_cols)) )

ipeds_e12_b <- student_enrollment %>%
    distinct() %>%
    summarise( CREDHRSU = sprintf("%08d", sum(Credits) ) ) %>%
    mutate( UNITID = '198668',
            SURVSECT = toupper(fn_report_code),
            PART = 'B',
            CONTHRS = '        ',
            Filler_1 = '         ') %>%
    select( all_of(names(e12_b_cols)) )

if (WRITE_OUTPUT) {
    write.table( data.frame(ipeds_e12_a), file.path(output_path, fn_E12), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
    write.table( data.frame(ipeds_e12_b), file.path(output_path, fn_E12), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
}