# Original source code can be found on GitHub at the link below:
# https://github.com/Haywood-Community-College-IERG/2022-04-07-NCAIR-Utilizing-R-for-IPEDS-Reporting

### Define GLOBAL USER variables
fn_report_code <- "e12"
report_year_data_adjustment <- -1

# Comment for current academic year's fall term
#OVERRIDE_REPORT_YEAR <- 2017 
WRITE_OUTPUT <- TRUE # Comment line for default: FALSE
CLEANUP <- FALSE # Comment line for default: TRUE
TEST <- TRUE # Comment line for default: FALSE

project_path <- file.path(".")
input_path <- file.path(project_path, "input")
output_path <- file.path(project_path, "output")

## Define Frontmatter - Part 2

### Load Packages
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(stringr) # string manipulation
library(lubridate)
# Load local package
library(haywoodcc) # renv::install("Haywood-Community-College-IERG/haywoodcc")

### Define common report variables
if (!exists("OVERRIDE_REPORT_YEAR")) {
    OVERRIDE_REPORT_YEAR <- NA_integer_
}
# Repeat for WRITE_OUTPUT, CLEANUP, TEST
if (!exists("WRITE_OUTPUT")) {
    WRITE_OUTPUT <- NA_integer_
}
if (!exists("CLEANUP")) {
    CLEANUP <- NA_integer_
}
if (!exists("TEST")) {
    TEST <- NA_integer_
}

###
### Define some local variables
###
current_year <- as.numeric(format(Sys.time(), "%Y"))
current_month <- as.numeric(format(Sys.time(), "%m"))

report_time_str <- format(Sys.time(),"%Y%m%d_%H%M")

report_year <- as.integer(current_year - ifelse(current_month < 7, 1, 0))
report_year <- ifelse(!is.na(OVERRIDE_REPORT_YEAR),OVERRIDE_REPORT_YEAR,report_year)
report_year_folder <- str_c(report_year,as.integer(substring(report_year,3,4))+1,sep='-')

report_data_year_start <- as.Date(str_c(report_year-1,"07-01",sep="-"))
report_data_year_end <- as.Date(str_c(report_year,"06-30",sep="-"))

fn_E12 <- stringr::str_c("ipeds_",report_year,"_",fn_report_code,"_",report_time_str,".txt")

# Now adjust the report year so data collection uses the correct year
report_year <- report_year + report_year_data_adjustment

ft_cohort_year <- str_c(report_year,"FT")
pt_cohort_year <- str_c(report_year,"PT")
tf_cohort_year <- str_c(report_year,"TF")
tp_cohort_year <- str_c(report_year,"TP")
rf_cohort_year <- str_c(report_year,"RF")
rp_cohort_year <- str_c(report_year,"RP")

non_summer_term_id <- str_c(report_year, "SU")

#######################################
#
# HERE IS WHERE THE DATA IS RETRIEVED
#
#######################################

### Uncomment this to use files in the data folder
#setCfg("data_source","from_file_path","data")
cfg <- getCfg()

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

# Additionally:
# - Get data from PERSON and ipeds_cohorts from files/DB
# - Get data via haywoodcc package for credential_seekers and term_enrollment

reporting_terms <- terms %>%
    filter( Term_Reporting_Year == report_year + 1 )

person <- getColleagueData( "PERSON" ) %>%
    select( Campus_ID = ID, 
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
    select( Campus_ID, First_Name, Last_Name, Birth_Date, Gender, IPEDS_Race_Gender_Code )

student_credential_seekers_all <- credential_seekers( report_year, exclude_hs = TRUE ) %>%
    rename( Campus_ID = ID )

student_credential_seekers <- student_credential_seekers_all %>%
    group_by( Campus_ID ) %>%
    summarise( Credential_Seeker = max(Credential_Seeker), .groups = "drop" )

ipeds_cohorts <- getColleagueData( "ipeds_cohorts", schema="local", version="latest" ) %>%
    select( Campus_ID = ID, Term_ID, Term_Cohort, Cohort ) %>%
    collect() %>%
    inner_join( reporting_terms %>% select(Term_ID), by="Term_ID" )
    
student_term_enrollment <- term_enrollment( report_year ) %>%
    rename( Campus_ID = ID ) 

# Data retrieved
# 
# - Demographics
# - Data frame of non-high school credential seekers
# - Data frame of enrollment for the `r report_year` terms

# As an example, create a df with distance load identified
#    ...this is used later on to determine if enrolled exclusively online
student_enrollment_all <- student_term_enrollment %>%
    left_join( person, by="Campus_ID" ) %>%
    left_join( student_credential_seekers, by="Campus_ID" ) %>%
    mutate( Credential_Seeker = coalesce(Credential_Seeker,2),
            de_level = case_when(
                Distance_Courses=="All" ~ 100,
                Distance_Courses=="At least 1" ~ 10,
                TRUE ~ 1) ) %>%
    left_join( ipeds_cohorts, by=c("Campus_ID","Term_ID") ) %>%
    mutate( Term_Cohort_Abbrev = substring(coalesce(Term_Cohort,
                                             str_c("    ",
                                                   if_else(Credential_Seeker==1,"R","N"),
                                                   substring(Status,1,1))),5,6) ) %>%
    select( -c("Term_Cohort","Cohort") )

se_report_year_non_summer_ids <- student_enrollment_all %>%
    filter( Term_ID != non_summer_term_id ) %>%
    select( Campus_ID ) %>%
    distinct()

student_enrollment_all %<>% inner_join( se_report_year_non_summer_ids, by="Campus_ID" )

se_first_term <- student_enrollment_all %>%
    left_join( terms %>% select(Term_ID,Term_Index), by="Term_ID" ) %>%
    group_by( Campus_ID ) %>%
    summarise( Term_Index = min(Term_Index), .groups = "drop" )

se_de <- student_enrollment_all %>%
    group_by(Campus_ID) %>%
    summarise( de_level = sum(de_level), .groups = "drop" ) %>%
    mutate( mod_100 = mod(de_level,100),
            mod_10  = mod(de_level,10) ) %>%
    mutate( DE = case_when(
                    mod_100==0 ~ "ENROLL_EXCLUSIVE",
                    de_level<10 ~ "NOTENROLL",
                    TRUE ~ "ENROLL_SOME") ) %>%
    select( Campus_ID, DE )

student_enrollment <- student_enrollment_all %>%
    left_join( terms %>% select(Term_ID,Term_Index), by="Term_ID" ) %>%
    inner_join( se_first_term, by=c("Campus_ID","Term_Index") ) %>%
    select( -c(Distance_Courses,de_level) ) %>%
    left_join( se_de, by="Campus_ID" )

student_enrollment %>% group_by(Credential_Seeker, DE) %>% summarise(n = n(), .groups = "drop")
student_enrollment %>% group_by(Term_Cohort_Abbrev) %>% summarise(n = n(), .groups = "drop")

se_de_fall <- student_enrollment_all %>%
    filter( Semester == "FA" ) %>%
    group_by(Campus_ID, Credential_Seeker) %>%
    summarise( de_level = sum(de_level), .groups = "drop" ) %>%
    mutate( mod_100 = mod(de_level,100),
            mod_10  = mod(de_level,10) ) %>%
    mutate( DE = case_when(
                    mod_100==0 ~ "ENROLL_EXCLUSIVE",
                    de_level<10 ~ "NOTENROLL",
                    TRUE ~ "ENROLL_SOME") ) %>%
    select( Campus_ID, Credential_Seeker, DE )

se_de_fall %>% group_by(Credential_Seeker, DE) %>% summarise(n = n(), .groups = "drop")

se_ids <- student_enrollment %>%
    inner_join( reporting_terms %>% select(Term_ID), by="Term_ID" ) %>%
    select(Campus_ID) %>%
    distinct()
se_ids %>% summarise(n = n(), .groups = "drop")

se_cohorts <- student_enrollment %>%
    select(Campus_ID,Term_ID,Term_Cohort_Abbrev,Credential_Seeker,DE,IPEDS_Race_Gender_Code) %>%
    mutate( LINE_a = case_when(
                        Term_Cohort_Abbrev == "FT" ~ " 1",
                        Term_Cohort_Abbrev == "TF" ~ " 2",
                        Term_Cohort_Abbrev == "RF" ~ " 3",
                        Term_Cohort_Abbrev == "NF" ~ " 7",
                        Term_Cohort_Abbrev == "HF" ~ " 7",
                        Term_Cohort_Abbrev == "PT" ~ "15",
                        Term_Cohort_Abbrev == "TP" ~ "16",
                        Term_Cohort_Abbrev == "RP" ~ "17",
                        Term_Cohort_Abbrev == "NP" ~ "21",
                        Term_Cohort_Abbrev == "HP" ~ "21",
                        TRUE ~ "00" ),
            LINE_c = if_else(Term_Cohort_Abbrev %in% c("FT","PT","TF","TP","RF","RP"),"1","2") )
se_cohorts %>% group_by(Credential_Seeker, LINE_c, DE) %>% summarise(n = n(), .groups = "drop")

## Create IPEDS data frames
race_gender_cols <- c( FYRACE01=NA_character_, FYRACE02=NA_character_,
                       FYRACE25=NA_character_, FYRACE26=NA_character_,
                       FYRACE27=NA_character_, FYRACE28=NA_character_,
                       FYRACE29=NA_character_, FYRACE30=NA_character_,
                       FYRACE31=NA_character_, FYRACE32=NA_character_,
                       FYRACE33=NA_character_, FYRACE34=NA_character_,
                       FYRACE35=NA_character_, FYRACE36=NA_character_,
                       FYRACE37=NA_character_, FYRACE38=NA_character_,
                       FYRACE13=NA_character_, FYRACE14=NA_character_ )

# Also create e12_*_cols for the parts A, B, and C
e12_a_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 LINE=NA_character_, Filler_1=NA_character_, 
                 race_gender_cols )
e12_b_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 Filler_1=NA_character_, CREDHRSU=NA_character_, CONTHRS=NA_character_,
                 Filler_2=NA_character_ )
e12_c_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 LINE=NA_character_, 
                 ENROLL_EXCLUSIVE=NA_character_, ENROLL_SOME=NA_character_ )

# This creates Part A.
# The first line looks like this in the output:
# 198668E12A 3       000000000000000001000004000001000002000000000001...
# 
# > str(se_chorts)
# tibble [280 x 8] (S3: tbl_df/tbl/data.frame)
# $ Campus_ID             : num [1:280] 5000331 5000690 5001380 5001681 5001697 ...
# $ Term_ID               : chr [1:280] "2021SP" "2021SP" "2021SP" "2021SP" ...
# $ Term_Cohort_Abbrev    : chr [1:280] "RP" "RF" "NP" "RP" ...
# $ Credential_Seeker     : num [1:280] 1 1 2 1 2 1 1 1 1 2 ...
# $ DE                    : chr [1:280] "ENROLL_EXCLUSIVE" "ENROLL_SOME" "ENROLL_EXCLUSIVE" "ENROLL_EXCLUSIVE" ...
# $ IPEDS_Race_Gender_Code: chr [1:280] "FYRACE36" "FYRACE36" "FYRACE36" "FYRACE35" ...
# $ LINE_a                : chr [1:280] "17" " 3" "21" "17" ...
# $ LINE_c                : chr [1:280] "1" "1" "2" "1" ...

ipeds_e12_a <- se_cohorts %>%
    rename( LINE = LINE_a ) %>%
    select( Campus_ID, LINE, IPEDS_Race_Gender_Code ) %>%
    distinct() %>%
    mutate( student = 1 ) %>%
    ### <b>
    group_by( LINE, IPEDS_Race_Gender_Code ) %>%
    summarise( Students = sprintf("%06d", sum(student) ), .groups = "drop" ) %>%
    spread( IPEDS_Race_Gender_Code, Students, fill="000000" ) %>%
    ungroup() %>%
    add_column( !!!race_gender_cols[!names(race_gender_cols) %in% names(.)] ) %>%
    ### </b>
    mutate_at(.vars=names(race_gender_cols), 
              function(x) {return( coalesce(x,"000000") )} ) %>%
    mutate( UNITID = cfg$school$ipeds,
            SURVSECT = toupper(fn_report_code),
            PART = 'A',
            Filler_1 = strrep(' ',7) ) %>%
    select( all_of(names(e12_a_cols)) )

# Similar steps are used to create Parts B and C

# > str(student_enrollment_all)
# tibble [338 x 16] (S3: tbl_df/tbl/data.frame)
# $ Campus_ID             : num [1:338] 5000331 5000690 5001380 5001681 5001697 ...
# $ Term_ID               : chr [1:338] "2021SP" "2021SP" "2021SP" "2021SU" ...
# $ Term_Reporting_Year   : num [1:338] 2020 2020 2020 2020 2020 2020 2020 2020 2020 2020 ...
# $ Semester              : chr [1:338] "SP" "SP" "SP" "SU" ...
# $ Credits               : num [1:338] 3 14 4 3 8 7 12 20 13 9 ...
# $ Status                : chr [1:338] "PT" "FT" "PT" "PT" ...
# $ Distance_Courses      : chr [1:338] "All" "At least 1" "All" "All" ...
# $ Enrollment_Status     : chr [1:338] "Enrolled" "Enrolled" "Enrolled" "Enrolled" ...
# $ First_Name            : logi [1:338] NA NA NA NA NA NA ...
# $ Last_Name             : logi [1:338] NA NA NA NA NA NA ...
# $ Birth_Date            : Date[1:338], format: "1981-02-21" "1987-08-26" "1990-01-31" "1990-01-31" ...
# $ Gender                : chr [1:338] "F" "F" "F" "F" ...
# $ IPEDS_Race_Gender_Code: chr [1:338] "FYRACE36" "FYRACE36" "FYRACE36" "FYRACE36" ...
# $ Credential_Seeker     : num [1:338] 1 1 2 2 1 2 1 1 1 1 ...
# $ de_level              : num [1:338] 100 10 100 100 100 100 10 10 100 100 ...
# $ Term_Cohort_Abbrev    : chr [1:338] "RP" "RF" "NP" "HP" ...

ipeds_e12_b <- student_enrollment_all %>%
    distinct() %>%
    summarise( CREDHRSU = sprintf("%08d", sum(Credits) ), .groups = "drop" ) %>%
    mutate( UNITID = cfg$school$ipeds,
            SURVSECT = toupper(fn_report_code),
            PART = 'B',
            CONTHRS = '        ',
            Filler_1 = '         ',
            Filler_2 = '        ' ) %>%
    select( all_of(names(e12_b_cols)) )

ipeds_e12_c <- se_cohorts %>%
    rename( LINE = LINE_c ) %>%
    select( Campus_ID, LINE, DE ) %>%
    distinct() %>%
    group_by( LINE, DE ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ), .groups = "drop" ) %>%
    spread( DE, Students, fill="000000" ) %>%
    ungroup() %>%
    add_column( !!!c("ENROLL_EXCLUSIVE", "ENROLL_SOME") ) %>%
    mutate_at(.vars=c("ENROLL_EXCLUSIVE", "ENROLL_SOME"), function(x) {return( coalesce(x,"000000") )} ) %>%
    mutate( UNITID = cfg$school$ipeds,
            SURVSECT = toupper(fn_report_code),
            PART = 'C' ) %>%
    select( all_of(names(e12_c_cols)) )

## Write out the data

if (WRITE_OUTPUT) {
    write.table( data.frame(ipeds_e12_a), 
                 file.path(output_path, fn_E12), 
                 sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
    write.table( data.frame(ipeds_e12_b), 
                 file.path(output_path, fn_E12), 
                 sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_e12_c), 
                 file.path(output_path, fn_E12), 
                 sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
}
