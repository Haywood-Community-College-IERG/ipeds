OVERRIDE_REPORT_YEAR <- NA # Set to NA to use current year
#OVERRIDE_REPORT_YEAR <- 2017 # Set to NA to use current year

WRITE_OUTPUT <- TRUE


ir_root <- "L:/IERG"

project_path <- file.path(ir_root,"Reporting","IPEDS","R")
input_path <- file.path(project_path, "input")
output_path <- file.path(project_path, "output")

nsc_path <- file.path(ir_root, "Data", "NSC")
ipeds_path <- file.path(ir_root, "Data", "IPEDS")

package_date <- "2018-07-01" # date of the CRAN snapshot that the checkpoint
                             # package uses

# # if checkpoint is not yet installed, install it (for people using this
# # system for the first time)
# if (!require(checkpoint)) {
#     install.packages("checkpoint")
#     require(checkpoint)
# }

# # install packages for the specified CRAN snapshot date
# checkpoint(snapshotDate = package_date,
#            checkpointLocation = ir_root,
#            project = project_path,
#            verbose = T,
#            scanForPackages = T,
#            use.knitr = F)

library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(stringr) # string manipulation
library(dbplyr)
library(odbc)
library(yaml)
library(lubridate)

# Enter local packages needed for this particular script 
if (!require(haywoodcc)) {
    #library(devtools)
    devtools::install_git("https://github.com/haywood-ierg/haywoodcc.git", git="external")
    require(haywoodcc)
}

sessionInfo()

# Now, load the campus configuration
cfg <- yaml.load_file(file.path(ir_root, "Data/config.yml"))
scripts_path <- cfg$R$scripts_path
if (str_sub(scripts_path, -1) == "/") {
    scripts_path = str_sub(scripts_path, 1, -2 )
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

catalog_year <- report_year - 1

distance_programs <- data.frame( Major = c( "10100", "25800", "25120", "55180", "55850", "55220", "55860", "25590", "25340","25260" ),
                                 stringsAsFactors = FALSE ) %>%
    mutate( Is_Distance = 'Y' )

fn_report_code <- "com"

fn_COM <- str_c("ipeds_",report_year,"_",fn_report_code,".txt")

#
# Get all the data from DB
#
person <- getColleagueData( "PERSON" ) %>%
    filter( FIRST.NAME != "" ) %>%
    select( Student_ID = ID, 
            First_Name = FIRST.NAME, 
            Last_Name = LAST.NAME, 
            Birth_Date = BIRTH.DATE,
            Gender = GENDER, 
            ETHNIC, PER.ETHNICS, 
            PER.RACES, 
            X.ETHNICS.RACES, 
            CITIZENSHIP, 
            RESIDENCE.COUNTRY, 
            VISA.TYPE ) %>%
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
                X.ETHNICS.RACES == "FOR" & Gender == "M" ~ "CRACE01",
                X.ETHNICS.RACES == "FOR" & Gender == "F" ~ "CRACE02",
                X.ETHNICS.RACES == "HIS" & Gender == "M" ~ "CRACE25",
                X.ETHNICS.RACES == "HIS" & Gender == "F" ~ "CRACE26",
                X.ETHNICS.RACES == "MULTI" & Gender == "M" ~ "CRACE37",
                X.ETHNICS.RACES == "MULTI" & Gender == "F" ~ "CRACE38",
                X.ETHNICS.RACES == "AN" & Gender == "M" ~ "CRACE27",
                X.ETHNICS.RACES == "AN" & Gender == "F" ~ "CRACE28",
                X.ETHNICS.RACES == "AS" & Gender == "M" ~ "CRACE29",
                X.ETHNICS.RACES == "AS" & Gender == "F" ~ "CRACE30",
                X.ETHNICS.RACES == "BL" & Gender == "M" ~ "CRACE31",
                X.ETHNICS.RACES == "BL" & Gender == "F" ~ "CRACE32",
                X.ETHNICS.RACES == "HP" & Gender == "M" ~ "CRACE33",
                X.ETHNICS.RACES == "HP" & Gender == "F" ~ "CRACE34",
                X.ETHNICS.RACES == "WH" & Gender == "M" ~ "CRACE35",
                X.ETHNICS.RACES == "WH" & Gender == "F" ~ "CRACE36",
                TRUE ~ if_else( Gender == "M", "CRACE13", "CRACE14" )
            ),
            IPEDS_Race_Code = case_when(
                X.ETHNICS.RACES == "FOR"   ~ "CRACE17",
                X.ETHNICS.RACES == "HIS"   ~ "CRACE41",
                X.ETHNICS.RACES == "MULTI" ~ "CRACE47",
                X.ETHNICS.RACES == "AN"    ~ "CRACE42",
                X.ETHNICS.RACES == "AS"    ~ "CRACE43",
                X.ETHNICS.RACES == "BL"    ~ "CRACE44",
                X.ETHNICS.RACES == "HP"    ~ "CRACE45",
                X.ETHNICS.RACES == "WH"    ~ "CRACE46",
                TRUE ~ "CRACE23"
            ),
            SEX = if_else( Gender == 'M', "CRACE15", if_else( Gender == 'F', "CRACE16", NA_character_) )
        ) %>%
    select( Student_ID, First_Name, Last_Name, Birth_Date, Gender, IPEDS_Race, IPEDS_Race_Gender_Code, IPEDS_Race_Code, SEX )

#
# Need to get all programs to match on credentials earned, but then need to filter list of programs
# to those offered during report year as they all need to be reported.
#
# Do we need to report the programs we don't offer?
#
acad_program_reqmts_all <- getColleagueData( "ACAD_PROGRAM_REQMTS" ) %>%
    select( Program = ACPR.ACAD.PROGRAM,
            Catalog = ACPR.CATALOG,
            Credit_Hours = ACPR.CRED ) %>%
    collect() %>%
    filter( substring(Program,1,1) %in% c('A','D','C') )

acad_program_reqmts_ry <- acad_program_reqmts_all %>%
    filter( Catalog == catalog_year ) %>%
    mutate( Current_Program = "Yes" ) %>%
    select( Program, Current_Program )

acad_programs <- getColleagueData( "ACAD_PROGRAMS" ) %>%
    select( Program = ACAD.PROGRAMS.ID,
            Program_Length = ACPG.CMPL.MONTHS,
            Major = ACPG.MAJORS,
            CIPCODE = ACPG.CIP,
            Catalogs = ACPG.CATALOGS ) %>%
    filter( !(Program %in% c('BSP', 'AHS', 'CONED', '=GED', '=HISET', 'C11', 'C50')) ) %>%
    collect() %>%
    mutate( Catalog = strsplit(Catalogs, ", ") ) %>%
    unnest() %>%
    select( -Catalogs ) %>%
    left_join( acad_program_reqmts_all ) %>%
    mutate( AWLEVEL = case_when(
                          substring(Program,1,1) == 'A' ~ "03",
                          substring(Program,1,1) == 'D' ~ "02",
                          substring(Program,1,1) == 'C' ~ "01",
                          TRUE ~ '9') ) %>%
    left_join( distance_programs ) %>%
    mutate( Is_Distance = coalesce(Is_Distance,'N') )

acad_programs_ry <- acad_programs %>%
    inner_join( acad_program_reqmts_ry ) %>%
    select( CIPCODE, AWLEVEL, Is_Distance ) %>%
    distinct() %>%
    mutate( MAJORNUM = 1, 
            CRACE01 = "000000", CRACE02 = "000000",
            CRACE25 = "000000", CRACE26 = "000000",
            CRACE27 = "000000", CRACE28 = "000000",
            CRACE29 = "000000", CRACE30 = "000000",
            CRACE31 = "000000", CRACE32 = "000000",
            CRACE33 = "000000", CRACE34 = "000000",
            CRACE35 = "000000", CRACE36 = "000000",
            CRACE37 = "000000", CRACE38 = "000000",
            CRACE13 = "000000", CRACE14 = "000000",
            CRACE15 = "000000", CRACE16 = "000000",
            DistanceED = if_else( Is_Distance == 'Y', 1, 
                                  if_else( Is_Distance == 'N', 2, NA_real_ ) )
        )


# student_programs_dates <- getColleagueData( "STUDENT_PROGRAMS__STPR_DATES", version="Full" ) %>%
#     filter( ItemNumber == 1 ) %>%
#     select( Student_ID = STPR.STUDENT,
#             Program = STPR.ACAD.PROGRAM,
#             Program_Start_Date = STPR.START.DATE,
#             Program_End_Date = STPR.END.DATE,
#             EffectiveDatetime ) %>%
#     collect()

student_programs <- getColleagueData( "STUDENT_PROGRAMS" ) %>%
    filter( STPR.STATUS == 'G' ) %>%
    select( Student_ID = STPR.STUDENT,
            Program = STPR.ACAD.PROGRAM,
            Catalog = STPR.CATALOG ) %>% #,
            #EffectiveDatetime ) %>%
    collect() #%>%
    #inner_join( student_programs_dates )

acad_credentials_base <- getColleagueData( "ACAD_CREDENTIALS" ) %>%
    filter( ACAD.INSTITUTIONS.ID == "0019844",
            ACAD.CAST.DATE >= report_data_year_start,
            ACAD.CAST.DATE <= report_data_year_end,
            ( !is.na(ACAD.DEGREE.DATE) | !is.na(ACAD.CCD.DATE) ),   # DMO A 191105
            !(ACAD.ACAD.PROGRAM %in% c("AHS", "=GED", "HSEGED")) ) %>%
    select( Student_ID = ACAD.PERSON.ID,
            Term_ID = ACAD.TERM, 
            Program = ACAD.ACAD.PROGRAM,
            Graduation_Date = ACAD.CAST.DATE
            ) %>%
    collect() %>%
    mutate( Graduated = 1,
            MAJORNUM = 1 ) %>%
    left_join( person ) %>%
    left_join( student_programs ) %>%
    left_join( acad_programs ) %>%
    left_join( acad_program_reqmts_ry ) %>%
    mutate( Current_Program = coalesce(Current_Program,"No"),
            Age = as.integer(as.duration(Birth_Date %--% Graduation_Date) / dyears(1)) ) %>%
    arrange( CIPCODE, AWLEVEL, Program, Catalog, Student_ID ) %>%
    #
    # EXCEPTIONS:
    #
    mutate( CIPCODE = case_when(
                        Student_ID == "1149818" & Term_ID == "2018SU" & Program == "A10100EC" ~ "24.0101",
                        Student_ID == "1024339" & Term_ID == "2017SU" & Program == "A30140" ~ "50.0602",
                        Student_ID == "1021104" & Term_ID == "2017SU" & Program == "A30300" ~ "50.0201",
                        Student_ID == "1101800" & Term_ID == "2017SU" & Program == "A30300" ~ "50.0201",
                        Student_ID == "1062698" & Term_ID == "2017SU" & Program == "A40200" ~ "15.0303",
                        Student_ID == "1075372" & Term_ID == "2017SU" & Program == "A55220" ~ "13.1210",
                        Student_ID == "1101713" & Term_ID == "2017SU" & Program == "A55280" ~ "24.0102",
                        # Student_ID == "XXXX" & Term_ID == "XXXX" & Program == "XXXX" ~ "",
                        TRUE ~ CIPCODE ),
            AWLEVEL = case_when( 
                        Student_ID == "1149818" & Term_ID == "2018SU" & Program == "A10100EC" ~ "03",
                        Student_ID == "1024339" & Term_ID == "2017SU" & Program == "A30140" ~ "03",
                        Student_ID == "1021104" & Term_ID == "2017SU" & Program == "A30300" ~ "03",
                        Student_ID == "1101800" & Term_ID == "2017SU" & Program == "A30300" ~ "03",
                        Student_ID == "1062698" & Term_ID == "2017SU" & Program == "A40200" ~ "03",
                        Student_ID == "1075372" & Term_ID == "2017SU" & Program == "A55220" ~ "03",
                        Student_ID == "1101713" & Term_ID == "2017SU" & Program == "A55280" ~ "03",
                        TRUE ~ AWLEVEL ),
            Is_Distance = case_when( 
                        Student_ID == "1149818" & Term_ID == "2018SU" & Program == "A10100EC" ~ "Y",
                        Student_ID == "1024339" & Term_ID == "2017SU" & Program == "A30140" ~ "N",
                        Student_ID == "1021104" & Term_ID == "2017SU" & Program == "A30300" ~ "N",
                        Student_ID == "1101800" & Term_ID == "2017SU" & Program == "A30300" ~ "N",
                        Student_ID == "1062698" & Term_ID == "2017SU" & Program == "A40200" ~ "N",
                        Student_ID == "1075372" & Term_ID == "2017SU" & Program == "A55220" ~ "Y",
                        Student_ID == "1101713" & Term_ID == "2017SU" & Program == "A55280" ~ "N",
                        TRUE ~ Is_Distance )
          )  										

acad_credentials_age_groups <- acad_credentials_base %>%
    group_by( Student_ID ) %>%
    summarise( Age = min(Age) ) %>%
    mutate( Age_Group = case_when(
                            Age < 18 ~ "AGE1",
                            Age < 25 ~ "AGE2",
                            Age < 40 ~ "AGE3",
                            Age >= 40 ~ "AGE4",
                            TRUE ~ "AGE5"
                        )
          ) %>%
    select( Student_ID, Age_Group )
    
acad_credentials <- acad_credentials_base %>%
    left_join( acad_credentials_age_groups )

race_gender_cols <- c( CRACE01=NA_character_, CRACE02=NA_character_,
                       CRACE25=NA_character_, CRACE26=NA_character_,
                       CRACE27=NA_character_, CRACE28=NA_character_,
                       CRACE29=NA_character_, CRACE30=NA_character_,
                       CRACE31=NA_character_, CRACE32=NA_character_,
                       CRACE33=NA_character_, CRACE34=NA_character_,
                       CRACE35=NA_character_, CRACE36=NA_character_,
                       CRACE37=NA_character_, CRACE38=NA_character_,
                       CRACE13=NA_character_, CRACE14=NA_character_ )
age_cols <- c( AGE1=NA_character_, AGE2=NA_character_, AGE3=NA_character_, AGE4=NA_character_, AGE5=NA_character_ )
gender_cols <- c( CRACE15=NA_character_, CRACE16=NA_character_ )
race_cols <- c( CRACE17=NA_character_, CRACE41=NA_character_, CRACE42=NA_character_,
                CRACE43=NA_character_, CRACE44=NA_character_, CRACE45=NA_character_,
                CRACE46=NA_character_, CRACE47=NA_character_, CRACE23=NA_character_ )
com_a_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 MAJORNUM=NA_character_, CIPCODE=NA_character_, AWLEVEL=NA_character_, 
                 race_gender_cols )
com_b_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 MAJORNUM=NA_character_, CIPCODE=NA_character_, AWLEVEL=NA_character_, 
                 DistanceED=NA_character_ )
com_c_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 race_gender_cols )
com_d_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 CTLEVEL=NA_character_, gender_cols, Filler_1=NA_character_, 
                 race_cols, Filler_2=NA_character_, age_cols )


#
# Create the cohort row
#
ipeds_com_a_base <- acad_credentials %>%
    distinct() %>%
    group_by( MAJORNUM, CIPCODE, AWLEVEL, IPEDS_Race_Gender_Code ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    spread( IPEDS_Race_Gender_Code, Students, fill="000000" ) %>%
    ungroup() %>%
    add_column( !!!race_gender_cols[!names(race_gender_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(race_gender_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'COM',
            PART = 'A' ) %>%
    select_( .dots=names(com_a_cols) )

ipeds_com_a_missing <- acad_programs_ry %>%
    anti_join( ipeds_com_a_base, by = c("CIPCODE","AWLEVEL","MAJORNUM") ) %>%
    mutate( UNITID = '198668',
        SURVSECT = 'COM',
        PART = 'A' ) %>%
    select_( .dots=names(com_a_cols) )

ipeds_com_a <- bind_rows( ipeds_com_a_base, ipeds_com_a_missing ) %>%
    arrange( CIPCODE, AWLEVEL )

ipeds_programs <- ipeds_com_a %>%
    select( CIPCODE, AWLEVEL ) %>%
    group_by( CIPCODE ) %>%
    summarise_each(funs(paste(., collapse = ", ")))

ipeds_com_b_base <- acad_credentials %>%
    distinct() %>%
    mutate( DistanceED = if_else( Is_Distance == 'Y', 1, 
                                  if_else( Is_Distance == 'N', 2, NA_real_ ) ) ) %>%
    group_by( MAJORNUM, CIPCODE, AWLEVEL ) %>%
    summarise( DistanceED = min(DistanceED) ) %>%
    ungroup() %>%
    mutate( UNITID = '198668',
            SURVSECT = 'COM',
            PART = 'B' ) %>%
    select_( .dots=names(com_b_cols) )

ipeds_com_b_missing <- acad_programs_ry %>%
    anti_join( ipeds_com_a_base, by = c("CIPCODE","AWLEVEL") ) %>%
    select( MAJORNUM, CIPCODE, AWLEVEL, DistanceED ) %>% 
    mutate( UNITID = '198668',
            SURVSECT = 'COM',
            PART = 'B' ) %>%
    select_( .dots=names(com_b_cols) )

ipeds_com_b <- bind_rows( ipeds_com_b_base, ipeds_com_b_missing ) %>%
    arrange( CIPCODE, AWLEVEL )

ipeds_com_c <- acad_credentials %>%
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
            SURVSECT = 'COM',
            PART = 'C' ) %>%
    select_( .dots=names(com_c_cols) )

ipeds_com_d_race <- acad_credentials %>%
    select( Student_ID, IPEDS_Race_Code, CTLEVEL = AWLEVEL ) %>%
    mutate( CTLEVEL = substring(CTLEVEL,2,2) ) %>%
    distinct() %>%
    group_by( CTLEVEL, IPEDS_Race_Code ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    spread( IPEDS_Race_Code, Students, fill="000000" ) %>%
    ungroup() %>%
    add_column( !!!race_cols[!names(race_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(race_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'COM',
            PART = 'D' ) #%>%

ipeds_com_d_gender <- acad_credentials %>%
    select( Student_ID, SEX, CTLEVEL = AWLEVEL ) %>%
    mutate( CTLEVEL = substring(CTLEVEL,2,2) ) %>%
    distinct() %>%
    group_by( CTLEVEL, SEX ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    spread( SEX, Students, fill="000000" ) %>%
    ungroup() %>%
    add_column( !!!gender_cols[!names(gender_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(gender_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'COM',
            PART = 'D' ) #%>%

ipeds_com_d_age <- acad_credentials %>%
    select( Student_ID, Age_Group, CTLEVEL = AWLEVEL ) %>%
    mutate( CTLEVEL = substring(CTLEVEL,2,2) ) %>%
    distinct() %>%
    group_by( CTLEVEL, Age_Group ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    spread( Age_Group, Students, fill="000000" ) %>%
    ungroup() %>%
    add_column( !!!age_cols[!names(age_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(age_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'COM',
            PART = 'D' ) #%>%

ipeds_com_d <- ipeds_com_d_race %>%
    left_join( ipeds_com_d_age ) %>%
    left_join( ipeds_com_d_gender ) %>%
    mutate( Filler_1 = "      ",
            Filler_2 = "      " ) %>%
    select_( .dots=names(com_d_cols) )

## 
## Write out the Graduation Rate table for importing into IPEDS
if (WRITE_OUTPUT) {
    write.table( data.frame(ipeds_com_a), file.path(output_path, fn_COM), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
    write.table( data.frame(ipeds_com_b), file.path(output_path, fn_COM), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_com_c), file.path(output_path, fn_COM), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_com_d), file.path(output_path, fn_COM), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )

    #write.table( data.frame(ipeds_com_a), file.path(output_path, fn_COM_A), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
}
