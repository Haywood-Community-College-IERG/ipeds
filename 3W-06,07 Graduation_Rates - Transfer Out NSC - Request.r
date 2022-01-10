#OVERRIDE_REPORT_YEAR <- 2018 # Set to NA to use current year

WRITE_OUTPUT <- TRUE

ir_root <- "L:/IERG"

nsc_path <- file.path(ir_root, "Data", "NSC")
ipeds_path <- file.path(ir_root, "Data", "IPEDS")

package_date <- "2020-01-01" # date of the CRAN snapshot that the checkpoint
                             # package uses

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

if (!exists("OVERRIDE_REPORT_YEAR")) {
    OVERRIDE_REPORT_YEAR <- NA_integer_
}

if (!exists("WRITE_OUTPUT")) {
    WRITE_OUTPUT <- NA_integer_
}
###
### Define some local variables
###

nsc_config <- data.frame( 
    schoolCode=cfg$school$fice,
    branchCode=cfg$school$branch,
    schoolName=cfg$school$name
)

report_year <- as.numeric(format(Sys.time(), "%Y"))

# Set report_month to actual month to allow adjustment of report_year based on month
# Otherwise, set to -1
report_month <- as.numeric(format(Sys.time(), "%m"))

# For file names
report_time_str <- format(Sys.time(),"%Y%m%d_%H%M")

# Fix report year to be the year of the Fall term
report_year <- report_year - if_else(report_month < 7, 1, 0)

# Use the current report year to determine the folder for the data
report_year_folder <- str_c(report_year,as.integer(substring(report_year,3,4))+1,sep='-')

# Fix report year to be the Override year if provided
report_year <- ifelse(!is.na(OVERRIDE_REPORT_YEAR),OVERRIDE_REPORT_YEAR,report_year)

# For spring and summer terms, set report year to fall's year
#report_year <- report_year - if_else(report_month < 7, 1, 0) - 3 - if_else(IPEDS_Report==150,0,1)
report_year_150 <- report_year - 3
report_year_200 <- report_year - 4

report_term_150 <- str_c(report_year_150,"FA")
report_term_200 <- str_c(report_year_200,"FA")

report_ftft_150 <- str_c(report_year_150,"FT")
report_ftft_200 <- str_c(report_year_200,"FT")

report_cohorts <- c(report_ftft_150, report_ftft_200)

report_cutoff_date <- as.Date(str_c(report_year,"08-31",sep="-"))

#nsc_detail_pat <- str_c(".*_DETLRPT_SE_.*", report_year, "fa.*csv")
#nsc_detail_pat <- str_c(".*_DETLRPT_SE_.*(", report_year_150, "|", report_year_200, ")fa.*csv")
nsc_detail_pat <- str_c("(", report_year_150, "|", report_year_200, ")fa",
                        "_DETLRPT_SE_.*.csv")
nsc_folder <- "nsc"

USE_FILE_COHORTS_150 <- report_year_150 < 2018
USE_FILE_COHORTS_200 <- report_year_200 < 2018

project_path <- file.path(ir_root,"Reporting","IPEDS",report_year_folder,"R")
project_path <- "."

#input_path <- file.path(project_path, "input")
output_path <- file.path(project_path, "output")

output_fn <- str_c( "hcc_ipeds_g2_", report_year, "_se.txt" )

#
# Get all the data from DB
#
terms <- getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
    select( Term_ID,
            Term_Index,
            Term_Name = Semester,
            Semester = Term_Abbreviation,
            Term_Start_Date,
            Term_Census_Date,
            Term_End_Date,
            Term_Reporting_Year = Reporting_Year,
            Academic_Year ) %>%
    collect() %>%
    mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) - 1 )
    
report_terms <- terms %>%
    filter( Term_Reporting_Year %in% c(report_year_150, report_year_200),
            Semester == "FA" )
report_term_still_enrolled <- terms %>%
    filter( Term_Reporting_Year == report_year,
            Semester == "FA" )

report_term_start_date_150 <- (report_terms %>% collect %$% Term_Start_Date)[2]
report_term_start_date_200 <- (report_terms %>% collect %$% Term_Start_Date)[1]

report_term_end_date_150 <- (report_terms %>% collect %$% Term_End_Date)[2]
report_term_end_date_200 <- (report_terms %>% collect %$% Term_End_Date)[1]

report_term_start_date <- pmin(report_term_start_date_150,report_term_start_date_200)
report_term_end_date <- pmin(report_term_end_date_150,report_term_end_date_200)

#
# Reduce terms df to just the terms between the cohort term and August 31st of the year 3 or 4 years later.
#
terms %<>%
    filter( Term_End_Date >= pmin(report_term_end_date_150,report_term_end_date_200),
            Term_Start_Date <= report_cutoff_date ) %>%
    arrange( Term_Start_Date )    

ipeds_cohort_FILE_COHORTS <- getColleagueData( "ipeds_cohorts", schema="local" ) %>%
    select( ID, Cohort = Term_Cohort, Term_ID ) %>%
    filter( Cohort %in% c(report_cohorts) ) %>%
    distinct() %>%
    collect() %>%
    inner_join( report_terms %>% 
                    filter( ( (Term_Reporting_Year==report_year_150) & USE_FILE_COHORTS_150 ) |
                                ( (Term_Reporting_Year==report_year_200) & USE_FILE_COHORTS_200 ) ) %>%
                    select(Term_ID, Cohort_Start_Date = Term_Start_Date) ) %>%
    select( -c(Term_ID) )

ipeds_cohort_COLLEAGUE_COHORTS <- getColleagueData( "STUDENT_TERMS" ) %>%
    select( ID = STTR.STUDENT, Cohort = STTR.FED.COHORT.GROUP ) %>%
    filter( Cohort %in% c(report_cohorts) ) %>%
    distinct() %>%
    collect() %>%
    mutate( Term_ID = str_c(substring(Cohort,1,4),"FA") ) %>%
    inner_join( report_terms %>% 
                    filter( ( (Term_Reporting_Year==report_year_150) & !USE_FILE_COHORTS_150 ) |
                            ( (Term_Reporting_Year==report_year_200) & !USE_FILE_COHORTS_200 ) ) %>%
                    select(Term_ID, Cohort_Start_Date = Term_Start_Date) ) %>%
    select( -c(Term_ID) )

ipeds_cohort <- ipeds_cohort_FILE_COHORTS %>%
    bind_rows( ipeds_cohort_COLLEAGUE_COHORTS )

person <- person <- getColleagueData( "PERSON" ) %>%
    filter( FIRST.NAME != "" ) %>%
    select( ID,
            FirstName = FIRST.NAME,
            MiddleInitial = MIDDLE.NAME,
            LastName = LAST.NAME,
            Suffix = SUFFIX,
            DOB = BIRTH.DATE ) %>%
    collect() %>%
    mutate( Suffix = if_else(str_detect(LastName,", Jr"), "Jr", "") ) %>%
    mutate( LastName = if_else(str_detect(LastName,", Jr"),
                               str_replace(LastName,", Jr", ""), LastName) ) %>%
    mutate( Suffix = str_replace( Suffix, "\\.", "" ) ) %>%
    mutate( FirstName = str_replace( FirstName, "\\.", "" ) ) %>%
    mutate( LastName = str_replace( LastName, "\\.", "" ) ) %>%
    filter( !str_detect(FirstName, "DO NOT USE"),
            !str_detect(toupper(LastName), "ZZZ") ) %>%
    inner_join( ipeds_cohort ) %>%
    arrange( Cohort_Start_Date, ID ) %>%
    mutate( ReturnRequestField = trimws(format(str_c(ID,Cohort,sep=";"),width=50)) ) %>%
    rename( SearchBeginDate = Cohort_Start_Date )
    
nsc_out <- nsc_request( person, nsc_config, inquiryType = "SE", path = output_path, fn = output_fn )
