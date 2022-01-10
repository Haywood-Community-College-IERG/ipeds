ir_root <- "L:/IERG"

#OVERRIDE_REPORT_YEAR <- 2017 # Set to NA to use current year
OVERRIDE_REPORT_YEAR <- NA # Set to NA to use current year

project_path <- file.path(ir_root,"Reporting","IPEDS","R")
input_path <- file.path(project_path, "input")
nsc_path <- file.path(project_path, "nsc")
output_path <- file.path(project_path, "output")

package_date <- "2018-07-01" # date of the CRAN snapshot that the checkpoint
                             # package uses

# if checkpoint is not yet installed, install it (for people using this
# system for the first time)
if (!require(checkpoint)) {
  install.packages("checkpoint")
  require(checkpoint)
}

# install packages for the specified CRAN snapshot date
checkpoint(snapshotDate = package_date,
           checkpointLocation = ir_root,
           project = project_path,
           verbose = T,
           scanForPackages = T,
           use.knitr = F)

library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
#library(stringr) # string manipulation
library(dbplyr)
library(odbc)
library(yaml)

# Enter local packages needed for this particular script 
library(haywoodcc)

sessionInfo()

# Now, load the campus configuration
cfg <- yaml.load_file(file.path(ir_root, "Data/config.yml"))
scripts_path <- cfg$R$scripts_path
if (str_sub(scripts_path, -1) == "/") {
    scripts_path = str_sub(scripts_path, 1, -2 )
}

# if you want to outsource logic to other script files, see README for 
# further information
source(file.path(scripts_path, "informer.R"))

# Define some functions that are used frequently
"%nin%" <- function(x, y) !(x %in% y) 

"%||%" <- function(a, b) if (!is.null(a)) a else b # From scales/date-time.r

coalesce_blanks <- function( var, default = '' ) {
    return( if_else( is.na(var) | var == '', default, var ) )
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

report_year <- if_else(!is.na(OVERRIDE_REPORT_YEAR),OVERRIDE_REPORT_YEAR,report_year)

report_term_id <- str_c(report_year,"FA")

output_fn_enrolled <- str_c("apps_enrolled_",report_year,".csv")
output_fn_nonenrolled <- str_c("apps_nonenrolled_",report_year,".csv")

## Load Data
nsc_config <- data.frame( 
    schoolCode=cfg$school$fice,
    branchCode=cfg$school$branch,
    schoolName=cfg$school$name
)


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
    mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) ) # %>%
    #filter( Term_Reporting_Year == report_year - 1,
    #        Semester %in% include_semesters )

final_report_terms <- terms %>%
    filter( Term_ID == report_term_id )


person <- getColleagueData( "PERSON" ) %>%
    filter( FIRST.NAME != "" ) %>%
    select( ID,
            FirstName = FIRST.NAME,
            MiddleInitial = MIDDLE.NAME,
            LastName = LAST.NAME,
            Suffix = SUFFIX,
            DOB = BIRTH.DATE ) %>%
    collect() %>%
    mutate( Suffix = str_replace( Suffix, "\\.", "" ) )

students <- getColleagueData( "STUDENTS" ) %>%
    select( ID = STUDENTS.ID,
            SSN = STU.SSN ) %>%
    collect()
    

applications <- getColleagueData( "APPLICATIONS" ) %>%
    select( ID = APPL.APPLICANT,
            Term_ID = APPL.START.TERM, 
            Program = APPL.ACAD.PROGRAM ) %>%
    filter( Term_ID == report_term_id ) %>%
    collect() %>%
    left_join( person ) %>%
    filter( FirstName != "**DO NOT USE**" )
    
student_acad_cred <- getColleagueData( "STUDENT_ACAD_CRED" ) %>%
    filter( STC.STATUS %in% c("A", "N", "W"), STC.TERM == report_term_id ) %>%
    select( ID = STC.PERSON.ID,
            Term_ID = STC.TERM ) %>%
    distinct() %>%
    collect()


##
## Create two data frames for exporting to NSC
##
apps_nonenrolled <- applications %>%
    anti_join( student_acad_cred ) %>%
    left_join( students ) %>%
    mutate( SSN = if_else(!is.na(SSN),str_replace_all(SSN, "-", ""),"") ) %>%
    mutate( ReturnRequestField = str_c(ID,Term_ID,Program, sep=".") ) %>%
    select( -Program )

apps_enrolled <- applications %>%
    inner_join( student_acad_cred ) %>%
    mutate( ReturnRequestField = str_c(ID,Term_ID,Program, sep=".") ) %>%
    select( -Program )

## Export data

Search_Date <- final_report_terms %>% 
    select( Term_Start_Date ) %>% 
    distinct %>% 
    mutate( Term_Start_Date = Term_Start_Date + days(-1) ) %>%
    pull(Term_Start_Date) %>% 
    as.character("%Y%m%d")

nscout <- nsc_request( apps_enrolled, 
                       nsc_config, 
                       inquiryType = "PA", 
                       path=output_path,
                       search=Search_Date,
                       fn=output_fn_enrolled,
                       enrolledStudents = TRUE )

nscout <- nsc_request( apps_nonenrolled, 
                       nsc_config, 
                       inquiryType = "PA", 
                       path=output_path,
                       search=Search_Date,
                       fn=output_fn_nonenrolled,
                       enrolledStudents = FALSE )
