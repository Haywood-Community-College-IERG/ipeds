ir_root <- "L:/IERG"

#OVERRIDE_REPORT_YEAR <- 2017 # Set to NA to use current year
OVERRIDE_REPORT_YEAR <- NA # Set to NA to use current year

project_path <- file.path(ir_root,"Reporting","IPEDS","R")
input_path <- file.path(project_path, "input")
nsc_path <- file.path(project_path, "nsc")
output_path <- file.path(project_path, "output")

library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(stringr) # string manipulation
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

ccp_programs <- data.frame( Program = c( 'C15200CP', 'C150200CP', 
                                         'C2506CP',  'C250200TP', 
                                         'C25100CP', 'C25120BE', 'C25310CP', 'C25340CP', 'C25550CB', 'C25590CB',
                                         'C35130CP', 'C35140CP', 'C35140CB',
                                         'C40200CP', 'C40200CD',
                                         'C50210CP', 'C50210AD', 'C50420CP',
                                         'C55180CP', 'C55180II', 'C55180IV', 'C55220CP', 'C55400CP', 'C55860CP', 'C55960CP',
                                         'C60130IN', 'C60130WE', 'C60130SP', 'C60160CP', 'C60160ES', 'C60160IN', 'C60160IM', 
                                         'P1012A', 'P1012B', 'P1012C', 'P1042A', 'P1042B', 'P1042C' ), stringsAsFactors = FALSE )

nsc_detail_pat <- str_c(".*_DETLRPT_PA_.*", report_year, ".*csv")
nsc_folder <- "nsc"

fn_ADM <- str_c("ipeds_",report_year,"_adm.txt")

##
## Read in National Student Clearinghouse records for Prior Attendance
##
nsc_config <- data.frame( 
    schoolCode=cfg$school$fice,
    branchCode=cfg$school$branch,
    schoolName=cfg$school$name
)

nsc_pa_data_all <- list.files( path = file.path(project_path, nsc_folder),
                               pattern = nsc_detail_pat, 
                               full.names = TRUE, 
                               ignore.case = TRUE ) %>% 
    lapply(function(x) readr::read_csv(x, col_types = cols(.default = col_character()))) %>% 
    bind_rows %>%
    separate( col=`Requester Return Field`, into=c("ID","Term_ID","Program"), sep="\\." ) 

nsc_pa_data_grads <- nsc_pa_data_all %>%
    filter( !is.na(`Graduation Date`) ) %>%
    select( ID ) %>%
    distinct() %>%
    mutate( Graduated = 1 )

nsc_pa_data <- nsc_pa_data_all %>%
    filter( is.na(`Graduation Date`) ) %>%
    mutate( `Enrollment Begin` = as.Date(`Enrollment Begin`, "%Y%m%d" ) )

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
    mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) )
    

applications <- getColleagueData( "APPLICATIONS" ) %>%
    select( ID = APPL.APPLICANT,
            Term_ID = APPL.START.TERM, 
            Program = APPL.ACAD.PROGRAM,
            Status = APPL.STATUS ) %>%
    filter( Term_ID == report_term_id ) %>%
    collect() %>%
    anti_join( ccp_programs %>% mutate( CCP = TRUE ) ) %>%
    left_join( person ) %>%
    mutate( Admit_Status = case_when(
                Status %in% c("AD","PR") ~ "Admitted",
                Status %in% c("AP","WD") ~ "Applied",
                TRUE ~ "Unknown" ),
            Degree_Seeker = case_when(
                Program %in% c('AHS','GED','=GED') ~ FALSE,
                substring(Program,1,1) %in% c('A','D','C') ~ TRUE,
                TRUE ~ FALSE
            )) %>%
    filter( FirstName != "**DO NOT USE**",
            Degree_Seeker )


person <- getColleagueData( "PERSON" ) %>%
    filter( FIRST.NAME != "" ) %>%
    select( ID, 
            Gender = GENDER ) %>%
    collect() %>%

    inner_join( applications %>% select(ID) )
    

institutions_attend <- getColleagueData( "INSTITUTIONS_ATTEND" ) %>%
    
    # Keep HS graduation records
    filter( INSTA.INST.TYPE == "HS",
            INSTA.GRAD.TYPE == "Y" ) %>%
    select( ID=INSTA.PERSON.ID,
            Institution_Name=X.INSTA.INSTITUTION,
            End_Dates=INSTA.END.DATES
    ) %>%
    collect() %>%
    
    inner_join( applications %>% select(ID) ) %>%
    
    # For some reason, there are some records with multiple dates, keep the latest one
    mutate( End_Date = strsplit(End_Dates,", ") ) %>%
    unnest( End_Date ) %>%
    select( -End_Dates ) %>%
    filter( !is.na(End_Date) ) %>%
    group_by( ID ) %>% 
    summarize( HS_Grad_Date = as.Date(max(End_Date),"%Y%m%d") ) %>%
    ungroup() %>%
    distinct()

# Now merge institutions_attend and nsc_pa_data to identify which prior attendance were 
# after high school

nsc_pa_data_ia <- nsc_pa_data %>%
    
    inner_join( applications %>% select(ID) ) %>%
    
    select( ID, Term_ID, `Enrollment Begin`, `Record Found Y/N`, `College Code/Branch` ) %>%
    mutate( At_HCC = coalesce(if_else(`College Code/Branch` == '008083-00', 1, 0), 0),
            At_Oth = coalesce(if_else(`College Code/Branch` == '008083-00', 0, 1), 0) ) %>%
    left_join( institutions_attend ) %>%
    mutate( After_HS = case_when(
        is.na(`Enrollment Begin`) ~ 0,
        `Enrollment Begin` < HS_Grad_Date ~ 0,
        TRUE ~ 1 ) ) %>%
    mutate( At_HCC = After_HS*At_HCC,
            At_Oth = After_HS*At_Oth ) %>%
    group_by( ID, Term_ID ) %>%
    summarise( Attended_HCC = max(At_HCC),
               Attended_Oth = max(At_Oth),
               After_HS_max = max(After_HS) ) %>%
    left_join( nsc_pa_data_grads ) %>%
    mutate( Graduated = coalesce(Graduated,0) )

# Pull as of October 15 of report year
report_cutoff_date <- as.Date(str_c(report_year,'-10-15'))

student_acad_cred <- getColleagueData( "STUDENT_ACAD_CRED", version="previous" ) %>%
    filter( STC.ACAD.LEVEL == "CU",
            STC.CRED > 0,
            EffectiveDatetime < report_cutoff_date ) %>%
    select( ID = STC.PERSON.ID,
            Term_ID = STC.TERM,
            Course_ID = STUDENT.ACAD.CRED.ID,
            Course_Status = STC.STATUS,
            Credit = STC.CRED,
            EffectiveDatetime ) %>%
    collect() %>%
    inner_join( applications %>% select(ID) )

sac_max_effdt <- student_acad_cred %>%
    group_by( ID, Term_ID, Course_ID ) %>%
    summarise( EffectiveDatetime = max(EffectiveDatetime) )

##
## TODO: Need to check for students who are taking ALL developmental courses. 
##       They should not be in that cohort.
##
sac_most_recent <- student_acad_cred %>%
    inner_join( sac_max_effdt ) %>%
    filter( Course_Status %in% c("A", "N") ) %>%
    select( -c(Course_ID, EffectiveDatetime) ) %>%
    inner_join( terms %>% select( Term_ID, Term_Reporting_Year, Semester ) ) %>%
    group_by( ID, Term_ID, Term_Reporting_Year, Semester ) %>%
    summarise( Credits = sum(Credit) ) %>%
    mutate( Status = if_else(Credits >= 12, "FT", "PT") ) %>%
    ungroup()

sac_report_term <- sac_most_recent %>%
    filter( Term_ID == report_term_id ) 

sac_report_term_ids <- sac_report_term %>% select(ID)

sac_previous_terms <- sac_most_recent %>%
    inner_join( sac_report_term_ids ) %>%
    filter( Term_Reporting_Year < report_year ) %>%
    left_join( terms %>% select( Term_ID, Term_Start_Date ) ) %>%
    left_join( institutions_attend ) %>%
    mutate( After_HS = case_when(
        Term_Start_Date < HS_Grad_Date ~ 0,
        TRUE ~ 1 ) ) %>%
    filter( After_HS == 1 ) %>%
    select( ID ) %>%
    distinct() %>%
    mutate( Previous_HCC = 1 ) 

sac_all <- sac_report_term %>%
    left_join( sac_previous_terms ) %>%
    mutate( Previous_HCC = coalesce(Previous_HCC,0) ) %>%
    left_join( person ) %>%
    left_join( nsc_pa_data_ia ) %>%
    mutate( After_HS_max = coalesce(After_HS_max,0),
            Attended_HCC = coalesce(Attended_HCC,0),
            Attended_Oth = coalesce(Attended_Oth,0),
            Graduated = coalesce(Graduated,0) ) %>%
    mutate( Cohort = case_when(
        Graduated > 0 ~ str_c(Term_Reporting_Year,'R',substring(Status,1,1)),
        Previous_HCC > 0 ~ str_c(Term_Reporting_Year,'R',substring(Status,1,1)),
        After_HS_max == 0 ~ str_c(Term_Reporting_Year,Status),
        TRUE ~ str_c(Term_Reporting_Year,"T",substring(Status,1,1))
    )) %>%
    filter( Cohort == str_c(Term_Reporting_Year,"FT") )

sac_cohort <- sac_all %>% select(ID,Cohort)

