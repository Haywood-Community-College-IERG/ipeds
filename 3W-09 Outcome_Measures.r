###
### Define GLOBAL USER variables
###

#OVERRIDE_REPORT_YEAR <- 2010 # Set to NA for current year
WRITE_OUTPUT <- TRUE

ir_root <- "L:/IERG"

nsc_path <- file.path(ir_root, "Data", "NSC")
ipeds_path <- file.path(ir_root, "Data", "IPEDS")

cohort_from_file_year <- 2015

###
### End GLOBAL USER variables
###

package_date <- "2020-01-01" # date of the CRAN snapshot that the checkpoint package uses

require(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
require(magrittr) # pipes
require(stringr) # string manipulation
require(dbplyr)
require(odbc)
require(yaml)

# Enter local packages needed for this particular script 
if (!require(haywoodcc)) {
    devtools::install_git("https://github.com/haywood-ierg/haywoodcc.git", git="external")
    require(haywoodcc)
}

sessionInfo()

# Now, load the campus configuration
cfg <- yaml.load_file(file.path(ir_root, "Data/config.yml"))

### 
### Define common report variables
###
if (!exists("OVERRIDE_REPORT_YEAR")) {
    OVERRIDE_REPORT_YEAR <- NA_integer_
}

if (!exists("WRITE_OUTPUT")) {
    WRITE_OUTPUT <- NA_integer_
}

report_year_data_adjustment <- 8

current_year <- as.numeric(format(Sys.time(), "%Y"))
current_month <- as.numeric(format(Sys.time(), "%m"))

report_time_str <- format(Sys.time(),"%Y%m%d_%H%M")

# Fix report year to be the year of the Fall term
report_year <- as.integer(current_year- ifelse(current_month < 7, 1, 0))
report_year_folder <- str_c(report_year,as.integer(substring(report_year,3,4))+1,sep='-')

# Fix report year to be the correct year for the data (or the OVERRIDE value)
report_year <- ifelse( !is.na(OVERRIDE_REPORT_YEAR),
                       OVERRIDE_REPORT_YEAR,
                       report_year - report_year_data_adjustment )

# We get one additional year if we need change from last year
report_year_data_start <- report_year
report_year_data_end <- report_year

#project_path <- file.path(ir_root,"Reporting","IPEDS",report_year_folder,"R")
project_path <- "."
#input_path <- file.path(project_path, "input")
output_path <- file.path(project_path, "output")

fn_OM1 <- stringr::str_c("ipeds_",report_year + report_year_data_adjustment,"_om1_",report_time_str,".txt")


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
    mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) - 1 )

reporting_terms <- terms %>%
    filter( Term_Reporting_Year == report_year )

credential_terms <- terms %>%
    filter( Term_Reporting_Year >= report_year,
            Term_Reporting_Year <= report_year + 7 )

###
### Get Cohort data from Colleague if year is before 2018, otherwise get it from ipeds_cohorts.csv
###
# if (report_year < cohort_from_file_year) {
#     student_acad_levels <- getColleagueData( "STUDENT_ACAD_LEVELS" ) %>%
#         #filter( STA.FED.COHORT.GROUP == report_cohort ) %>%
#         select( ID=STA.STUDENT, Cohort=STA.FED.COHORT.GROUP ) %>%
#         collect() %>%
#         mutate( Term_ID = str_c(substring(Cohort,1,4),"FA") ) %>%
#         inner_join( terms %>% 
#                         select(Term_ID,
#                                Cohort_Year=Term_Reporting_Year,
#                                Cohort_Start_Date=Term_Start_Date) )
# } else {
    ipeds_cohorts <- getColleagueData( "ipeds_cohorts", schema = "local", version = "history" ) %>%
        select( ID, Term_ID, Cohort=OM_Cohort ) %>%
        collect() %>%
        inner_join( terms %>% 
                        select(Term_ID,
                               Cohort_Year=Term_Reporting_Year,
                               Cohort_Start_Date=Term_Start_Date) ) %>%
        filter( !is.na(Cohort),
                Cohort_Year == report_year )
#}

# ipeds_cohorts <- read_csv(file.path(ipeds_path,"ipeds_cohorts.csv")) %>%
#     select( ID, Term_ID, Cohort=OM_Cohort ) %>%
#     inner_join( terms %>% 
#                     select(Term_ID,
#                            Cohort_Year=Term_Reporting_Year,
#                            Cohort_Start_Date=Term_Start_Date) ) %>%
#     filter( !is.na(Cohort),
#             Cohort_Year == report_year )

ipeds_cohorts_ids <- ipeds_cohorts %>%
    select(ID) %>%
    distinct()

#
# Get Person data from DB for all report students
#
person <- getColleagueData( "PERSON" ) %>%
    filter( FIRST.NAME != "" ) %>%
    select( ID,
            FirstName = FIRST.NAME,
            MiddleInitial = MIDDLE.NAME,
            LastName = LAST.NAME,
            Suffix = SUFFIX,
            DOB = BIRTH.DATE ) %>%
    collect() %>%
    mutate( Suffix = str_replace( Suffix, "\\.", "" ) ) %>%
    inner_join( ipeds_cohorts_ids )

#
# Pull students who were credential-seekers enrolled through census during the AY
#    per Outcome Measures instructions
#
student_acad_cred <- getColleagueData( "STUDENT_ACAD_CRED" ) %>%
    filter( STC.ACAD.LEVEL == "CU", 
            STC.CRED > 0,
            STC.STATUS %in% c('A','N','W') ) %>%
    select( ID = STC.PERSON.ID,
            Term_ID = STC.TERM,
            Course_ID = STUDENT.ACAD.CRED.ID,
            Course_Status = STC.STATUS,
            Credit = STC.CRED,
            Course_Level = STC.COURSE.LEVEL,
            Grade_Code = STC.VERIFIED.GRADE ) %>%
    collect() %>%
    inner_join( ipeds_cohorts %>% select(ID,Cohort_Start_Date) ) %>%
    left_join( terms %>% select(Term_ID,Term_Start_Date) ) %>%
    filter( Term_Start_Date >= Cohort_Start_Date )


#
# Get list of students who are taking at least 1 non-developmental/audited course
#
sac_non_dev_ids <- student_acad_cred %>%
    filter( Course_Level != 'DEV' ) %>%
    filter( Grade_Code != "9" ) %>%
    select( ID, Term_ID ) %>%
    distinct()

#
# Now create a summary table to calculate load by term
#
sac_load_by_term <- student_acad_cred %>%
    inner_join( sac_non_dev_ids ) %>%
    inner_join( terms %>% select( Term_ID, Term_Reporting_Year, Semester ) ) %>%
    group_by( ID, Term_ID, Term_Reporting_Year, Semester ) %>%
    summarise( Credits = sum(Credit) ) %>%
    mutate( Status = if_else(Credits >= 12, "FT", "PT") ) %>%
    ungroup()

#
# Now get graduation information with dates
#
acad_credentials <- getColleagueData( "ACAD_CREDENTIALS" ) %>%
    filter( ACAD.INSTITUTIONS.ID == "0019844",
            coalesce(ACAD.TERM,'')!='',
            !(ACAD.ACAD.PROGRAM %in% c("AHS", "=GED", "HSEGED")) ) %>%
    select( ID = ACAD.PERSON.ID,
            Term_ID = ACAD.TERM, 
            Program = ACAD.ACAD.PROGRAM,
            End_Date = ACAD.END.DATE,
            Cast_Date = ACAD.CAST.DATE
    ) %>%
    collect() %>%
    
    # Reduce to the final list, minus the first-time summer students who took classes in the fall.
    inner_join( ipeds_cohorts_ids ) %>%
    
    # For some reason, some of the Term_IDs are messed up, so map the CE terms to CU terms.
    mutate( Term_ID = case_when(
                substring(Term_ID,5,7) == "CE1" ~ str_c(substring(Term_ID,1,4),"SP"),
                substring(Term_ID,5,7) == "CE2" ~ str_c(substring(Term_ID,1,4),"SU"),
                substring(Term_ID,5,7) == "CE3" ~ str_c(substring(Term_ID,1,4),"FA"),
                TRUE ~ Term_ID
            ),
            Credential_Level = case_when(
                substring(Program,1,1) == "A" ~ 10000,
                substring(Program,1,1) %in% c("D","C") ~ 1,
                TRUE ~ 0
            ) ) %>%
    
    # Only keep graduation records for this reporting year and following years.
    inner_join( terms %>% filter( Term_Reporting_Year >= report_year) %>% select(Term_ID, Term_End_Date ) ) %>%
    
    # We are using Cast_Date for the day of graduation
    mutate( Cast_Date = as.Date(Cast_Date) ) %>%
    mutate( Cast_Date = coalesce(Cast_Date, End_Date) ) %>%

        # Fix cast date so that it is not past the end of the term
    mutate( Graduation_Date = if_else(Cast_Date < Term_End_Date, Term_End_Date, Cast_Date),
            Graduated = 1 ) %>%
    
    select( ID, Graduation_Date, Credential_Level, Graduated )

#
# Need to get PELL information
#

# Get terms into a character vector for the SQL query
fa_terms <- terms %>%
    filter( Term_Reporting_Year == report_year ) %>% 
    collect %$% 
    Term_ID

#
# For old data (pre-2015) need to get PELL information from FA_TRANSMITTALS.
#
if (report_year < 2015) {
    federal_award_types <- c("FPELL") # As listed in FA_TRANSMITTALS
    
    pell_aid <- getColleagueData( "FA_TRANSMITTALS" ) %>%
        filter( FAX.TA.TERM %in% fa_terms, FAX.TA.AWARD %in% federal_award_types ) %>%
        select( ID=FAX.TA.STUDENT,
                NAME=FAX.STU.SORT.NAME,
                TERM=FAX.TA.TERM,
                DISTRIBUTED.AMOUNT=FAX.TA.TERM.AMT ) %>%
        collect() %>%
        inner_join( ipeds_cohorts_ids ) %>%
        filter( DISTRIBUTED.AMOUNT > 0.00 ) %>%
        select( ID ) %>%
        distinct() %>%
        mutate( Pell_Recipient = 1 )
    
} else {
    federal_award_types <- c("PELL") # As listed in TA.AWARD.CATEGORY
    
    pell_aid <- getColleagueData( "TA_ACYR" ) %>%
        filter( TA.TERM %in% fa_terms, TA.AWARD.CATEGORY %in% federal_award_types ) %>%
        select( ID=TA.STUDENT.ID, 
                FIRST.NAME=TA.FIRST,
                MIDDLE.NAME=TA.MIDDLE, 
                LAST.NAME=TA.LAST,
                TERM=TA.TERM, 
                AWARD.TYPE=TA.AWARD.CATEGORY, 
                AWARD.AMOUNT=TA.TERM.AMOUNT, 
                DISTRIBUTED.AMOUNT=TA.TERM.XMIT.AMT, 
                DISTRIBUTED.DATE=TA.TERM.XMIT.DT ) %>%
        collect() %>%
        mutate(ID=str_pad(ID,width=7,side="left",pad="0")) %>%
        inner_join( ipeds_cohorts_ids ) %>%
        filter( DISTRIBUTED.AMOUNT > 0.00 ) %>%
        select( ID ) %>%
        distinct() %>%
        mutate( Pell_Recipient = 1 )
}

#
# Build the final data frame for reporting
#
sac_cohort <- ipeds_cohorts %>% 
    select( ID, Cohort ) %>%
    left_join( pell_aid ) %>%
    left_join( acad_credentials ) %>%
    
    # Make sure all NAs are 0s
    mutate( Pell_Recipient = coalesce(Pell_Recipient,0),
            Credential_Level = coalesce(Credential_Level,0),
            Graduation_Date = coalesce(Graduation_Date,as.Date('2999-12-31')),
            Graduated = coalesce(Graduated,0),
            Cohort = substring(Cohort,5,6) ) %>%
    
    # Recode for IPEDS report
    mutate( Cohort = case_when(
                Cohort == "FT" ~ "1",
                Cohort == "PT" ~ "2",
                Cohort == "TF" ~ "3",
                Cohort == "TP" ~ "4",
                TRUE ~ "X"
            ),
            Pell_Recipient = case_when(
                Pell_Recipient == 1 ~ "1",
                TRUE ~ "2"
            )) %>%
    rename( LINE = Cohort,
            RECIPIENT_TYPE = Pell_Recipient ) %>%
    distinct()

#
# This is needed for Part D
#
not_graduated <- sac_cohort %>%
    filter( Graduation_Date > as.Date(str_c(report_year+8,'-08-31')) ) %>%
    select( ID, LINE, RECIPIENT_TYPE ) %>%
    distinct()

#
# This is needed for Part D
#
still_enrolled <- sac_load_by_term %>%
    filter( Term_Reporting_Year == report_year + 8,
            Semester == "FA" ) %>%
    select( ID ) %>%
    distinct() %>%
    inner_join( not_graduated ) 

still_enrolled_summary <- still_enrolled %>%
    group_by( LINE, RECIPIENT_TYPE ) %>%
    summarise( STILL_ENROLLED = sprintf("%06d", n()) )

#
# This is needed for Part D
#
sac_last_enrollment <- not_graduated %>%
    
    # Get terms prior to the EIGHT year report date for credential-seekers
    left_join( sac_load_by_term %>% 
                   inner_join( not_graduated %>% select(ID) ) %>%
                   left_join( terms %>% select(Term_ID, Term_Start_Date) ) %>%
                   filter( Term_Start_Date <= as.Date(str_c(report_year+8,'-08-31')) ) ) %>%
    
    # By ID, find last term enrolled
    group_by( ID ) %>%
    summarise( Term_Start_Date = max(Term_Start_Date) ) %>%
    
    # Need Term_End_Date for subsequent enrollment next
    left_join( terms %>% select(Term_Start_Date, Term_End_Date) )

#
# This is needed for Part D
#
nsc_detail_pat_se <- str_c("(", report_year, "fa|", report_year+1, "sp|", report_year+1, "su)",
                           "_DETLRPT_SE_.*csv")
transferred <- list.files( path = nsc_path,
                           pattern = nsc_detail_pat_se, 
                           full.names = TRUE, 
                           ignore.case = TRUE ) %>% 
    lapply(function(x) readr::read_csv(x, col_types = cols(.default = col_character()))) %>% 
    bind_rows %>%
    separate( col=`Requester Return Field`, into=c("ID","Term_ID"), sep="\\." ) %>%
    
    # Exclude any enrollment records at HCC
    filter( `College Code/Branch` != "008083-00") %>%
    ungroup() %>%
    
    # Reduce data to our cohort
    inner_join( ipeds_cohorts_ids ) %>%
    
    # Combine the Enrollment and Graduation since all we need is the actual date
    mutate( Transfer_Date = as.Date(if_else(is.na(`Enrollment Begin`), `Graduation Date`, `Enrollment Begin`), "%Y%m%d") ) %>%

    # Select and rename columns
    select( ID, Transfer_Date ) %>%
    
    # Bring in last enrollment date for these students who have not yet graduated
    inner_join( sac_last_enrollment ) %>%
    
    # Remove the students who are still enrolled here at HCC
    anti_join( sac_load_by_term %>%
                   filter( Term_Reporting_Year == report_year + 8,
                           Semester == "FA" ) %>%
                   select( ID ) %>%
                   distinct() %>%
                   inner_join( not_graduated ) ) %>%

    # Anyone with a Transfer date after their last term is a transfer student
    mutate( Transfer = (Transfer_Date >= Term_End_Date) * 1 ) %>%
    group_by( ID ) %>%
    summarise( Transferred = max(Transfer) ) %>%
    filter( Transferred == 1 ) %>%
    select( -Transferred ) %>%
    inner_join( not_graduated ) 
    
transferred_summary <- transferred %>%
    group_by( LINE, RECIPIENT_TYPE ) %>%
    summarise( ENROLLED_ANOTHER = sprintf("%06d", n()) )

#
# Part A is the cohort
#
ipeds_om1_a <- sac_cohort %>%
    select( ID, LINE, RECIPIENT_TYPE ) %>%
    distinct() %>%
    group_by( LINE, RECIPIENT_TYPE ) %>%
    summarise( COHORT = sprintf("%06d", n()) ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'OM1',
            PART = 'A',
            EXCLUSION = "000000" ) %>%
    select( UNITID, SURVSECT, PART, LINE, RECIPIENT_TYPE, COHORT, EXCLUSION )

#
# Part B - FOUR years
#
ipeds_om1_b <- sac_cohort %>%
    filter( Graduation_Date <= as.Date(str_c(report_year+4,'-08-31')) ) %>%
    group_by( ID, LINE, RECIPIENT_TYPE ) %>%
    summarise( Credential_Level = max(Credential_Level) ) %>%
    mutate( Associate = if_else(Credential_Level==10000,1,0),
            Certificate = if_else(Credential_Level==1,1,0) ) %>%
    group_by( LINE, RECIPIENT_TYPE ) %>%
    summarise( AWARD_ASSOCIATES = sprintf("%06d",sum(Associate)),
               AWARD_CERTIFICATE = sprintf("%06d",sum(Certificate)) ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'OM1',
            PART = 'B',
            AWARD_BACHELORS = "000000" ) %>%
    select( UNITID, SURVSECT, PART, LINE, RECIPIENT_TYPE, AWARD_CERTIFICATE, AWARD_ASSOCIATES, AWARD_BACHELORS )
    
#
# Part C - SIX years
#
ipeds_om1_c <- sac_cohort %>%
    filter( Graduation_Date <= as.Date(str_c(report_year+6,'-08-31')) ) %>%
    group_by( ID, LINE, RECIPIENT_TYPE ) %>%
    summarise( Credential_Level = max(Credential_Level) ) %>%
    mutate( Associate = if_else(Credential_Level==10000,1,0),
            Certificate = if_else(Credential_Level==1,1,0) ) %>%
    group_by( LINE, RECIPIENT_TYPE ) %>%
    summarise( AWARD_ASSOCIATES = sprintf("%06d",sum(Associate)),
               AWARD_CERTIFICATE = sprintf("%06d",sum(Certificate)) ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'OM1',
            PART = 'C',
            AWARD_BACHELORS = "000000" ) %>%
    select( UNITID, SURVSECT, PART, LINE, RECIPIENT_TYPE, AWARD_CERTIFICATE, AWARD_ASSOCIATES, AWARD_BACHELORS )


#
# Part D - EIGHT years
#
ipeds_om1_d <- sac_cohort %>%
    filter( Graduation_Date <= as.Date(str_c(report_year+8,'-08-31')) ) %>%
    group_by( ID, LINE, RECIPIENT_TYPE ) %>%
    summarise( Credential_Level = max(Credential_Level) ) %>%
    mutate( Associate = if_else(Credential_Level==10000,1,0),
            Certificate = if_else(Credential_Level==1,1,0) ) %>%
    group_by( LINE, RECIPIENT_TYPE ) %>%
    summarise( AWARD_ASSOCIATES = sprintf("%06d",sum(Associate)),
               AWARD_CERTIFICATE = sprintf("%06d",sum(Certificate)) ) %>%
    left_join( still_enrolled_summary ) %>%
    left_join( transferred_summary ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'OM1',
            PART = 'D',
            AWARD_BACHELORS = "000000",
            STILL_ENROLLED = coalesce(as.character(STILL_ENROLLED),"000000"),
            ENROLLED_ANOTHER = coalesce(as.character(ENROLLED_ANOTHER),"000000"),
            Filler1 = "         "
    ) %>%
    select( UNITID, SURVSECT, PART, LINE, RECIPIENT_TYPE, AWARD_CERTIFICATE, AWARD_ASSOCIATES, AWARD_BACHELORS, Filler1, STILL_ENROLLED, ENROLLED_ANOTHER )

if (WRITE_OUTPUT) {
    write.table( data.frame(ipeds_om1_a), file.path(output_path, fn_OM1), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
    write.table( data.frame(ipeds_om1_b), file.path(output_path, fn_OM1), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_om1_c), file.path(output_path, fn_OM1), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_om1_d), file.path(output_path, fn_OM1), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
}
