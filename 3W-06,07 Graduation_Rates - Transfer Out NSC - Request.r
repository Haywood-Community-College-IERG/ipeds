# To override the current report year, set report_year here
params <- list( current_datetime="",
                report_year="2018",
                report_data_term="",
                write_output=TRUE,
                test=FALSE,
                cleanup=FALSE,
                report_code="GR2",
                report_code2="G22"
              )

report_year_data_adjustment <- 0

source('_packages.r')
source('_setup.r')

library(nscrequest)

sessionInfo()

###
### Define some local variables
###

nsc_config <- data.frame( 
    schoolCode=cfg$school$fice,
    branchCode=cfg$school$branch,
    schoolName=cfg$school$name
)


report_year_150 <- report_year - 3
report_year_200 <- report_year - 4

report_term_150 <- str_c(report_year_150,"FA")
report_term_200 <- str_c(report_year_200,"FA")

report_ftft_150 <- str_c(report_year_150,"FT")
report_ftft_200 <- str_c(report_year_200,"FT")

report_cohorts <- c(report_ftft_150, report_ftft_200)

report_cutoff_date <- as.Date(str_c(report_year,"08-31",sep="-"))

nsc_detail_pat <- str_c("(", report_year_150, "|", report_year_200, ")fa",
                        "_DETLRPT_SE_.*.csv")
nsc_folder <- "nsc"

USE_FILE_COHORTS_150 <- report_year_150 < 2018
USE_FILE_COHORTS_200 <- report_year_200 < 2018

output_fn <- str_glue( "hcc_ipeds_{report_code}_{report_year}_se.txt" )

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
