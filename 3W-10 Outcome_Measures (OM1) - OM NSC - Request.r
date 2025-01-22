# To override the current report year, set report_year here
params <- list( current_datetime="",
                report_year="",
                report_data_term="",
                write_output=TRUE,
                test=FALSE,
                cleanup=FALSE,
                report_code=""
              )

report_year_data_adjustment <- 8

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


output_fn <- str_c( "hcc_ipeds_om1_", report_year, "_se.txt" )

#
# Get all the data from DB
#
terms <- getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
    select( Term_ID,
            Cohort_Start_Date = Term_Start_Date ) %>%
    collect()

ipeds_cohort <- getColleagueData( "ipeds_cohorts", schema = "local", version = "history" ) %>%
    select( ID, Term_ID, Cohort=OM_Cohort ) %>%
    filter( Cohort %in% c(local(glue::glue("{report_data_year}FT")),
                          local(glue::glue("{report_data_year}PT")),
                          local(glue::glue("{report_data_year}TF")),
                          local(glue::glue("{report_data_year}TP"))) ) %>%
    collect() %>%
    left_join( terms, by = "Term_ID" )

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
    arrange( ID ) %>%
    mutate( ReturnRequestField = trimws(format(str_c(ID,Cohort,sep=";"),width=50)) ) %>%
    rename( SearchBeginDate = Cohort_Start_Date )
    
nsc_out <- nsc_request( person, nsc_config, inquiryType = "SE", path = output_path, fn = output_fn )
print(glue::glue("Output file: {output_path}/{output_fn}"))
