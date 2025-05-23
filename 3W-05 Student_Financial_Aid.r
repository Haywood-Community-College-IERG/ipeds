#OVERRIDE_REPORT_YEAR <- 2017 # Comment for current academic year's fall term

WRITE_OUTPUT <- TRUE
#CLEANUP <- TRUE
#TEST <- TRUE

ir_root <- "L:/IERG"

nsc_path <- file.path(ir_root, "Data", "NSC")
ipeds_path <- file.path(ir_root, "Data", "IPEDS")

package_date <- "2020-01-01" # date of the CRAN snapshot that the checkpoint package uses

# if checkpoint is not yet installed, install it (for people using this
# system for the first time)
#if (!require(checkpoint)) {
#    install.packages("checkpoint")
#    require(checkpoint)
#}

# install packages for the specified CRAN snapshot date
#checkpoint(snapshotDate = package_date,
#          checkpointLocation = ir_root,
#          verbose = T,
#          scanForPackages = T,
#          use.knitr = F)

require(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
require(magrittr) # pipes
require(stringr) # string manipulation
require(lubridate)
require(dbplyr)
require(odbc)
require(yaml)

# Enter local packages needed for this particular script 
if (!require(haywoodcc)) {
    #library(devtools)
    devtools::install_git("https://github.com/haywood-ierg/haywoodcc.git", git="external")
    require(haywoodcc)
}

sessionInfo()

# Now, load the campus configuration
cfg <- yaml.load_file(file.path(ir_root, "Data/config.yml"))

ipeds_unitid <- as.character(cfg$school$ipeds)

if (!exists("OVERRIDE_REPORT_YEAR")) {
    OVERRIDE_REPORT_YEAR <- NA_integer_
}

if (!exists("WRITE_OUTPUT")) {
    WRITE_OUTPUT <- FALSE
}

if (!exists("TEST")) {
    TEST <- FALSE
}

if (!exists("CLEANUP")) {
    CLEANUP <- TRUE
}

report_year_data_adjustment <- 1

# For file names
report_time_str <- format(Sys.time(),"%Y%m%d_%H%M")

report_year <- as.numeric(format(Sys.time(), "%Y"))

# Set report_month to actual month to allow adjustment of report_year based on month
# Otherwise, set to -1
report_month <- as.numeric(format(Sys.time(), "%m"))

# Fix report year to be the year of the Fall term
report_year <- report_year - if_else(report_month < 7, 1, 0)
report_year_folder <- str_c(report_year,as.integer(substring(report_year,3,4))+1,sep='-')

report_year <- ifelse(!is.na(OVERRIDE_REPORT_YEAR),OVERRIDE_REPORT_YEAR,report_year - report_year_data_adjustment)

# We get one additional year if we need change from last year
report_year_data_start <- report_year
#report_year_data_end <- report_year - (dplyr::if_else( include_current_ay, 0, 1 ))

report_term <- str_c(report_year, "FA")
report_cohort <- str_c(report_year, "FT")

report_year_date <- ymd( str_c(report_year, 10, 15) )

aid_federal_loans <- c("GSL","NDSL","PERK","PLUS","SMALT","USTF")
aid_federal_non_pell <- c("ACG","ACG1","ACG2","IASG","SCH","SEOG") # Need to use Award_Type == 'F' for SCH
aid_federal_pell <- c("PELL")
aid_federal_grant <- unique(c(aid_federal_non_pell,aid_federal_pell))
aid_federal_ws <- c("FWS")
aid_federal_other <- c("CARES") # CARES funds are not Title IV
aid_state_local <- c("NCCG","SCH","SSIG","STATE")  # Need to use Award_Type == 'S' for SCH
aid_institution <- c("SCH")  # Need to use Award_Type == 'I'
aid_other <- c("ALT","SCH","VET") # Need to use Award_Type == 'O'
aid_other_loans <- c("ALT","WFALT")
aid_loans <- unique(c(aid_federal_loans,aid_other_loans))
aid_veterans_gi <- c("CH33")
aid_veterans_dod <- c("NCMIL")
aid_veterans <- unique(c(aid_veterans_gi,aid_veterans_dod))

aid_group_2a <- unique(c(aid_federal_ws,
                         aid_loans,
                         aid_federal_grant,
                         aid_state_local,
                         aid_institution,
                         aid_federal_other,
                         aid_other))
aid_group_2b <- unique(c(aid_loans,
                         aid_federal_grant,
                         aid_state_local,
                         aid_institution,
                         aid_federal_other))
aid_group_3 <- unique(c(aid_federal_grant,
                        aid_state_local,
                        aid_institution))
aid_group_4 <- unique(c(aid_federal_grant,
                        aid_federal_ws,
                        aid_federal_loans))

fn_report_code <- "sfa"

#project_path <- file.path(ir_root,"Reporting","IPEDS","R")
project_path <- "." # file.path(ir_root,"Reporting","IPEDS",report_year_folder,"R")
#input_path <- file.path(project_path, "input")
output_path <- file.path(project_path, "output")

fn_SFA <- str_c("ipeds_",report_year + report_year_data_adjustment,"_",fn_report_code,"_",report_time_str,".txt")


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
            Term_Reporting_Year = Reporting_Year,
            Academic_Year ) %>%
    collect() %>%
    mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) - 1 )

reporting_terms <- terms %>%
    filter( Term_Reporting_Year == report_year,
            Semester == "FA" )

###
### Get Cohort data from ipeds_cohorts.csv. 
### We need Term_Cohort so we need to get from the file and not STUDENT_TERMS so we cannot
###      use the ipeds_cohort() function.
###
#ipeds_cohorts <- read_csv( file.path(ipeds_path,"ipeds_cohorts.csv"), col_types = cols(.default=col_character()) ) %>%
ipeds_cohorts <- getColleagueData( "ipeds_cohorts", schema="local", version="latest" ) %>%
    select( ID, Term_ID, Cohort, Term_Cohort ) %>%
    collect() %>%
    inner_join( terms %>% select(Term_ID,Cohort_Year=Term_Reporting_Year,Cohort_Start_Date=Term_Start_Date) ) %>%
    filter( Cohort_Year %in% c(report_year),
            Term_ID == report_term )

cs_comp <- getColleagueData( "CS_ACYR__CS_COMP", version="history" ) %>%
    filter( CS.YEAR == report_year_data_start,
            CS.COMP.ID %in% c("HOME","AWAY") ) %>%
    select( CS.STUDENT.ID, CS.YEAR, CS.COMP.ID, EffectiveDatetime ) %>%
    collect()

cs_acyr <- getColleagueData( "CS_ACYR" ) %>%
    filter( CS.YEAR == report_year_data_start ) %>%
    select( CS.STUDENT.ID, CS.YEAR, CS.TOTAL.FAMILY.INCOME, EffectiveDatetime ) %>%
    collect() %>%
    left_join( cs_comp ) %>%
    rename( ID=CS.STUDENT.ID,
            Term_Reporting_Year = CS.YEAR,
            Total_Family_Income=CS.TOTAL.FAMILY.INCOME,
            Code = CS.COMP.ID ) %>%
    select( -c(EffectiveDatetime) ) %>%
    mutate( ID = str_pad(ID, 7, "left", pad="0"),
            Term_Reporting_Year = as.integer(Term_Reporting_Year),
            Code = coalesce( Code, "UNKNOWN" ),
            Found = 'Y' ) %>%
    pivot_wider( id_cols = c(ID,Term_Reporting_Year,Total_Family_Income), 
                 names_from = Code,
                 values_from = Found ) %>%
    mutate( Living_Arrangement = if_else( coalesce(HOME,'N') == 'Y', "HOME", "AWAY"),
            Income_Range = case_when(
                Total_Family_Income <=  30000 ~ 1,
                Total_Family_Income <=  48000 ~ 2,
                Total_Family_Income <=  75000 ~ 3,
                Total_Family_Income <= 110000 ~ 4,
                Total_Family_Income >  110000 ~ 5,
                TRUE ~ 99
            )) %>%
    select( ID, Term_Reporting_Year, Total_Family_Income, Income_Range, Living_Arrangement ) %>%
    distinct()

if (CLEANUP) {
    rm(cs_comp)
}

#fafsa <- getColleagueData( "ISIR_FAFSA" ) %>%
#    collect()

#
# Get Person data from DB for all report students
#
person <- getColleagueData( "PERSON", version="history" ) %>%
    filter( FIRST.NAME != "" ) %>%
    select( ID, 
            First_Name = FIRST.NAME, 
            Last_Name = LAST.NAME, 
            State = STATE,
            Residence_State = RESIDENCE.STATE,
            EffectiveDatetime ) %>%
    collect() %>%
    filter( EffectiveDatetime <= as.Date(str_c(report_year,"-10-15")) ) %>%
    mutate( Residence_State = if_else(Residence_State=='', State, Residence_State) ) %>%
    mutate( Tuition_Type = if_else( Residence_State == "NC", "In-State", "Out-of-State" ) ) %>%
    group_by( ID ) %>%
    top_n( 1, EffectiveDatetime ) %>%
    select( -c(State,EffectiveDatetime) ) %>%
    select( ID, First_Name, Last_Name, Residence_State, Tuition_Type )

if (report_year < 2015) {
    federal_award_types <- c("FPELL","NCCCG","NCELS","SEOG","SCH","GSL","USTF","STATE") # As listed in FA_TRANSMITTALS
    
    financial_aid1 <- getColleagueData( "FA_TRANSMITTALS" ) %>%
        filter( FAX.TA.TERM == report_term ) %>%
        select( ID=FAX.TA.STUDENT,
                Term_ID=FAX.TA.TERM,
                Award_Type=FAX.TA.AWARD, 
                Award_Amount=NA_real_,
                Distributed_Amount=FAX.TA.TERM.AMT ) %>%
        collect() %>%
        filter( Distributed_Amount > 0.00 )
    
} else {

    financial_aid_award_list <- getColleagueData( "SA_ACYR__AWARD_LIST", version = "history" ) %>%
        filter( SA.YEAR == report_year ) %>%
        select( ID=SA.STUDENT.ID,
                Term_Reporting_Year=SA.YEAR,
                Award_Amount=SA.AMOUNT,
                Distributed_Amount=SA.XMIT.AMT,
                Award_Code=SA.AWARD,
                Award_Action=SA.ACTION,
                Award_Category=X.SA.AWARD.CATEGORY,
                Award_Type=X.SA.AWARD.TYPE,
                Award_Destination=X.SA.AWARD.DESTINATION,
                EffectiveDatetime ) %>%
        collect()
    
    financial_aid <- getColleagueData( "SA_ACYR" ) %>%
        filter( SA.YEAR == report_year ) %>%
        select( ID=SA.STUDENT.ID, 
                Term_Reporting_Year=SA.YEAR, 
                Award_Amount_Total=SA.AWARDED,
                EffectiveDatetime ) %>%
        collect() %>%
        inner_join( financial_aid_award_list ) %>%
        mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) ) %>%
        filter( Award_Amount > 0.00 ) %>%
        select( -EffectiveDatetime )
    
    if( CLEANUP ) {
        rm( financial_aid_award_list )
    }
}

#
# Group 1 includes all the fall enrolled students as of October 15. 
#

sfa_group1 <- fall_enrollment( report_year ) %>%
    filter( Enrollment_Status != "Withdrawn" ) %>%
    select( ID, Term_ID, Term_Reporting_Year ) %>%
    left_join( person ) %>%
    left_join( ipeds_cohorts ) %>%
    left_join( cs_acyr ) %>%
    left_join( financial_aid ) %>%
    mutate( Living_Arrangement = coalesce(Living_Arrangement,"UNKNOWN"),
            Award_Amount_Total = coalesce(Award_Amount_Total,0),
            Award_Amount = coalesce(Award_Amount,0),
            Distributed_Amount = coalesce(Distributed_Amount,0),
            Award_Code = coalesce(Award_Code,"NONE"),
            Award_Action = coalesce(Award_Action,'-'),
            Award_Category = coalesce(Award_Category,"NONE"),
            Award_Type = coalesce(Award_Type,'-'),
            Award_Destination = coalesce(Award_Destination,'-') ) %>%
    mutate( IPEDS_Award_Category = case_when(
        Award_Category %in% c('SCH') & Award_Type == 'F' ~ "Federal grant",
        Award_Category %in% c('SCH') & Award_Type == 'S' ~ "State grant",
        Award_Category %in% c('SCH') & Award_Type == 'I' ~ "Institutional grant",
        Award_Category %in% c('SCH') & Award_Type == 'O' ~ "Other grant",
        Award_Category %in% c('ALT') & Award_Type == 'F' ~ "Federal loan",
        Award_Category %in% c('ALT') & Award_Type == 'O' ~ "Other loan", # This should be ignored and not reported here.
        Award_Category %in% aid_federal_pell ~ "Federal Pell",
        Award_Category %in% aid_federal_grant ~ "Federal grant",
        Award_Category %in% aid_federal_loans ~ "Federal loan",
        Award_Category %in% aid_federal_other ~ "Federal other",
        Award_Category %in% aid_state_local ~ "State grant",
        Award_Category %in% aid_institution ~ "Institutional grant",
        Award_Category %in% aid_federal_ws ~ "Federal work-study",
        Award_Category %in% aid_other ~ "Other grant",
        Award_Category %in% aid_other_loans ~ "Other loan",
        TRUE ~ "UNDEFINED"
    ),
        IPEDS_Award_Veterans = if_else( Award_Code %in% aid_veterans, "Veterans", "Not Veterans" )
    )

if (CLEANUP) {
    rm( person, ipeds_cohorts, financial_aid )
}

#
# Group 2 are the first-time, full-time degree seeking students from Group 1.
#
sfa_group2 <- sfa_group1 %>%
    filter( Term_Cohort == report_cohort )

sfa_group2_ids <- sfa_group2 %>%
    select(ID) %>%
    distinct()

sfa_group2b_ids <- sfa_group2 %>%
    filter( Award_Category %in% aid_group_2b ) %>%
    select(ID) %>%
    distinct()

# Group 3 students include in-state students awarded grants or scholarships from:
# federal government, state/local government, or the institution.
# 
# DO NOT include students who received aid only from other sources.
sfa_group3 <- sfa_group2 %>%
    filter( Award_Category %in% aid_group_3,
            Award_Type %in% c('F','S','I') ) %>%
    filter( Tuition_Type == "In-State" ) #%>%
    #select( ID ) %>%
    #distinct() %>%
    #left_join( sfa_group2 )

sfa_group3_ids <- sfa_group3 %>%
    select(ID) %>%
    distinct()

# Group 4 students include in-state students awarded Title IV federal student aid:
# Federal Pell, Federal Supplemental Educational Opportunity Grant (FSEOG), 
# Academic Competitiveness Grant (ACG), National Science and Mathematics Access 
# to Retain Talent Grant (National SMART Grant), Teacher Education Assistance for 
# College and Higher Education (TEACH), Federal Work Study, Federal Perkins Loan,
# Subsidized Direct or FFEL Stafford Loan, and Unsubsidized Direct or FFEL Stafford Loan
sfa_group4 <- sfa_group2 %>%
    filter( Award_Category %in% aid_group_4,
            Award_Type == 'F' ) #%>%
    # select( ID ) %>%
    # distinct() %>%
    # left_join( sfa_group2 )

sfa_group1 %<>% 
    left_join( sfa_group2 %>% select(ID) %>% distinct() %>% mutate( In_Group2 = "Yes") ) %>%
    left_join( sfa_group3 %>% select(ID) %>% distinct() %>% mutate( In_Group3 = "Yes") ) %>%
    left_join( sfa_group4 %>% select(ID) %>% distinct() %>% mutate( In_Group4 = "Yes") ) %>%
    mutate_at(.vars=c("In_Group2","In_Group3","In_Group4"), function(x) {return( coalesce(x,"No") )} )
    
#####
#####
#
# Code to generate IPEDS import files
#
####
####

#
# Part A is ALL students
#
saf_a_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
                 SCFA2=NA_character_, SCFY2=NA_character_,
                 UGAIDN=NA_character_, UGAIDPN=NA_character_, UGLNFN=NA_character_,
                 UGAIDA=NA_character_, UGAIDPA=NA_character_, UGLNFA=NA_character_ )
a_cols_a <- c( UGAIDA=NA_character_, UGAIDPA=NA_character_, UGLNFA=NA_character_ )
n_cols_a <- c( SCFA2=NA_character_, UGAIDN=NA_character_, UGAIDPN=NA_character_, UGLNFN=NA_character_ )
b6_cols_a <- c( SCFY2=NA_character_ )

ipeds_award_categories_ugaida <- unique(c(aid_federal_grant,aid_state_local,aid_institution,aid_federal_other,aid_other))

ipeds_saf_a <- sfa_group1 %>%
    
    summarise( SCFA2 = n_distinct(ID),  # IPEDS SFA Part A, Line 01
               UGAIDN = n_distinct(ID[Award_Amount > 0], na.rm = TRUE),  # IPEDS Part B, Line 01, Column 1
               UGAIDPN = n_distinct(ID[Award_Category %in% aid_federal_pell & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part B, Line 02, Column 1
               UGLNFN = n_distinct(ID[Award_Category %in% aid_federal_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part B, Line 03, Column 1
               UGAIDA = sum(Award_Amount, na.rm = TRUE),  # IPEDS SFA Part B, Line 01, Column 3
               UGAIDPA = sum(Award_Amount[Award_Category %in% aid_federal_pell], na.rm = TRUE),  # IPEDS SFA Part B, Line 02, Column 3
               UGLNFA = sum(Award_Amount[Award_Category %in% aid_federal_loans], na.rm = TRUE),  # IPEDS SFA Part B, Line 03, Column 3
    ) %>%
    
    add_column( !!!saf_a_cols[!names(saf_a_cols) %in% names(.)] ) %>%

    mutate_at(.vars=names(n_cols_a), function(x) {return( sprintf("%06d", x) )} ) %>%
    mutate_at(.vars=names(a_cols_a), function(x) {return( sprintf("%012.0f", x) )} ) %>%
    mutate_at(.vars=names(b6_cols_a), function(x) {return( coalesce(x, "      ") )} ) %>%

    # Add additional columns as per Import Specs
    mutate( UNITID = ipeds_unitid,
            SURVSECT = "SFA",
            PART = 'A' ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( !! names(saf_a_cols) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( saf_a_cols, a_cols_a, n_cols_a )
}

#
# Part B is First-time/First-Term, Counts of students (GROUP 2)
#
saf_b_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
                 SCFA1N=NA_character_, SCFA11N=NA_character_, SCFA12N=NA_character_, SCFA13N=NA_character_,
                 SCFY1N=NA_character_,  # IPEDS SFA Part A, Line 02
                 ANYAIDN=NA_character_, ANYAIDNF=NA_character_ )
n_cols_b <- c( SCFA1N=NA_character_, SCFA12N=NA_character_, SCFA13N=NA_character_,
               ANYAIDN=NA_character_, ANYAIDNF=NA_character_ )
b6_cols_b <- c( SCFY1N=NA_character_, SCFA11N=NA_character_  )


ipeds_saf_b <- sfa_group2 %>%
    
    summarise( SCFA1N = n_distinct(ID),  # IPEDS SFA Part A, Line 02
               SCFA12N = n_distinct(ID[Tuition_Type == "In-State"], na.rm = TRUE),  # IPEDS SFA Part C, Page 1, Line 01b
               SCFA13N = n_distinct(ID[Tuition_Type == "Out-of-State"], na.rm = TRUE),  # IPEDS SFA Part C, Page 1, Line 01c
               
               ANYAIDN = n_distinct(ID[Award_Category %in% aid_group_2a & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part A, Line 02a
               ANYAIDNF = n_distinct(ID[Award_Category %in% aid_group_2b & Award_Amount >0], na.rm = TRUE)  # IPEDS SFA Part A, Line 02b
    ) %>%
    
    add_column( !!!saf_b_cols[!names(saf_b_cols) %in% names(.)] ) %>%
    
    mutate_at(.vars=names(n_cols_b), function(x) {return( sprintf("%06d", x) )} ) %>%
    mutate_at(.vars=names(b6_cols_b), function(x) {return( coalesce(x, "      ") )} ) %>%

    # Add additional columns as per Import Specs
    mutate( UNITID = ipeds_unitid,
            SURVSECT = "SFA",
            PART = 'B',
            SCFA11N = "000000"   # IPEDS SFA Part C, Page 1, Line 01a
            ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( !! names(saf_b_cols) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( saf_b_cols, n_cols_b )
}

#
# Part C is First-time/First-Term, Counts and Total Awarded for various subgroups (GROUP 2)
#
saf_c_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
                 AGRNT_N=NA_character_, FGRNT_N=NA_character_, PGRNT_N=NA_character_, OFGRNT_N=NA_character_,
                 SGRNT_N=NA_character_, IGRNT_N=NA_character_, 
                 LOAN_N=NA_character_, FLOAN_N=NA_character_, OLOAN_N=NA_character_,
                 PGRNT_T=NA_character_, OFGRNT_T=NA_character_, SGRNT_T=NA_character_, IGRNT_T=NA_character_, 
                 FLOAN_T=NA_character_, OLOAN_T=NA_character_ )
n_cols_c <- c( AGRNT_N=NA_character_, FGRNT_N=NA_character_, PGRNT_N=NA_character_, OFGRNT_N=NA_character_,
               SGRNT_N=NA_character_, IGRNT_N=NA_character_, 
               LOAN_N=NA_character_, FLOAN_N=NA_character_, OLOAN_N=NA_character_ )
a_cols_c <- c( PGRNT_T=NA_character_, OFGRNT_T=NA_character_, SGRNT_T=NA_character_, IGRNT_T=NA_character_, 
               FLOAN_T=NA_character_, OLOAN_T=NA_character_ )

ipeds_award_categories_c02_q1 <- unique(c(aid_federal_grant,aid_state_local,aid_institution,aid_federal_other))
ipeds_award_categories_c02_q2 <- unique(c(aid_federal_non_pell,aid_federal_other))

ipeds_saf_c <- sfa_group2 %>%
    
    summarise( AGRNT_N = n_distinct(ID[Award_Category %in% ipeds_award_categories_c02_q1 & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 01, Column 1
               FGRNT_N = n_distinct(ID[Award_Category %in% aid_federal_grant & Award_Type == 'F' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02, Column 1
               PGRNT_N = n_distinct(ID[Award_Category %in% aid_federal_pell & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02a, Column 1
               OFGRNT_N = n_distinct(ID[Award_Category %in% ipeds_award_categories_c02_q2 & Award_Type == 'F' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02b, Column 1
               SGRNT_N = n_distinct(ID[Award_Category %in% aid_state_local & Award_Type == 'S' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 03, Column 1
               IGRNT_N = n_distinct(ID[Award_Category %in% aid_institution & Award_Type == 'I' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 04, Column 1
               LOAN_N = n_distinct(ID[Award_Category %in% aid_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05, Column 1
               FLOAN_N = n_distinct(ID[Award_Category %in% aid_federal_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05a, Column 1
               OLOAN_N = n_distinct(ID[Award_Category %in% aid_other_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05b, Column 1
               
               PGRNT_T = sum(Award_Amount[Award_Category %in% aid_federal_pell], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02a, Column 3
               OFGRNT_T = sum(Award_Amount[Award_Category %in% ipeds_award_categories_c02_q2 & Award_Type == 'F'], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02b, Column 3
               SGRNT_T = sum(Award_Amount[Award_Category %in% aid_state_local & Award_Type == 'S'], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 03, Column 3
               IGRNT_T = sum(Award_Amount[Award_Category %in% aid_institution & Award_Type == 'I'], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 04, Column 3
               FLOAN_T = sum(Award_Amount[Award_Category %in% aid_federal_loans], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05a, Column 3
               OLOAN_T = sum(Award_Amount[Award_Category %in% aid_other_loans], na.rm = TRUE)  # IPEDS SFA Part C, Page 2, Line 05b, Column 3
    ) %>%
    
    #mutate( OFGRNT_N = FGRNT_N - PGRNT_N, OFGRNT_T = FGRNT_T - PGRNT_T ) %>%
    
    mutate_at(.vars=names(n_cols_c), function(x) {return( sprintf("%06d", x) )} ) %>%
    mutate_at(.vars=names(a_cols_c), function(x) {return( sprintf("%012.0f", x) )} ) %>%
    
    # Add additional columns as per Import Specs
    mutate( UNITID = ipeds_unitid,
            SURVSECT = "SFA",
            PART = 'C' ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( !! names(saf_c_cols) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( saf_c_cols, a_cols_c, n_cols_c )
}

groupc_pgrnt <- as.integer((ipeds_saf_c %>% collect %$% PGRNT_T)[1])
groupc_ofgrnt <- as.integer((ipeds_saf_c %>% collect %$% OFGRNT_T)[1])
groupc_sgrnt <- as.integer((ipeds_saf_c %>% collect %$% SGRNT_T)[1])
groupc_igrnt <- as.integer((ipeds_saf_c %>% collect %$% IGRNT_T)[1])
groupc_awards <- groupc_pgrnt + groupc_ofgrnt + groupc_sgrnt + groupc_igrnt

#
# Part D is first-time/first-Term in-state students who were awarded grants or scholarships from
#     federal government, state/local government, or the institution, Counts and Total Awarded 
#     for various subgroups (GROUP 3)
#
saf_d_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
                 FTFT_Y1=NA_character_, FTFT_Y2=NA_character_, FTFT_Y3=NA_character_,
                 ONCAM_Y1=NA_character_, ONCAM_Y2=NA_character_, ONCAM_Y3=NA_character_,
                 OF_W_FAM_Y1=NA_character_, OF_W_FAM_Y2=NA_character_, OF_W_FAM_Y3=NA_character_, 
                 OF_WO_FAM_Y1=NA_character_, OF_WO_FAM_Y2=NA_character_, OF_WO_FAM_Y3=NA_character_, 
                 TOTALG_Y1=NA_character_ )
n_cols_d <- c( FTFT_Y1=NA_character_, ONCAM_Y1=NA_character_, OF_W_FAM_Y1=NA_character_, OF_WO_FAM_Y1=NA_character_ )
a_cols_d <- c( TOTALG_Y1=NA_character_ )
b6_cols_d <- c( FTFT_Y2=NA_character_, FTFT_Y3=NA_character_,
                ONCAM_Y2=NA_character_, ONCAM_Y3=NA_character_,
                OF_W_FAM_Y2=NA_character_, OF_W_FAM_Y3=NA_character_, 
                OF_WO_FAM_Y2=NA_character_, OF_WO_FAM_Y3=NA_character_
                )

ipeds_saf_d <- sfa_group3 %>%
    
    summarise( FTFT_Y1 = n_distinct(ID[Award_Amount >0], na.rm = TRUE),   # IPEDS SFA Part A, Line 03
               ONCAM_Y1 = 0,  # IPEDS SFA Part D, Line 01a, Column 3
               OF_W_FAM_Y1 = n_distinct(ID[Living_Arrangement == "HOME" & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part D, Line 01b, Column 3
               OF_WO_FAM_Y1 = n_distinct(ID[Living_Arrangement == "AWAY" & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part D, Line 01c, Column 3

               TOTALG_Y1 = sum(Award_Amount, na.rm = TRUE)  # IPEDS SFA Part D, Line 02, Column 3
    ) %>%
    
    add_column( !!!saf_d_cols[!names(saf_d_cols) %in% names(.)] ) %>%
    
    mutate_at(.vars=names(n_cols_d), function(x) {return( sprintf("%06d", x) )} ) %>%
    mutate_at(.vars=names(a_cols_d), function(x) {return( sprintf("%012.0f", x) )} ) %>%
    mutate_at(.vars=names(b6_cols_d), function(x) {return( coalesce(x, "      ") )} ) %>%
    
    # Add additional columns as per Import Specs
    mutate( UNITID = ipeds_unitid,
            SURVSECT = "SFA",
            PART = 'D' ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( !! names(saf_d_cols) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( saf_d_cols, a_cols_d, n_cols_d )
}

group3_awards <- as.integer((ipeds_saf_d %>% collect %$% TOTALG_Y1)[1])

if( group3_awards > groupc_awards ) {
    stop( "Group 3 awards in Part D are greater than the Group 2 awards in Part C, Page 2")
}

#
# Part E is first-time/first-Term in-state students who were awarded awarded Title IV federal student aid 
#     which includes Federal Pell, Federal Supplemental Educational Opportunity Grant (FSEOG), Academic 
#     Competitiveness Grant (ACG), National Science and Mathematics Access to Retain Talent Grant 
#     (National SMART Grant), Teacher Education Assistance for College and Higher Education (TEACH),
#     Federal Work Study, Federal Perkins Loan, Subsidized Direct or FFEL Stafford Loan, and
#     Unsubsidized Direct or FFEL Stafford Loan, Counts by various subgroups (GROUP 4)
#
saf_e_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
                 YEAR=NA_character_, FTFT_T4=NA_character_, ONCAM_T4=NA_character_, OF_W_FAM_T4=NA_character_, OF_WO_FAM_T4=NA_character_ )
n_cols_e <- c( FTFT_T4=NA_character_, ONCAM_T4=NA_character_, OF_W_FAM_T4=NA_character_, OF_WO_FAM_T4=NA_character_ )

ipeds_saf_e <- sfa_group4 %>%
    
    summarise( YEAR = 1,   # IPEDS SFA Part E, Column 3
               ONCAM_T4=0,  # IPEDS SFA Part A, Line 04
               FTFT_T4 = n_distinct(ID[Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part E, Line 01a, Column 1/2/3
               OF_W_FAM_T4 = n_distinct(ID[Living_Arrangement == "HOME" & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part E, Line 01b, Column 1/2/3
               OF_WO_FAM_T4 = n_distinct(ID[Living_Arrangement == "AWAY" & Award_Amount >0], na.rm = TRUE)  # IPEDS SFA Part E, Line 01c, Column 1/2/3
    ) %>%
    
    mutate_at(.vars=names(n_cols_e), function(x) {return( sprintf("%06d", x) )} ) %>%

    # Add additional columns as per Import Specs
    mutate( UNITID = ipeds_unitid,
            SURVSECT = "SFA",
            PART = 'E' ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( !! names(saf_e_cols) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( saf_e_cols, n_cols_e )
}

#
# Part F is first-time/first-Term in-state students who were awarded awarded Title IV federal student aid 
#     which includes Federal Pell, Federal Supplemental Educational Opportunity Grant (FSEOG), Academic 
#     Competitiveness Grant (ACG), National Science and Mathematics Access to Retain Talent Grant 
#     (National SMART Grant), Teacher Education Assistance for College and Higher Education (TEACH),
#     Federal Work Study, Federal Perkins Loan, Subsidized Direct or FFEL Stafford Loan, and
#     Unsubsidized Direct or FFEL Stafford Loan, Counts and Total Awarded by various subgroups (GROUP 4)
#
saf_f_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, YEAR=NA_character_, 
                 INCOME_RANGE=NA_character_, T4AID_N=NA_character_, T4AID_G=NA_character_, T4AID_T=NA_character_ )
n_cols_f <- c( T4AID_N=NA_character_, T4AID_G=NA_character_ )
a_cols_f <- c( T4AID_T=NA_character_ )

ipeds_saf_f <- sfa_group4 %>%
    
    rename( INCOME_RANGE = Income_Range ) %>%
    group_by( INCOME_RANGE ) %>%   # IPEDS SFA Part E, Line a/b/c/d/e or 2/3/4
    
    summarise( T4AID_N = n_distinct(ID[Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part E, Line 2/3/4, Column 1
               T4AID_G = n_distinct(ID[Award_Category %in% aid_federal_grant & Award_Type == 'F' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part E, Line 2/3/4, Column 2

               T4AID_T = sum(Award_Amount[Award_Category %in% aid_federal_grant], na.rm = TRUE)  # IPEDS SFA Part E, Line 2/3/4, Column 3
    ) %>%
    
    mutate_at(.vars=names(n_cols_f), function(x) {return( sprintf("%06d", x) )} ) %>%
    mutate_at(.vars=names(a_cols_f), function(x) {return( sprintf("%012.0f", x) )} ) %>%
    
    # Add additional columns as per Import Specs
    mutate( UNITID = ipeds_unitid,
            SURVSECT = "SFA",
            PART = 'F',
            YEAR = '1'   # IPEDS SFA Part 
            ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( !! names(saf_f_cols) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( saf_f_cols, a_cols_f, n_cols_f )
}

#
# Part G is Veteran's benefits
#
saf_g_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, SLEVEL=NA_character_,
                 GI_BEN_N=NA_character_, GI_BEN_T=NA_character_, DOD_ASSIST_N=NA_character_, DOD_ASSIST_T=NA_character_ )
n_cols_g <- c( GI_BEN_N=NA_character_, DOD_ASSIST_N=NA_character_ )
a_cols_g <- c( GI_BEN_T=NA_character_, DOD_ASSIST_T=NA_character_ )

ipeds_saf_g <- sfa_group1 %>%
    
    summarise( GI_BEN_N = n_distinct(ID[Award_Code %in% aid_veterans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Section 2, Line 01, Column 1
               GI_BEN_T = sum(Award_Amount[Award_Code %in% aid_veterans_gi], na.rm = TRUE),  # IPEDS SFA Section 2, Line 01, Column 2
               
               DOD_ASSIST_N = n_distinct(ID[Award_Code %in% aid_veterans_dod & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Section 2, Line 03, Column 1
               DOD_ASSIST_T = sum(Award_Amount[Award_Code %in% aid_veterans_dod], na.rm = TRUE)  # IPEDS SFA Section 2, Line 03, Column 2
    ) %>%
    
    mutate_at(.vars=names(n_cols_g), function(x) {return( sprintf("%06d", x) )} ) %>%
    mutate_at(.vars=names(a_cols_g), function(x) {return( sprintf("%012.0f", x) )} ) %>%
    
    # Add additional columns as per Import Specs
    mutate( UNITID = ipeds_unitid,
            SURVSECT = "SFA",
            PART = 'G',
            SLEVEL = '1'   # IPEDS SFA Section 2, Line 1/3 
            ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( !! names(saf_g_cols) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( saf_g_cols, a_cols_g, n_cols_g )
}

#
# Now, write all the parts out a flat file for import into IPEDS
#

if (WRITE_OUTPUT) {
    write.table( data.frame(ipeds_saf_a), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
    write.table( data.frame(ipeds_saf_b), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_saf_c), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_saf_d), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_saf_e), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_saf_f), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_saf_g), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
}

if (TEST) {
    write_csv( sfa_group1, file.path(output_path, str_c("sfa_group1_",report_year,".csv")), na = "")
    write_csv( sfa_group2, file.path(output_path, str_c("sfa_group2_",report_year,".csv")), na = "")
    write_csv( sfa_group3, file.path(output_path, str_c("sfa_group3_",report_year,".csv")), na = "")
    write_csv( sfa_group4, file.path(output_path, str_c("sfa_group4_",report_year,".csv")), na = "")
    write_csv( ipeds_saf_a, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"a",sep="_"),".csv")), na = "" )
    write_csv( ipeds_saf_b, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"b",sep="_"),".csv")), na = "" )
    write_csv( ipeds_saf_c, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"c",sep="_"),".csv")), na = "" )
    write_csv( ipeds_saf_d, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"d",sep="_"),".csv")), na = "" )
    write_csv( ipeds_saf_e, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"e",sep="_"),".csv")), na = "" )
    write_csv( ipeds_saf_f, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"f",sep="_"),".csv")), na = "" )
    write_csv( ipeds_saf_g, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"g",sep="_"),".csv")), na = "" )
}
