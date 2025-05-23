#OVERRIDE_REPORT_YEAR <- 2018 # Set to NA to use current year
#OVERRIDE_COHORTS <- TRUE

WRITE_OUTPUT <- TRUE

ir_root <- "L:/IERG"

nsc_path <- file.path(ir_root, "Data", "NSC")
ipeds_path <- file.path(ir_root, "Data", "IPEDS")

cohort_from_file_year <- 2015

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

if (!exists("OVERRIDE_COHORTS")) {
    OVERRIDE_COHORTS <- NA_integer_
}

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

# For file names
report_time_str <- format(Sys.time(),"%Y%m%d_%H%M")

# Fix report year to be the year of the Fall term
report_year <- report_year - if_else(report_month < 7, 1, 0)

# Use the current report year to determin the folder for the data
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
nsc_detail_pat <- str_c(".*_DETLRPT_SE_.*_hcc_ipeds_g2_", report_year, "_se.*csv")
#nsc_detail_pat <- str_c("(", report_year_150, "|", report_year_200, ")fa","_DETLRPT_SE_.*.csv")
# nsc_detail_pat <- str_c("hcc_ipeds_g2_",report_year,"_se")
nsc_folder <- "nsc"
nsc_path <- file.path(".", nsc_folder)

USE_FILE_COHORTS_150 <- report_year_150 < cohort_from_file_year
USE_FILE_COHORTS_200 <- report_year_200 < cohort_from_file_year

#project_path <- file.path(ir_root,"Reporting","IPEDS","R")
#project_path <- file.path(ir_root,"Reporting","IPEDS",report_year_folder,"R")
project_path <- '.'
#input_path <- file.path(project_path, "input")
output_path <- file.path(project_path, "output")

fn_G2 <- str_c("ipeds_",report_year,"_g2_",report_time_str,".txt")
fn_G22 <- str_c("ipeds_",report_year,"_g22_",report_time_str,".txt")

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

#
# Use the old cohort value for years prior to 2018. 
# After that, the IPEDS reports all used the one generated outside of Colleague.
#
ipeds_cohort_COLLEAGUE_COHORTS2 <- getColleagueData( "STUDENT_ACAD_LEVELS" ) %>%
    filter( STA.FED.COHORT.GROUP %in% c(report_cohorts) ) %>%
    select( ID=STA.STUDENT, Cohort=STA.FED.COHORT.GROUP ) %>%
    collect() %>%
    mutate( IPEDS_Report = if_else(Cohort==report_ftft_150,150,200),
            Term_ID = str_c(substring(Cohort,1,4),"FA") ) %>%
    inner_join( report_terms %>% 
                    filter( ( (Term_Reporting_Year==report_year_150) & (report_year_150 < 2018)) |
                                ( (Term_Reporting_Year==report_year_200) & (report_year_200 < 2018)) ) %>%
                    select(Term_ID, Cohort_Start_Date = Term_Start_Date) )

ipeds_cohort_FILE_COHORTS <- getColleagueData( "ipeds_cohorts", schema="local" ) %>%
    select( ID, Cohort = Term_Cohort, Term_ID ) %>%
    filter( Cohort %in% c(report_cohorts) ) %>%
    distinct() %>%
    collect() %>%
    mutate( IPEDS_Report = if_else(Cohort==report_ftft_150,150,200) ) %>%
    inner_join( report_terms %>% 
                    filter( ( (Term_Reporting_Year == report_year_150 ) & USE_FILE_COHORTS_150 ) |
                                (Term_Reporting_Year == report_year_200 ) & USE_FILE_COHORTS_200 ) %>%
                    select(Term_ID, Cohort_Start_Date = Term_Start_Date) ) #%>%


ipeds_cohort_COLLEAGUE_COHORTS <- getColleagueData( "STUDENT_TERMS" ) %>%
    select( ID = STTR.STUDENT, Cohort = STTR.FED.COHORT.GROUP ) %>%
    filter( Cohort %in% c(report_cohorts) ) %>%
    distinct() %>%
    collect() %>%
    mutate( Term_ID = str_c(substring(Cohort,1,4),"FA"),
            IPEDS_Report = if_else(Cohort==report_ftft_150,150,200) ) %>%
    inner_join( report_terms %>% 
                    filter( ( (Term_Reporting_Year==report_year_150) & !USE_FILE_COHORTS_150 ) |
                            ( (Term_Reporting_Year==report_year_200) & !USE_FILE_COHORTS_200 ) ) %>%
                    select(Term_ID, Cohort_Start_Date = Term_Start_Date) ) #%>%
    #select( -Student_Type )

#ipeds_cohort <- ipeds_cohort_FILE_COHORTS %>%
#    bind_rows( ipeds_cohort_COLLEAGUE_COHORTS )
ipeds_cohort <- getColleagueData( "ipeds_cohorts", schema="local", version="latest" ) %>%
    filter( Cohort %in% c(report_cohorts) ) %>%
    select( ID, Cohort, Term_ID ) %>%
    collect() %>%
    mutate( IPEDS_Report = if_else(Cohort==report_ftft_150,150,200) ) %>%
    inner_join( terms %>% select(Term_ID,Cohort_Start_Date=Term_Start_Date) ) #%>%
#    filter( Cohort_Year %in% c(report_year),
#            Term_ID == report_term )

person <- getColleagueData( "PERSON" ) %>%
    filter( FIRST.NAME != "" ) %>%
    select( ID, 
            First_Name = FIRST.NAME, 
            Last_Name = LAST.NAME, 
            Gender = GENDER, 
            ETHNIC, PER.ETHNICS, 
            PER.RACES, 
            X.ETHNICS.RACES, 
            CITIZENSHIP, 
            RESIDENCE.COUNTRY, 
            VISA.TYPE ) %>%
    collect() %>%
    inner_join( ipeds_cohort %>% select(ID) ) %>%
    mutate( CITIZENSHIP = if_else( CITIZENSHIP == "USA", "", CITIZENSHIP ) ) %>%
 #   left_join( ethnics ) %>%
 #   left_join( foreign_person ) %>%
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
    IPEDS_Race_Code = case_when(
        X.ETHNICS.RACES == "FOR" & Gender == "M" ~ "GRRACE01",
        X.ETHNICS.RACES == "FOR" & Gender == "F" ~ "GRRACE02",
        X.ETHNICS.RACES == "HIS" & Gender == "M" ~ "GRRACE25",
        X.ETHNICS.RACES == "HIS" & Gender == "F" ~ "GRRACE26",
        X.ETHNICS.RACES == "MULTI" & Gender == "M" ~ "GRRACE37",
        X.ETHNICS.RACES == "MULTI" & Gender == "F" ~ "GRRACE38",
        X.ETHNICS.RACES == "AN" & Gender == "M" ~ "GRRACE27",
        X.ETHNICS.RACES == "AN" & Gender == "F" ~ "GRRACE28",
        X.ETHNICS.RACES == "AS" & Gender == "M" ~ "GRRACE29",
        X.ETHNICS.RACES == "AS" & Gender == "F" ~ "GRRACE30",
        X.ETHNICS.RACES == "BL" & Gender == "M" ~ "GRRACE31",
        X.ETHNICS.RACES == "BL" & Gender == "F" ~ "GRRACE32",
        X.ETHNICS.RACES == "HP" & Gender == "M" ~ "GRRACE33",
        X.ETHNICS.RACES == "HP" & Gender == "F" ~ "GRRACE34",
        X.ETHNICS.RACES == "WH" & Gender == "M" ~ "GRRACE35",
        X.ETHNICS.RACES == "WH" & Gender == "F" ~ "GRRACE36",
        TRUE ~ if_else( Gender == "M", "GRRACE13", "GRRACE14" )
    )) %>%
    select( ID, First_Name, Last_Name, Gender, IPEDS_Race, IPEDS_Race_Code )

acad_programs <- getColleagueData( "ACAD_PROGRAMS" ) %>%
    select( Program = ACAD.PROGRAMS.ID,
            Program_Length = ACPG.CMPL.MONTHS ) %>%
    filter( !(Program %in% c('BSP', 'AHS', 'CONED', '=GED', '=HISET', 'C11', 'C50')) ) %>%
    collect() %>%
    mutate( Program_Length_100 = as.integer(Program_Length) ) %>%
    mutate( Program_Length_100 = case_when(
        substring(Program,1,1) == "P" ~ as.integer(24),
        TRUE ~ Program_Length_100
    )) %>%
    mutate( Program_Length_150 = Program_Length_100 * 1.5,
            Program_Length_200 = Program_Length_100 * 2 ) %>%
    select( -Program_Length )

student_programs__dates <- getColleagueData( "STUDENT_PROGRAMS__STPR_DATES", version="history" ) %>%
    select( ID = STPR.STUDENT, Program = STPR.ACAD.PROGRAM, Program_Start_Date = STPR.START.DATE, Program_End_Date = STPR.END.DATE, EffectiveDatetime ) %>%
    filter( !(Program %in% c('BSP', 'AHS', 'CONED', '=GED', '=HISET', 'C11', 'C50')) ) %>%
    collect() %>%
    inner_join( ipeds_cohort %>% select(ID) ) %>%
    mutate( Program_End_Date = coalesce(Program_End_Date,as.Date('9999-12-31')) )
    
student_programs <- getColleagueData( "STUDENT_PROGRAMS" ) %>%
    select( ID = STPR.STUDENT, Program = STPR.ACAD.PROGRAM, EffectiveDatetime ) %>%
    collect() %>%
    inner_join( student_programs__dates ) %>%
    select( -EffectiveDatetime ) %>%
    inner_join( ipeds_cohort %>% select(ID, Cohort_Start_Date, IPEDS_Report) ) %>%
    mutate( Credential_Level = case_when(
        substring(Program,1,1) == "A" ~ 10000,
        substring(Program,1,1) == "D" ~ 1,
        substring(Program,1,1) %in% c("C","P") ~ 1,
        TRUE ~ 0
        ), 
        j = 1 ) %>%
    full_join( terms %>% select(Term_ID, Term_Start_Date, Term_Census_Date, Term_End_Date) %>% mutate(j=1) ) %>%
    filter( Program_Start_Date <= Term_Census_Date,
            Program_End_Date >= Term_Census_Date,
            Credential_Level > 0 ) %>%
    select( -Term_Census_Date, -j ) %>%
    mutate( Program_Start_Date = if_else(Program_Start_Date < report_term_start_date, report_term_start_date, Program_Start_Date),
            Program_End_Date = if_else(Program_End_Date < Term_End_Date, Term_End_Date, Program_End_Date) ) %>%
    left_join( acad_programs ) %>%
    select( -c(Program_Start_Date) ) %>%
    mutate( IPEDS_Date_100 = Cohort_Start_Date %m+% months(Program_Length_100),
            IPEDS_Date_150 = ceiling_date(Cohort_Start_Date + duration(Program_Length_150,"months"), "day"),
            IPEDS_Date_200 = Cohort_Start_Date %m+% months(Program_Length_200),
            Cutoff_Date = report_cutoff_date ) %>%
    select( -c(Program_Length_100,Program_Length_150,Program_Length_200) ) %>%
    mutate( IPEDS_Date_100 = pmin(IPEDS_Date_100, Cutoff_Date),
            IPEDS_Date_150 = pmin(IPEDS_Date_150, Cutoff_Date),
            IPEDS_Date_200 = pmin(IPEDS_Date_200, Cutoff_Date) ) %>%
    mutate( IPEDS_Date_150 = if_else((Credential_Level == 10000) & (IPEDS_Report == 150), pmax(IPEDS_Date_150, Cutoff_Date), IPEDS_Date_150),
            IPEDS_Date_200 = if_else((Credential_Level == 10000) & (IPEDS_Report == 200), pmax(IPEDS_Date_200, Cutoff_Date), IPEDS_Date_200) ) %>%
    distinct() %>%
    select( -c(Cutoff_Date,Program_End_Date) )

acad_credentials <- getColleagueData( "ACAD_CREDENTIALS" ) %>%
    filter( ACAD.INSTITUTIONS.ID == "0019844",
            ACAD.END.DATE > report_term_start_date || ACAD.CAST.DATE > report_term_start_date,
            !(ACAD.ACAD.PROGRAM %in% c("AHS", "=GED", "HSEGED")) ) %>%
    select( ID = ACAD.PERSON.ID,
            Term_ID = ACAD.TERM, 
            Program = ACAD.ACAD.PROGRAM,
            End_Date = ACAD.END.DATE,
            Cast_Date = ACAD.CAST.DATE
            ) %>%
    collect() %>%
    inner_join( ipeds_cohort %>% select(ID,Cohort_Start_Date) ) %>%
    left_join( terms %>% select(Term_ID, Term_End_Date ) ) %>%
    mutate( Cast_Date = as.Date(Cast_Date) ) %>%
    mutate( Graduation_Date = if_else(Cast_Date < Term_End_Date, Term_End_Date, Cast_Date),
            Graduated = 1 ) %>%
    filter( Term_End_Date >= Cohort_Start_Date ) %>%
    select( -c(Cast_Date, End_Date, Term_End_Date, Cohort_Start_Date) )

student_acad_cred <- getColleagueData( "STUDENT_ACAD_CRED" ) %>%
    filter( STC.STATUS %in% c('A','N','W'),
            STC.START.DATE >= report_term_start_date ) %>%
    select( ID = STC.PERSON.ID, Term_ID = STC.TERM ) %>%
    distinct() %>%
    collect() %>%
    inner_join( terms %>% select(Term_ID,Term_Start_Date) ) %>%
    left_join( report_term_still_enrolled %>% select(Term_ID) %>% mutate(Enrolled_IPEDS = 1) ) %>%
    inner_join(ipeds_cohort %>% select(ID,Cohort_Start_Date)) %>%
    filter( Term_Start_Date >= Cohort_Start_Date) %>%
    mutate( Enrolled = 1,
            Enrolled_IPEDS = coalesce(Enrolled_IPEDS,0) ) %>%
    select( -c(Term_Start_Date,Cohort_Start_Date) )

##
## Now we need to incorporate the Transfer Outs for those who did not graduate
##

# Get the NSC file for the report year and run it through nsc_return_se_convert().
nsc_files <- list.files( path = nsc_path,
                         pattern = nsc_detail_pat, 
                         full.names = TRUE, 
                         ignore.case = TRUE ) %>% 
    lapply(function(x) readr::read_csv(x, col_types = cols(.default = col_character()))) %>% 
    bind_rows %>%
    separate( col=`Requester Return Field`, into=c("ID","Term_ID"), sep="\\;" ) %>%
    filter( `College Code/Branch` != "008083-00") %>%
    ungroup() %>%
    inner_join(ipeds_cohort %>% select(ID,Cohort)) %>% #Term_ID)) %>%
    select( ID, College_Name=`College Name`, Enrollment_Begin=`Enrollment Begin`, Enrollment_End=`Enrollment End` ) %>%
    mutate( Enrollment_Begin = ymd(Enrollment_Begin),
            Enrollment_End = ymd(Enrollment_End) ) %>%
    mutate( j=1 ) %>%
    full_join( terms %>% select(Term_ID, Term_Start_Date, Term_Census_Date, Term_End_Date) %>% mutate(j=1) ) %>%
    filter( (Enrollment_Begin <= Term_Start_Date & Enrollment_End > Term_Start_Date) |
            (Enrollment_Begin <= Term_End_Date & Enrollment_End > Term_End_Date) |
            (Enrollment_Begin >= Term_Start_Date & Enrollment_End <= Term_End_Date) ) %>%
    select( ID, Transfer_Term_Start_Date=Term_Start_Date ) %>%
    mutate( j=1 ) %>%
    full_join( terms %>% select(Term_ID, Term_Start_Date) %>% mutate(j=1) ) %>%
    filter( Transfer_Term_Start_Date <= Term_Start_Date ) %>%
    mutate( Transferred = 1 ) %>%
    select( -c(Transfer_Term_Start_Date, j) ) %>%
    distinct()
    
ipeds_terms <- ipeds_cohort %>%
    select( ID, Cohort_Start_Date, IPEDS_Report ) %>%
    mutate( j=1 ) %>%
    full_join( terms %>% select(Term_ID, Term_Start_Date) %>% mutate(j=1) ) %>%
    select(-j) %>%
    filter( Term_Start_Date >= Cohort_Start_Date ) %>%
    left_join( student_programs ) %>%
    left_join( acad_credentials ) %>%
    left_join( student_acad_cred ) %>%
    left_join( nsc_files ) %>%
    
    # People who graduate in 100% of time are considered having graduated in 150% of time
    # The 200% report are only those who graduated after the 150% mark
    mutate( Grad_100 = Credential_Level * (Graduation_Date <= IPEDS_Date_100),
            Grad_150 = Credential_Level * (Graduation_Date <= IPEDS_Date_150),
            Grad_200 = Credential_Level * (Graduation_Date <= IPEDS_Date_200) * (Graduation_Date > IPEDS_Date_150),
            Transferred = if_else( coalesce(Enrolled,0) == 1 | coalesce(Graduated,0) == 1, 0, Transferred ) ) %>%
    
    select( -c(IPEDS_Date_100, IPEDS_Date_150, IPEDS_Date_200, Graduation_Date, Graduated,
               Program, Term_End_Date, Credential_Level, Enrolled) ) %>%
    mutate( Enrolled_IPEDS = coalesce(Enrolled_IPEDS,0),
            Transferred = coalesce(Transferred,0),
            Grad_100 = coalesce(Grad_100,0),
            Grad_150 = coalesce(Grad_150,0),
            Grad_200 = coalesce(Grad_200,0) )

ipeds_terms_grads <- ipeds_terms %>%
    select( ID, Term_Start_Date, Grad_100, Grad_150, Grad_200 ) %>%
    gather( "Grad_Time", "Grad_Level", -c(ID, Term_Start_Date) ) %>%
    mutate( Grad_Time = str_replace(Grad_Time,".*_","") ) %>%
    filter( Grad_Level > 0 ) %>%
    group_by( ID, Term_Start_Date, Grad_Time ) %>%
    summarise( Grad_Level = max(Grad_Level) ) %>%
    group_by( ID, Grad_Time, Grad_Level ) %>%
    summarise( Graduation_Date = min(Term_Start_Date) ) %>%
    ungroup()

ipeds_terms_transferred <- ipeds_terms %>%
    select( ID, Term_Start_Date, Transferred ) %>%
    filter( Transferred > 0 ) %>%
    left_join( ipeds_terms_grads ) %>%
    mutate( Graduation_Date = coalesce(Graduation_Date,as.Date("9999-12-31")) ) %>%
    filter( Term_Start_Date <= Graduation_Date ) %>%
    group_by( ID, Transferred ) %>%
    summarise( Transferred_Date = min(Term_Start_Date) ) %>%
    ungroup()

fa_terms <- terms %>%
    filter( Term_Reporting_Year %in% c(report_year_150) ) %>% 
    collect %$% 
    Term_ID

if (report_year < 2018) {
    federal_award_types <- c("FPELL") # As listed in FA_TRANSMITTALS
    
    ta_acyr <- getColleagueData( "FA_TRANSMITTALS" ) %>%
        filter( FAX.TA.TERM %in% fa_terms, FAX.TA.AWARD %in% federal_award_types ) %>%
        select( ID=FAX.TA.STUDENT,
                NAME=FAX.STU.SORT.NAME,
                TERM=FAX.TA.TERM,
                DISTRIBUTED.AMOUNT=FAX.TA.TERM.AMT ) %>%
        collect() %>%
        inner_join(ipeds_cohort %>% select(ID)) %>%
        filter( DISTRIBUTED.AMOUNT > 0.00 ) %>%
        select( ID ) %>%
        distinct() %>%
        mutate( Pell_Recipient = 1 )
    
} else {
    federal_award_types <- c("PELL") # As listed in TA.AWARD.CATEGORY
    
    ta_acyr <- getColleagueData( "TA_ACYR" ) %>%
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
        inner_join(ipeds_cohort %>% select(ID)) %>%
        filter( DISTRIBUTED.AMOUNT > 0.00 ) %>%
        select( ID ) %>%
        distinct() %>%
        mutate( Pell_Recipient = 1 )
}

race_cols <- c( GRRACE01=NA_character_, GRRACE02=NA_character_,
                GRRACE25=NA_character_, GRRACE26=NA_character_,
                GRRACE27=NA_character_, GRRACE28=NA_character_,
                GRRACE29=NA_character_, GRRACE30=NA_character_,
                GRRACE31=NA_character_, GRRACE32=NA_character_,
                GRRACE33=NA_character_, GRRACE34=NA_character_,
                GRRACE35=NA_character_, GRRACE36=NA_character_,
                GRRACE37=NA_character_, GRRACE38=NA_character_,
                GRRACE13=NA_character_, GRRACE14=NA_character_ )
g2_b_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                Filler1=NA_character_,Filler2=NA_character_,LINE=NA_character_,Filler3=NA_character_,
                race_cols)


#
# Create the cohort row
#
ipeds_g2_b_10 <- ipeds_terms %>%
    filter( IPEDS_Report == 150 ) %>%
    select( ID, Cohort_Start_Date, Term_ID ) %>%
    mutate( IPEDS_Status = '10' ) %>%
    distinct() %>%
    inner_join( report_term_still_enrolled %>% select(Term_ID) ) %>%
    group_by( ID ) %>%
    left_join( person %>% select(ID,IPEDS_Race_Code) ) %>%
    group_by( IPEDS_Status, IPEDS_Race_Code ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    spread( IPEDS_Race_Code, Students, fill="000000" ) %>%
    rename( LINE = IPEDS_Status ) %>%
    add_column( !!!race_cols[!names(race_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(race_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'GR2',
            PART = 'B',
            Filler1 = ' ',
            Filler2 = '     ',
            Filler3 = ' ' ) %>%
    select( !! names(g2_b_cols) )
#    select_( .dots=names(g2_b_cols) )

ipeds_g2_b <- ipeds_terms %>%
    filter( IPEDS_Report == 150 ) %>%
    select( ID, Cohort_Start_Date, Term_ID, Term_Start_Date, Enrolled_IPEDS ) %>%
    distinct() %>%
    left_join( ipeds_terms_grads ) %>%
    left_join( ipeds_terms_transferred ) %>%
    mutate( Status_Type = case_when(
                Graduation_Date <= Term_Start_Date ~ case_when(
                    Grad_Time %in% c('100','150') ~ if_else(Grad_Level==1, '11', '12'),
                    TRUE ~ '99'
                ),
                Transferred_Date <= Term_Start_Date ~ '30',
                Enrolled_IPEDS > 0 ~ '51',
                TRUE ~ '99'
            ) ) %>%
    filter( Status_Type != '99' ) %>%
    inner_join( report_term_still_enrolled %>% select(Term_ID) ) %>%
    group_by( ID ) %>%
    summarise( IPEDS_Status = min(Status_Type) ) %>%
    left_join( person %>% select(ID,IPEDS_Race_Code) ) %>%
    group_by( IPEDS_Status, IPEDS_Race_Code ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    spread( IPEDS_Race_Code, Students, fill="000000" ) %>%
    rename( LINE = IPEDS_Status ) %>%
    ungroup() %>%
    add_column( !!!race_cols[!names(race_cols) %in% names(.)] ) %>%
    add_row( LINE = '45' ) %>%
    mutate_at(.vars=names(race_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'GR2',
            PART = 'B',
            Filler1 = ' ',
            Filler2 = '     ',
            Filler3 = ' ' ) %>%
    select( !! names(g2_b_cols) ) %>%
    # select_( .dots=names(g2_b_cols) ) %>%
    bind_rows( ipeds_g2_b_10 ) %>%
    arrange( LINE )

ipeds_g2_c <- ipeds_terms %>%
    filter( IPEDS_Report == 150 ) %>%
    select( ID, Cohort_Start_Date, Term_ID, Term_Start_Date, Enrolled_IPEDS ) %>%
    distinct() %>%
    left_join( ipeds_terms_grads ) %>%
    left_join( ipeds_terms_transferred ) %>%
    mutate( Status_Type = case_when(
                Graduation_Date <= Term_Start_Date ~ case_when(
                    Grad_Time %in% c('100') ~ if_else(Grad_Level==1, 'COMP10011', 'COMP10012'),
                    TRUE ~ 'ZZZZ99999'
                ),
                TRUE ~ 'ZZZZ99999'
            )
    ) %>%
    inner_join( report_term_still_enrolled %>% select(Term_ID) ) %>%
    group_by( ID ) %>%
    summarise( IPEDS_Status = min(Status_Type) ) %>%
    filter( IPEDS_Status != 'ZZZZ99999' ) %>%
    #left_join( person %>% select(ID,IPEDS_Race_Code) ) %>%
    #group_by( IPEDS_Status, IPEDS_Race_Code ) %>%
    group_by( IPEDS_Status ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    spread( IPEDS_Status, Students, fill="000000" ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'GR2',
            PART = 'C' ) %>%
    select( UNITID, SURVSECT, PART, COMP10011, COMP10012 )

#
# Create the cohort row
#
ipeds_g2_d_10 <- ipeds_terms %>%
    filter( IPEDS_Report == 150 ) %>%
    select( ID, Cohort_Start_Date, Term_ID ) %>%
    mutate( IPEDS_Status = '10' ) %>%
    distinct() %>%
    left_join( ta_acyr ) %>%
    mutate( Pell_Recipient = coalesce(Pell_Recipient,0) ) %>%
    inner_join( report_term_still_enrolled %>% select(Term_ID) ) %>%
    mutate( student = 1 ) %>%
    summarise( PELLGRANT_RCPT = sprintf("%06d", sum(Pell_Recipient) ),
               DIRECTLOAN_RCPT = "000000" ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'GR2',
            PART = 'D',
            Filler1 = '      ',
            LINE = '10',
            Filler2 = ' ' ) %>%
    select( UNITID, SURVSECT, PART, Filler1, LINE, Filler2, PELLGRANT_RCPT, DIRECTLOAN_RCPT )
    

ipeds_g2_d <- ipeds_terms %>%
    filter( IPEDS_Report == 150 ) %>%
    select( ID, Cohort_Start_Date, Term_ID, Term_Start_Date ) %>%
    distinct() %>%
    left_join( ipeds_terms_grads ) %>%
    left_join( ta_acyr ) %>%
    mutate( Pell_Recipient = coalesce(Pell_Recipient,0) ) %>%
    mutate( Pell_150 = case_when(
                Graduation_Date <= Term_Start_Date ~ case_when(
                    Grad_Time %in% c('100','150') ~ 1,
                    TRUE ~ 0
                ),
                TRUE ~ 0
            ) * Pell_Recipient
    ) %>%
    inner_join( report_term_still_enrolled %>% select(Term_ID) ) %>%
    group_by( ID ) %>%
    summarise( Pell_Recipient = max(Pell_Recipient),
               Pell_150 = max(Pell_150) ) %>%
    ungroup() %>%
    filter( Pell_150 == 1 ) %>%
    summarise( PELLGRANT_RCPT = sprintf("%06d",sum(Pell_Recipient)),
               DIRECTLOAN_RCPT = "000000" ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'GR2',
            PART = 'D',
            Filler1 = '      ',
            LINE = '29',
            Filler2 = ' ' ) %>%
    select( UNITID, SURVSECT, PART, Filler1, LINE, Filler2, PELLGRANT_RCPT, DIRECTLOAN_RCPT ) %>%
    add_row( UNITID = '198668',
             SURVSECT = 'GR2',
             PART = 'D',
             Filler1 = '      ',
             LINE = '45',
             Filler2 = ' ',
             PELLGRANT_RCPT = "000000", 
             DIRECTLOAN_RCPT = "000000" ) %>%
        bind_rows( ipeds_g2_d_10 ) %>%
        arrange( LINE )        

g22_a_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_,
                 Filler=NA_character_,ADEXCL=NA_character_,
                 COMPY4=NA_character_,STILLENROLLED=NA_character_ )

ipeds_g22_a <- ipeds_terms %>%
    filter( IPEDS_Report == 200 ) %>%
    select( ID, Cohort_Start_Date, Term_ID, Term_Start_Date, Enrolled_IPEDS ) %>%
    distinct() %>%
    left_join( ipeds_terms_grads ) %>%
    mutate( Status_Type = case_when(
        Graduation_Date <= Term_Start_Date ~ case_when(
            Grad_Time %in% c('100') ~ 'COMPY4_100',
            Grad_Time %in% c('150') ~ 'COMPY4_150',
            Grad_Time %in% c('200') ~ 'COMPY4',
            TRUE ~ 'ZZZZ99'
        ),
        Enrolled_IPEDS > 0 ~ 'STILLENROLLED',
        TRUE ~ 'ZZZZ99'
    )
    ) %>%
    group_by( ID ) %>%
    summarise( IPEDS_Status = min(Status_Type) ) %>%
    group_by( IPEDS_Status ) %>%
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    filter( IPEDS_Status != 'ZZZZ99' ) %>%
    spread( IPEDS_Status, Students, fill="000000" ) %>%
    mutate( UNITID = '198668',
            SURVSECT = 'G22',
            PART = 'A',
            Filler = '      ',
            ADEXCL = '000000' ) %>% # TODO: Need to find exclusions next time!
    add_column( !!!g22_a_cols[!names(g22_a_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(g22_a_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    select( !! names(g22_a_cols) )
    # select_( .dots=names(g22_a_cols) )

## Write out the Graduation Rate tables for importing into IPEDS
if (WRITE_OUTPUT) {
    write.table( data.frame(ipeds_g2_b), file.path(output_path, fn_G2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
    write.table( data.frame(ipeds_g2_c), file.path(output_path, fn_G2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_g2_d), file.path(output_path, fn_G2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_g22_a), file.path(output_path, fn_G22), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
}
