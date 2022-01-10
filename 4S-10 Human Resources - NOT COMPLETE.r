###
### Define GLOBAL USER variables
###

fn_report_code <- "hr1"
report_year_data_adjustment <- 0
report_cutoff_day <- 1
report_cutoff_month <- 11

#OVERRIDE_REPORT_YEAR <- 2017 # Comment for current academic year's fall term

WRITE_OUTPUT <- TRUE
CLEANUP <- TRUE
#TEST <- TRUE

ir_root <- "L:/IERG"

###
### End GLOBAL USER variables
###

nsc_path <- file.path(ir_root, "Data", "NSC")
ipeds_path <- file.path(ir_root, "Data", "IPEDS")

package_date <- "2020-01-01" # date of the CRAN snapshot that the checkpoint package uses

# if checkpoint is not yet installed, install it (for people using this
# system for the first time)
if (!require(checkpoint)) {
    install.packages("checkpoint")
    require(checkpoint)
}

# install packages for the specified CRAN snapshot date
checkpoint(snapshotDate = package_date,
          checkpointLocation = ir_root,
          verbose = T,
          scanForPackages = T,
          use.knitr = F)

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

todays_datetime <- lubridate::now()
todays_date <- as.Date(todays_datetime)

# For file names
report_time_str <- format(todays_datetime,"%Y%m%d_%H%M")

report_year <- as.numeric(format(todays_datetime, "%Y"))

# Set report_month to actual month to allow adjustment of report_year based on month
# Otherwise, set to -1
report_month <- as.numeric(format(todays_datetime, "%m"))

# Fix report year to be the year of the Fall term
report_year <- report_year - if_else(report_month < 7, 1, 0)
report_year_folder <- str_c(report_year,as.integer(substring(report_year,3,4))+1,sep='-')

report_year <- ifelse(!is.na(OVERRIDE_REPORT_YEAR),OVERRIDE_REPORT_YEAR,report_year - report_year_data_adjustment)

# We get one additional year if we need change from last year
report_year_data_start <- report_year
#report_year_data_end <- report_year - (dplyr::if_else( include_current_ay, 0, 1 ))

report_term <- str_c(report_year, "FA")
report_cohort <- str_c(report_year, "FT")

hripeds_credit_categ_list <- c("C","N")
hripeds_credit_categ_rules <- c("CREDIT","NONCRED")
hripeds_default_credit_categ <- "" # Default Credit Category
hripeds_faculty_class <- c("2") # Faculty Classification
hripeds_ft_statuses <- c("FT")
hripeds_meds_values <- ""
hripeds_medical_ind <- "N"
hripeds_nh_end_day <- 31 # New Hire End Day
hripeds_nh_end_month <- 10 # New Hire End Month
hripeds_nh_start_day <- 1 # New Hire Start Day
hripeds_nh_start_month <- 11 # New Hire Start Month
hripeds_pos_medical <- ""
hripeds_pt_statuses <- c("PT","PTB","PTT")
hripeds_tenure_system <- "N"

report_year_date <- ymd( str_c(report_year, report_cutoff_month, report_cutoff_day, sep='-') )
new_hire_start_date <- ymd( str_c(report_year-1, hripeds_nh_start_month, hripeds_nh_start_day, sep='-') )
new_hire_end_date <- ymd( str_c(report_year, hripeds_nh_end_month, hripeds_nh_end_day, sep='-') )
courses_start_date <- ymd( str_c(report_year-1, report_cutoff_month, report_cutoff_day, sep='-') )
courses_end_date <- ymd( str_c(report_year, report_cutoff_month, report_cutoff_day, sep='-') ) - days(1)

validation_tables <- c("ALIEN.STATUSES",
                       "EEO.RPT.RANKS",
                       "HR.STATUSES",
                       "IPEDS.CONTRACT.TYPES",
                       "IPEDS.FUNCTIONS",
                       "IPEDS.OCCUP.CATEGS",
                       "PERSON.ETHNICS",
                       "PERSON.RACES",
                       "TENURE.TYPES",
                       "TIME.UNITS")

#project_path <- file.path(ir_root,"Reporting","IPEDS","R")
project_path <- file.path(ir_root,"Reporting","IPEDS",report_year_folder,"R")
#input_path <- file.path(project_path, "input")
output_path <- file.path(project_path, "output")

fn_output <- str_c("ipeds_",report_year + report_year_data_adjustment,"_",fn_report_code,"_",report_time_str,".txt")


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

valcodes_vals <- getColleagueData( "META__ALL_VALCODES__VALS", version="history" ) %>%
    filter( VALCODE.ID %in% validation_tables ) %>%
    select( VALCODE.ID,
            VAL.INTERNAL.CODE,
            VAL.EXTERNAL.REPRESENTATION,
            VAL.ACTION.CODE.1,
            EffectiveDatetime ) %>%
    collect() %>%
    arrange( VALCODE.ID, VAL.INTERNAL.CODE )

valcodes <- getColleagueData( "META__ALL_VALCODES" ) %>%
    filter( VALCODE.ID %in% validation_tables ) %>%
    select( VALCODE.ID, EffectiveDatetime ) %>%
    collect() %>%
    inner_join( valcodes_vals, by = c("VALCODE.ID","EffectiveDatetime") ) %>%
    distinct() %>%
    select( -EffectiveDatetime ) %>%
    arrange( VALCODE.ID, VAL.INTERNAL.CODE )

depts <- getColleagueData( "DEPTS" ) %>%
    collect()

#locations <- getColleagueData( "LOCATION" ) %>%
#    collect()

foreign_person <- getColleagueData( " FOREIGN_PERSON" ) %>%
    select( HRIW.EMP.PERSON.ID = FOREIGN.PERSON.ID, FPER.ALIEN.STATUS ) %>%
    collect()

perstat <- getColleagueData( "PERSTAT" ) %>%
    filter( PERSTAT.STATUS %in% c(hripeds_ft_statuses, hripeds_pt_statuses) ) %>%
    # filter( PERSTAT.STATUS %in% c(hripeds_ft_statuses, hripeds_pt_statuses),
    #         PERSTAT.START.DATE <= report_year_date,
    #         coalesce(PERSTAT.END.DATE,report_year_date) >= report_year_date ) %>%
    select( HRIW.EMP.PERSON.ID = PERSTAT.HRP.ID, 
            HRIW.PRIMARY.POSITION = PERSTAT.PRIMARY.POS.ID,
            HRIW.IPEDS.FUNCTION = PERSTAT.IPEDS.FUNCTION, 
            HRIW.EMPLOYEE.STATUS = PERSTAT.STATUS,
            HRIW.PERSTAT.START.DATE = PERSTAT.START.DATE,
            HRIW.PERSTAT.END.DATE = PERSTAT.END.DATE,
            HRIW.GRAD.ASSIST = PERSTAT.GRAD.ASSIST,
            HRIW.TENURE.TYPE = PERSTAT.TENURE.TYPE ) %>%
    collect() %>%
    mutate( HRIW.UNIQUE.KEY.IDX = str_c(HRIW.EMP.PERSON.ID,report_year,sep='*'),
            HRIW.IPEDS.FUNCTION = na_if(HRIW.IPEDS.FUNCTION,''), 
            HRIW.GRAD.ASSIST = na_if(HRIW.GRAD.ASSIST,'') )



# I don't think I need the dates
# hrper_perpos_info <- getColleagueData( "HRPER__PERPOS_INFO", version="history" ) %>%
#     select( HRIW.EMP.PERSON.ID = HRPER.ID, 
#             HRP.PERPOS.START.DATE,
#             HRP.PERPOS.END.DATE,
#             EffectiveDatetime ) %>%
#     collect() %>%
# For HRPER,
#    inner_join( hrper_perpos_info, by = c("HRIW.EMP.PERSON.ID","EffectiveDatetime") ) %>%

hrper_status_dates <- getColleagueData( "HRPER__STATUS_DATES", version="history" ) %>%
    # filter( HRP.PERSTAT.START.DATE <= report_year_date,
    #         coalesce(HRP.PERSTAT.END.DATE,report_year_date) >= report_year_date ) %>%
    select( HRIW.EMP.PERSON.ID = HRPER.ID, 
            EffectiveDatetime, 
            HRP.PERSTAT.START.DATE,
            HRP.PERSTAT.END.DATE ) %>%
    collect()

hrper <- getColleagueData( "HRPER" ) %>%
    select( HRIW.EMP.PERSON.ID = HRPER.ID, 
            EffectiveDatetime, 
            HRIW.EMPLOYMENT.DATE = HRP.EFFECT.EMPLOY.DATE,
            HRIW.PRIMARY.POSITION = HRP.PRI.POS ) %>%
    collect() %>%
    inner_join( hrper_status_dates, by = c("HRIW.EMP.PERSON.ID","EffectiveDatetime") ) %>%
    select( -EffectiveDatetime ) %>%
    distinct()

perpos <- getColleagueData( "PERPOS" ) %>%
    # filter( PERPOS.START.DATE <= report_year_date,
    #         coalesce(PERPOS.END.DATE,report_year_date) >= report_year_date ) %>%
    select( HRIW.EMP.PERSON.ID = PERPOS.HRP.ID,
            PERPOS.ID, 
            PERPOS.START.DATE, 
            PERPOS.END.DATE,
            HRIW.PRIMARY.POSITION = POSITION.ID,
            POSITION.ID = PERPOS.POSITION.ID,
            PERPOSWG.ID = XPERPOS.CURRENT.WAGE ) %>%
    collect() %>%
    mutate( PERPOS.END.DATE.ADJ = coalesce(PERPOS.END.DATE,todays_date) ) 

perposwg <- getColleagueData( "PERPOSWG" ) %>%
    select( HRIW.EMP.PERSON.ID = PPWG.HRP.ID,
            HRIW.BASE.SALARY = PPWG.BASE.AMT,
            PPWG.CONTRACT.LENGTH, PPWG.CONTRACT.UNITS,
            HRIW.EXTRA.HOURLY.RATE = PPWG.WORK.UNIT.RATE,
            PERPOS.ID = PPWG.PERPOS.ID,
            PERPOSWG.ID,
            PPWG.YEAR.WORK.TIME.AMT, PPWG.YEAR.WORK.TIME.UNITS,
            PERPOS.START.DATE = PPWG.START.DATE, 
            PERPOS.END.DATE.ADJ = PPWG.END.DATE,
            HRIW.IPEDS.CONTRACT.TYPE = PPWG.IPEDS.CONTRACT.TYPE # Must be at the end
    ) %>%
    collect() %>%
    mutate( PERPOS.END.DATE.ADJ = coalesce(PERPOS.END.DATE.ADJ,todays_date) ) %>%
    inner_join( perpos, by = c("HRIW.EMP.PERSON.ID","PERPOSWG.ID", "PERPOS.ID","PERPOS.START.DATE","PERPOS.END.DATE.ADJ")) %>%
    # filter( PPWG.START.DATE == PERPOS.START.DATE,
    #         PPWG.END.DATE == PERPOS.END.DATE ) %>%
    select( -c(PERPOSWG.ID, PERPOS.ID)) %>%
    mutate( HRIW.BASE.SALARY = coalesce(as.double(HRIW.BASE.SALARY),0)/100 ) %>%
    mutate( HRIW.IPEDS.CONTRACT.TYPE = if_else(HRIW.IPEDS.CONTRACT.TYPE=='',NA_character_,HRIW.IPEDS.CONTRACT.TYPE),
            Calculated_Base_Pay = case_when(
                HRIW.IPEDS.CONTRACT.TYPE == "AN" ~ 
                    case_when(
                        HRIW.BASE.SALARY==0 ~ 
                            case_when(
                                PPWG.YEAR.WORK.TIME.UNITS == 'H' ~ PPWG.YEAR.WORK.TIME.AMT*HRIW.EXTRA.HOURLY.RATE,
                                TRUE ~ HRIW.BASE.SALARY
                            ),
                        TRUE ~ HRIW.BASE.SALARY
                    ),
                TRUE ~ PPWG.YEAR.WORK.TIME.AMT * HRIW.EXTRA.HOURLY.RATE ),
            Calculated_12mo_Pay = case_when(
                HRIW.IPEDS.CONTRACT.TYPE == "AN" ~ Calculated_Base_Pay,
                PPWG.CONTRACT.UNITS == 'M' ~ 2080 * HRIW.EXTRA.HOURLY.RATE,
                TRUE ~ Calculated_Base_Pay ) )

person <- getColleagueData( "PERSON" ) %>%
    select( HRIW.EMP.PERSON.ID = ID, 
            FIRST.NAME, MIDDLE.NAME, LAST.NAME,
            HRIW.ETHNICITY = X.ETHNICS.RACES, 
            HRIW.GENDER = GENDER,
            PER.RACES, PER.ETHNICS ) %>%
    filter( !is.na(FIRST.NAME) ) %>%
    collect() %>%
    inner_join( hrper %>% select(HRIW.EMP.PERSON.ID) %>% distinct(), by = "HRIW.EMP.PERSON.ID" ) %>%
    mutate( HRIW.EMP.NAME.LFM = str_c( LAST.NAME, str_c(FIRST.NAME, MIDDLE.NAME, sep=' '), sep=", " ) ) %>%
    select( -c(FIRST.NAME, MIDDLE.NAME, LAST.NAME) )

position <- getColleagueData( "POSITION" ) %>%
    select( HRIW.PRIMARY.POSITION = POSITION.ID, 
            HRIW.ACADEMIC.RANK = POS.EEO.RANK,
            HRIW.SOC.CODE = POS.SOC.CODE,
            POS.CLASS,
            #ALL.POSPAY,
            POS.DEPT ) %>%
    collect() %>%
    mutate( HRIW.SOC.CODE = if_else(HRIW.SOC.CODE=='',NA_character_,HRIW.SOC.CODE) )

occup_categs <- getColleagueData( "SOC_CODES" ) %>%
    select( HRIW.SOC.CODE = SOC.CODES.ID, 
            HRIW.IPEDS.OCCUP.CATEG = SOC.IPEDS.OCCUP.CATEG ) %>%
    collect() 

course_sec_faculty <- getColleagueData( "COURSE_SEC_FACULTY" ) %>%
    select( SEC.FACULTY = COURSE.SEC.FACULTY.ID, HRIW.EMP.PERSON.ID = CSF.FACULTY ) %>%
    collect()

course_sections__sec_faculty <- getColleagueData( "COURSE_SECTIONS__SEC_FACULTY", version="history" ) %>%
    select( COURSE.SECTIONS.ID, SEC.FACULTY, EffectiveDatetime ) %>%
    collect() %>%
    left_join( course_sec_faculty, by = "SEC.FACULTY" ) %>%
    select( -SEC.FACULTY )

course_sections_last_date <- getColleagueData( "COURSE_SECTIONS" ) %>%
    filter( SEC.END.DATE <= courses_end_date, 
            SEC.STATUS %in% c('A') ) %>%
    select( COURSE.SECTIONS.ID, SEC.END.DATE, EffectiveDatetime ) %>%
    collect() %>%
    inner_join( course_sections__sec_faculty, by = c("COURSE.SECTIONS.ID","EffectiveDatetime") ) %>%
    group_by( HRIW.EMP.PERSON.ID ) %>%
    summarise( HRIW.EXTRA.LAST.COURSE.DATE = max(SEC.END.DATE) )

course_sections <- getColleagueData( "COURSE_SECTIONS" ) %>%
    filter( SEC.START.DATE >= courses_start_date,
            SEC.END.DATE <= courses_end_date, 
            SEC.STATUS %in% c('A') ) %>%
    select( COURSE.SECTIONS.ID, SEC.CRED.TYPE, EffectiveDatetime ) %>%
    collect() %>%
    inner_join( course_sections__sec_faculty, by = c("COURSE.SECTIONS.ID","EffectiveDatetime") ) %>%
    group_by( HRIW.EMP.PERSON.ID ) %>%
    summarise( All_Classes = n(),
               Credit_Classes = sum(if_else(SEC.CRED.TYPE %in% c("IN"), 1, 0)) ) %>%
    mutate( HRIW.CREDIT.CATEG = case_when(
        All_Classes == Credit_Classes & All_Classes > 0 ~ 'C',
        Credit_Classes == 0 & All_Classes > 0 ~ 'N',
        All_Classes > 0 ~ 'B',
        TRUE ~ NA_character_
    )) %>%
    select( HRIW.EMP.PERSON.ID, HRIW.CREDIT.CATEG )

hr_ipeds_work_active <- person %>%
    left_join( hrper, by = "HRIW.EMP.PERSON.ID" ) %>%
    mutate( HRP.PERSTAT.END.DATE.ADJ = coalesce(HRP.PERSTAT.END.DATE,todays_date) ) %>%
    filter( HRP.PERSTAT.START.DATE <= report_year_date,
            HRP.PERSTAT.END.DATE.ADJ >= report_year_date ) %>%

    left_join( position, by = "HRIW.PRIMARY.POSITION" ) %>%
    
    left_join( perstat, by = c("HRIW.EMP.PERSON.ID","HRIW.PRIMARY.POSITION") ) %>%
    mutate( HRIW.PERSTAT.END.DATE.ADJ = coalesce(HRIW.PERSTAT.END.DATE,todays_date) ) %>%
    filter( #HRIW.PERSTAT.START.DATE <= report_year_date,
            #HRIW.PERSTAT.END.DATE.ADJ >= report_year_date,
            HRIW.PERSTAT.START.DATE == HRP.PERSTAT.START.DATE,
            HRIW.PERSTAT.END.DATE.ADJ == HRP.PERSTAT.END.DATE.ADJ
            ) %>%
    
    left_join( perpos, by = c("HRIW.EMP.PERSON.ID","HRIW.PRIMARY.POSITION") ) %>%
    left_join( perposwg, by = c("HRIW.EMP.PERSON.ID","HRIW.PRIMARY.POSITION") ) %>%

        filter( PPWG.START.DATE == HRP.PERSTAT.START.DATE ) %>%
    
    select( -c(PERPOS.ID,PERPOSWG.ID) ) %>%
    distinct()
    
    filter( HRIW.PERSTAT.START.DATE <= report_year_date,
            coalesce(HRIW.PERSTAT.END.DATE,report_year_date) >= report_year_date )

hr_ipeds_work <- person %>%
    left_join( hrper, by = "HRIW.EMP.PERSON.ID" ) %>%
    left_join( position, by = "HRIW.PRIMARY.POSITION" ) %>%
    left_join( perstat, by = c("HRIW.EMP.PERSON.ID","HRIW.PRIMARY.POSITION") ) %>%
    left_join( perpos, by = c("HRIW.EMP.PERSON.ID","HRIW.PRIMARY.POSITION") ) %>%
    left_join( perposwg, by = c("HRIW.EMP.PERSON.ID","HRIW.PRIMARY.POSITION") ) %>%
    left_join( occup_categs, by = "HRIW.SOC.CODE" ) %>%
    left_join( course_sections, by = "HRIW.EMP.PERSON.ID" ) %>%
    left_join( course_sections_last_date, by = "HRIW.EMP.PERSON.ID" ) %>%
    mutate( HRIW.RPT.YEAR = report_year,
            HRIW.MEDICAL.IND = case_when(
                hripeds_medical_ind == 'Y' & POS.DEPT %in% hripeds_meds_values ~ 'Y',
                TRUE ~ NA_character_ ),
            HRIW.FACULTY.STATUS.FLAG = case_when(
                POS.CLASS %in% hripeds_faculty_class ~ 'Y',
                TRUE ~ NA_character_ ),
            HRIW.PRIMARY.POSITION = if_else(HRIW.PRIMARY.POSITION=='',NA_character_,HRIW.PRIMARY.POSITION),
            HRIW.IPEDS.FUNCTION = if_else(HRIW.IPEDS.FUNCTION=='',NA_character_,HRIW.IPEDS.FUNCTION),
            HRIW.EXTRA.HOURLY.RATE = if_else(HRIW.EMPLOYEE.STATUS %in% hripeds_pt_statuses,
                                             NA_real_,
                                             as.double(HRIW.EXTRA.HOURLY.RATE) ) ) %>%
    mutate( HRIW.BASE.SALARY = if_else(HRIW.EMPLOYEE.STATUS %in% hripeds_ft_statuses, 
                                       if_else(coalesce(as.double(HRIW.BASE.SALARY),0.0)==0.0,
                                               Calculated_Base_Pay, as.double(HRIW.BASE.SALARY)/100 ),
                                       NA_real_),
            HRIW.ACADEMIC.RANK = 6,
            HRIW.CONTRACT.MONTHS = case_when(
                HRIW.IPEDS.FUNCTION %in% c("IO","IC") & HRIW.MEDICAL.IND %in% c('','N',NA_character_) ~ 
                    if_else( PPWG.CONTRACT.UNITS == 'M', as.integer(PPWG.CONTRACT.LENGTH), NA_integer_ ),
                TRUE ~ NA_integer_ ),
            HRIW.IPEDS.CONTRACT.TYPE = case_when(
                hripeds_tenure_system == 'N' ~ HRIW.IPEDS.CONTRACT.TYPE,
                hripeds_tenure_system == 'Y' & HRIW.TENURE.TYPE != 'T' ~ HRIW.IPEDS.CONTRACT.TYPE,
                TRUE ~ NA_character_ ),
            HRIW.NEW.HIRE.FLAG = case_when(
                HRIW.PERSTAT.START.DATE >= new_hire_start_date & 
                    (coalesce(HRIW.PERSTAT.END.DATE,report_year_date+1) > report_year_date) ~ 'Y',
                TRUE ~ NA_character_ ),
            HRIW.TENURE.TYPE = case_when(
                hripeds_tenure_system == 'N' ~ NA_character_,
                TRUE ~ '' ),
            HRIW.CREDIT.CATEG = if_else(HRIW.FACULTY.STATUS.FLAG == 'Y',HRIW.CREDIT.CATEG,NA_character_)
            ) %>%
    select( HRIW.EMP.PERSON.ID, HRIW.EMP.NAME.LFM,
            HRIW.ACADEMIC.RANK,
            HRIW.BASE.SALARY,
            HRIW.CONTRACT.MONTHS,
            HRIW.CREDIT.CATEG,
            HRIW.EMPLOYEE.STATUS,
            HRIW.EMPLOYMENT.DATE,
            HRIW.ETHNICITY,
            HRIW.FACULTY.STATUS.FLAG,
            HRIW.GENDER,
            HRIW.GRAD.ASSIST,
            HRIW.IPEDS.CONTRACT.TYPE,
            HRIW.IPEDS.FUNCTION,
            HRIW.IPEDS.OCCUP.CATEG,
            HRIW.MEDICAL.IND,
            HRIW.NEW.HIRE.FLAG,
            HRIW.PERSTAT.END.DATE,
            HRIW.PERSTAT.START.DATE,
            HRIW.PRIMARY.POSITION,
            HRIW.RPT.YEAR,
            HRIW.SOC.CODE,
            HRIW.TENURE.TYPE,
            HRIW.UNIQUE.KEY.IDX,
            HRIW.EXTRA.HOURLY.RATE,
            HRIW.EXTRA.LAST.COURSE.DATE,
            HRIW.EXTRA.YEAR.WORK.TIME.UNITS = PPWG.YEAR.WORK.TIME.UNITS,
            HRIW.EXTRA.YEAR.WORK.TIME.AMT = PPWG.YEAR.WORK.TIME.AMT
            ) %>%
    arrange( HRIW.EMP.PERSON.ID ) %>%
    distinct()

#####
#####
#
# VALIDATION CHECKS
#
####
####

error_checks <- hr_ipeds_work %>%
    mutate( ERROR01 = !(as.character(HRIW.ACADEMIC.RANK) %in% c(valcodes %>% filter(VALCODE.ID=='EEO.RPT.RANKS') %>% collect %$% VAL.INTERNAL.CODE) ),
            ERROR02 = (HRIW.EMPLOYEE.STATUS %in% hripeds_ft_statuses & coalesce(HRIW.BASE.SALARY,0) == 0),
            #ERROR03 = "XXXX",
            ERROR04 = (coalesce(HRIW.FACULTY.STATUS.FLAG,'N') == 'Y' & coalesce(HRIW.CREDIT.CATEG,'Z') %nin% c('C','N','B') ),
            ERROR05 = (coalesce(HRIW.IPEDS.OCCUP.CATEG,"XX") == "05" & coalesce(HRIW.IPEDS.FUNCTION,"XX") == "XX"),
            ERROR06 = (coalesce(HRIW.IPEDS.OCCUP.CATEG,"XX") != "05" & coalesce(HRIW.IPEDS.FUNCTION,"XX") != "XX"),
            ERROR07 = (coalesce(HRIW.IPEDS.OCCUP.CATEG,"XX")=="XX"),
            #ERROR08 = "XXXX",
            ERROR09 = (coalesce(HRIW.SOC.CODE,"XX-XXXX")=="XX-XXXX"),
            ERROR10 = (hripeds_tenure_system == 'Y' & (HRIW.TENURE.TYPE %in% c(valcodes %>% filter(VALCODE.ID=='TENURE.TYPES') %>% collect %$% VAL.INTERNAL.CODE)) ),
            #ERROR11 = "XXXX",
            ERROR12 = (HRIW.EMPLOYEE.STATUS %in% hripeds_ft_statuses & coalesce(HRIW.BASE.SALARY,0) == 0 & coalesce(HRIW.EXTRA.YEAR.WORK.TIME.AMT,0.0)==0.0),
            ERROR13 = (HRIW.EMPLOYEE.STATUS %in% hripeds_ft_statuses & coalesce(HRIW.BASE.SALARY,0) == 0 & (HRIW.EXTRA.YEAR.WORK.TIME.UNITS %in% c(valcodes %>% filter(VALCODE.ID=='TIME.UNITS') %>% collect %$% VAL.INTERNAL.CODE)) )
            ) %>%
    #select( HRIW.EMP.PERSON.ID, HRIW.ACADEMIC.RANK, HRIW.SOC.CODE, HRIW.TENURE.TYPE, starts_with("ERROR") ) %>%
    mutate( ERROR07 = if_else(ERROR07 & ERROR09,FALSE,ERROR07) ) %>%
    pivot_longer( starts_with("ERROR") ) %>%
    filter( value == TRUE ) %>%
    mutate( Error_Message = case_when(
        name == "ERROR01" ~ str_c("E01: Academic Rank", HRIW.ACADEMIC.RANK, "must exist in the EEO.RPT.RANKS validation table with an action code between 1 and 6.", sep=" "),
        name == "ERROR02" ~ "E02: Annual wage amount must not be equal to zero.",
        name == "ERROR03" ~ "E03: Could not determine a value for FIELD.NAME.",
        name == "ERROR04" ~ str_c("E04: Faculty member is not currently teaching any courses. Last course ended on",coalesce(as.character(HRIW.EXTRA.LAST.COURSE.DATE),"<Unknown>"),".",sep=' '),
        name == "ERROR05" ~ "E05: IPEDS Function is required if the IPEDS Occupational Category for the SOC Code is Postsecondary Teacher.",
        name == "ERROR06" ~ "E06: IPEDS Function should not be specified if the SOC Code is not a Postsecondary Teacher.",
        name == "ERROR07" ~ str_c("E07: ",coalesce(HRIW.SOC.CODE,"<unknown SOC Code>"),"is missing IPEDS Occupational Category, which could not be determined for employees with this SOC Code.", sep=" "),
        name == "ERROR08" ~ str_c("E08: PERSTAT","NNNN","cannot be processed without an Employee ID.", sep=" "),
        name == "ERROR09" ~ str_c("E09: ",HRIW.EMP.PERSON.ID,"is missing SOC Code. Unable to determine IPEDS Occupational Category for employees in this position.", sep=" "),
        name == "ERROR10" ~ str_c("E10: Tenure Type",HRIW.TENURE.TYPE,"must exist in the TENURE.TYPES validation table with an action code.", sep=" "),
        name == "ERROR11" ~ "E11: The HRIP form does not contain enough information to build the IPEDS work files. Go to the HRIP form and enter all required information.",
        name == "ERROR12" ~ "E12: Yearly Work Time must be specified with an hourly unit so that an annual pay amount may be calculated.",
        name == "ERROR13" ~ "E13: Yearly Work Time must be specified with an hourly unit so that an annual pay amount may be calculated.",
        TRUE ~ "Unknown ERROR"
    )) %>%
    select( -c(name,value) )
    group_by( HRIW.EMP.PERSON.ID ) %>%
    summarize( Error_Message = str_c( Error_Message, collapse = "; " ) )


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

ipeds_award_categories_ugaida <- unique(c(aid_federal_grant,aid_state_local,aid_institution,aid_other))

ipeds_saf_a <- sfa_group1 %>%
    
    summarise( SCFA2 = n_distinct(ID),  # IPEDS SFA Part A, Line 01
               UGAIDN = n_distinct(ID[Award_Amount > 0], na.rm = TRUE),  # IPEDS Part B, Line 01, Column 1
               UGAIDPN = n_distinct(ID[Award_Category %in% aid_federal_pell & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part B, Line 02, Column 1
               UGLNFN = n_distinct(ID[Award_Category %in% aid_federal_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part B, Line 03, Column 1
               UGAIDA = sum(Award_Amount[Award_Category %in% ipeds_award_categories_ugaida], na.rm = TRUE),  # IPEDS SFA Part B, Line 01, Column 3
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

ipeds_award_categories_2a <- unique(c(aid_federal_ws,
                                      aid_loans,
                                      aid_federal_grant,
                                      aid_state_local,
                                      aid_institution,
                                      aid_other))

ipeds_award_categories_2b <- unique(c(aid_loans,
                                      aid_federal_grant,
                                      aid_state_local,
                                      aid_institution))

ipeds_saf_b <- sfa_group2 %>%
    
    summarise( SCFA1N = n_distinct(ID),  # IPEDS SFA Part A, Line 02
               SCFA12N = n_distinct(ID[Tuition_Type == "In-State"], na.rm = TRUE),  # IPEDS SFA Part C, Page 1, Line 01b
               SCFA13N = n_distinct(ID[Tuition_Type == "Out-of-State"], na.rm = TRUE),  # IPEDS SFA Part C, Page 1, Line 01c
               
               ANYAIDN = n_distinct(ID[Award_Category %in% ipeds_award_categories_2a & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part A, Line 02a
               ANYAIDNF = n_distinct(ID[Award_Category %in% ipeds_award_categories_2b & Award_Amount >0], na.rm = TRUE)  # IPEDS SFA Part A, Line 02b
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

ipeds_award_categories_c02_q1 <- unique(c(aid_federal_grant,aid_state_local,aid_institution))

ipeds_award_categories_2a <- unique(c(aid_federal_ws,
                                      aid_loans,
                                      aid_federal_grant,
                                      aid_state_local,
                                      aid_institution,
                                      aid_other))

ipeds_award_categories_2b <- unique(c(aid_loans,
                                      aid_federal_grant,
                                      aid_state_local,
                                      aid_institution))

ipeds_saf_c <- sfa_group2 %>%
    
    summarise( AGRNT_N = n_distinct(ID[Award_Category %in% ipeds_award_categories_c02_q1 & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 01, Column 1
               FGRNT_N = n_distinct(ID[Award_Category %in% aid_federal_grant & Award_Type == 'F' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02, Column 1
               PGRNT_N = n_distinct(ID[Award_Category %in% aid_federal_pell & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02a, Column 1
               OFGRNT_N = n_distinct(ID[Award_Category %in% aid_federal_non_pell & Award_Type == 'F' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02b, Column 1
               SGRNT_N = n_distinct(ID[Award_Category %in% aid_state_local & Award_Type == 'S' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 03, Column 1
               IGRNT_N = n_distinct(ID[Award_Category %in% aid_institution & Award_Type == 'I' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 04, Column 1
               LOAN_N = n_distinct(ID[Award_Category %in% aid_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05, Column 1
               FLOAN_N = n_distinct(ID[Award_Category %in% aid_federal_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05a, Column 1
               OLOAN_N = n_distinct(ID[Award_Category %in% aid_other_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05b, Column 1
               
               PGRNT_T = sum(Award_Amount[Award_Category %in% aid_federal_pell], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02a, Column 3
               OFGRNT_T = sum(Award_Amount[Award_Category %in% aid_federal_non_pell & Award_Type == 'F'], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02b, Column 3
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
    write.table( data.frame(ipeds_hr1_a), file.path(output_path, fn_output), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
    write.table( data.frame(ipeds_hr2_b), file.path(output_path, fn_output), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
}

if (TEST) {
    #write_csv( XXXX, file.path(output_path, str_c(fn_report_code,"_group1_",report_year,".csv")), na = "")
}
