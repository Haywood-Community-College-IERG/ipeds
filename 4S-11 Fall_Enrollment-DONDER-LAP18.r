###
### Define GLOBAL USER variables
###

fn_report_code <- "ef2"
report_year_data_adjustment <- 0

# OVERRIDE_REPORT_YEAR <- 2019 # Set to NA for current academic year's fall term

WRITE_OUTPUT <- TRUE
CLEANUP <- FALSE
TEST <- FALSE

# TODO: Faculty/Student Ratio

ir_root <- "L:/IERG"

###
### End GLOBAL USER variables
###

nsc_path <- file.path(ir_root, "Data", "NSC")
ipeds_path <- file.path(ir_root, "Data", "IPEDS")

require(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
require(magrittr) # pipes
require(stringr) # string manipulation
require(lubridate)
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

report_year_data_start <- report_year
#report_year_data_end <- report_year - (dplyr::if_else( include_current_ay, 0, 1 ))

report_term <- str_c(report_year, "FA")

report_term_FTFT <- str_c(report_year, "FT")
report_term_FTTR <- str_c(report_year, "TF")
report_term_PTFT <- str_c(report_year, "PT")
report_term_PTTR <- str_c(report_year, "TP")

report_term_retention_FTFT <- str_c(report_year-1, "FT")
report_term_retention_PTFT <- str_c(report_year-1, "PT")

report_year_date <- ymd( str_c(report_year, 10, 15) )

project_path <- file.path(ir_root,"Reporting","IPEDS",report_year_folder,"R")
input_path <- file.path(project_path, "input")
output_path <- file.path(project_path, "output")

fn_EF2 <- stringr::str_c("ipeds_",report_year + report_year_data_adjustment,"_",fn_report_code,"_",report_time_str,".txt")

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
            Term_Reporting_Year = Reporting_Year_FSS,
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
    select( Campus_ID = ID, Term_ID, Cohort, Term_Cohort ) %>%
    collect() %>%
    inner_join( terms %>% select(Term_ID,Cohort_Year=Term_Reporting_Year,Cohort_Start_Date=Term_Start_Date) ) %>%
    filter( Cohort_Year %in% c(report_year, report_year - 1) )

ipeds_cohorts_ids <- ipeds_cohorts %>%
    select(Campus_ID) %>%
    distinct()

ipeds_cohorts_ly_ft <- ipeds_cohorts %>%
    filter( Cohort_Year == (report_year - 1),
            str_detect(Cohort,"T$") )

#
# Get Person data from DB for all report students
#
person <- getColleagueData( "PERSON" ) %>%
    filter( FIRST.NAME != "" ) %>%
    select( Campus_ID = ID, 
            First_Name = FIRST.NAME, 
            Last_Name = LAST.NAME, 
            Birth_Date = BIRTH.DATE,
            State = STATE,
            Residence_State = RESIDENCE.STATE,
            Gender = GENDER, 
            ETHNIC, 
            CITIZENSHIP, 
            Country = RESIDENCE.COUNTRY, 
            VISA.TYPE,
            PER.ETHNICS, 
            PER.RACES, 
            X.ETHNICS.RACES ) %>%
    collect() %>%
    mutate( CITIZENSHIP = if_else( CITIZENSHIP == "USA", "", CITIZENSHIP ),
            Age = floor(interval(start = Birth_Date, end = report_year_date) /
                            duration(num = 1, units = "years")),
            State = if_else(State != "NC", Residence_State, State) ) %>%
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
                X.ETHNICS.RACES == "FOR" & Gender == "M" ~ "EFRACE01",
                X.ETHNICS.RACES == "FOR" & Gender == "F" ~ "EFRACE02",
                X.ETHNICS.RACES == "HIS" & Gender == "M" ~ "EFRACE25",
                X.ETHNICS.RACES == "HIS" & Gender == "F" ~ "EFRACE26",
                X.ETHNICS.RACES == "MULTI" & Gender == "M" ~ "EFRACE37",
                X.ETHNICS.RACES == "MULTI" & Gender == "F" ~ "EFRACE38",
                X.ETHNICS.RACES == "AN" & Gender == "M" ~ "EFRACE27",
                X.ETHNICS.RACES == "AN" & Gender == "F" ~ "EFRACE28",
                X.ETHNICS.RACES == "AS" & Gender == "M" ~ "EFRACE29",
                X.ETHNICS.RACES == "AS" & Gender == "F" ~ "EFRACE30",
                X.ETHNICS.RACES == "BL" & Gender == "M" ~ "EFRACE31",
                X.ETHNICS.RACES == "BL" & Gender == "F" ~ "EFRACE32",
                X.ETHNICS.RACES == "HP" & Gender == "M" ~ "EFRACE33",
                X.ETHNICS.RACES == "HP" & Gender == "F" ~ "EFRACE34",
                X.ETHNICS.RACES == "WH" & Gender == "M" ~ "EFRACE35",
                X.ETHNICS.RACES == "WH" & Gender == "F" ~ "EFRACE36",
                TRUE ~ if_else( Gender == "M", "EFRACE13", "EFRACE14" )
            ),
            Age_Category = case_when(
                Age <  18 ~ "01",
                Age <= 19 ~ "02",
                Age <= 21 ~ "03",
                Age <= 24 ~ "04",
                Age <= 29 ~ "05",
                Age <= 34 ~ "06",
                Age <= 39 ~ "07",
                Age <= 49 ~ "08",
                Age <= 64 ~ "09",
                Age >= 65 ~ "10",
                TRUE ~ "XX"
            ),
            State_Code = case_when(
                State == "AL" ~ "01", State == "AK" ~ "02", State == "AZ" ~ "04", State == "AR" ~ "05", State == "CA" ~ "06",
                State == "CO" ~ "08", State == "CT" ~ "09", State == "DE" ~ "10", State == "DC" ~ "11", State == "FL" ~ "12",
                State == "GA" ~ "13", State == "HI" ~ "15", State == "ID" ~ "16", State == "IL" ~ "17", State == "IN" ~ "18",
                State == "IA" ~ "19", State == "KS" ~ "20", State == "KY" ~ "21", State == "LA" ~ "22", State == "ME" ~ "23",
                State == "MD" ~ "24", State == "MA" ~ "25", State == "MI" ~ "26", State == "MN" ~ "27", State == "MO" ~ "29",
                State == "MT" ~ "30", State == "NE" ~ "31", State == "NV" ~ "32", State == "NH" ~ "33", State == "NJ" ~ "34",
                State == "NM" ~ "35", State == "NY" ~ "36", State == "NC" ~ "37", State == "ND" ~ "38", State == "OH" ~ "39",
                State == "OK" ~ "40", State == "OR" ~ "41", State == "PA" ~ "42", State == "RI" ~ "44", State == "SC" ~ "45",
                State == "TN" ~ "47", State == "TX" ~ "48", State == "UT" ~ "49", State == "VT" ~ "50", State == "VA" ~ "51",
                State == "WA" ~ "53", State == "WV" ~ "54", State == "WI" ~ "55", State == "WY" ~ "56", 
                
                Country == "AS" ~ "60", Country == "FM" ~ "64", Country == "GU" ~ "66", Country == "MH" ~ "68",
                Country == "MP" ~ "69", Country == "PW" ~ "70", Country == "PR" ~ "72", Country == "VI" ~ "78",
                coalesce(Country,"") != "" ~ "90",

                TRUE ~ "57"
            )) %>%
    select( Campus_ID, First_Name, Last_Name, Age, Age_Category, State, Country, State_Code, Gender, IPEDS_Race, IPEDS_Race_Code )

student_enrollment_all_terms <- term_enrollment() %>%
    rename( Campus_ID = ID )

#
# Get the fall enrolled students
#
fall_students <- fall_enrollment( report_year ) %>%
    rename( Campus_ID = ID ) %>%
    filter( Enrollment_Status != "Withdrawn" )

#
# Get the fall enrolled students
# HS students are not considered credential seekers by IPEDS
#
credential_students <- fall_credential_seekers( report_year, exclude_hs = TRUE ) %>%
    rename( Campus_ID = ID )
    
# institutions_attend <- high_school_graduation_dates()
institutions_attend <- getColleagueData( "INSTITUTIONS_ATTEND" ) %>%
    
    # Keep HS graduation records
    filter( INSTA.INST.TYPE == "HS",
            INSTA.GRAD.TYPE == "Y" ) %>%
    select( Campus_ID=INSTA.PERSON.ID,
            Institution_Name=X.INSTA.INSTITUTION,
            End_Dates=INSTA.END.DATES
    ) %>%
    collect() %>%

    # Filter down the list of students
    inner_join( ipeds_cohorts_ids ) %>%
    
    # For some reason, there are some records with multiple dates, keep the earliest one
    mutate( End_Date = strsplit(End_Dates,", ") ) %>%
    unnest( End_Date ) %>%
    select( -End_Dates ) %>%
    filter( !is.na(End_Date) ) %>%
    mutate( End_Date = ymd(End_Date) ) %>%
    group_by( Campus_ID ) %>% 
    summarize( HS_Grad_Date = min(End_Date) ) %>%
    ungroup() %>%
    distinct()

non_credential_seekers_ft <- fall_students %>%
    anti_join( credential_students ) %>%
    select( Campus_ID ) %>%
    left_join( student_enrollment_all_terms ) %>%
    select( Campus_ID, Term_ID ) %>%
    left_join( terms %>% select(Term_ID,Term_Start_Date) ) %>%
    group_by( Campus_ID ) %>%
    summarise( Term_Start_Date = min(Term_Start_Date) ) %>%
    mutate( First_Term = 1 )

non_credential_seekers <- fall_students %>%
    anti_join( credential_students ) %>%
    left_join( terms %>% select(Term_ID,Term_Start_Date) ) %>%
    left_join( non_credential_seekers_ft ) %>%
    mutate( Non_Degree_First_Term = coalesce(First_Term,0) ) %>%
    select( Campus_ID, Non_Degree_First_Term )

fall_students_with_credentials <- fall_students %>%
    left_join( ipeds_cohorts ) %>%
    left_join( credential_students ) %>%
    mutate( Credential_Seeker = coalesce(Credential_Seeker,0) ) %>%
    left_join( non_credential_seekers ) %>%
    mutate( Non_Degree_First_Term = coalesce(Non_Degree_First_Term,0) ) %>%
    left_join( person ) %>%
    left_join( institutions_attend ) %>%
    mutate( Student_Level_Cohort = case_when(
                is.na(Cohort) & Non_Degree_First_Term == 1 ~ "First-time",
                is.na(Cohort) ~ "Continuing",
                Cohort %in% c(report_term_FTFT, report_term_PTFT) ~ "First-time",
                Cohort %in% c(report_term_FTTR, report_term_PTTR) ~ "Transfer",
                TRUE ~ "Unknown"
            ),
            HS_Grad_Diff = coalesce(as.integer(Cohort_Start_Date - HS_Grad_Date),0) ) %>%
    mutate( Student_Level_A = case_when(
                (Status == "FT") & (Student_Level_Cohort == "First-time") & (Credential_Seeker == 1) ~ "01",
                (Status == "FT") & (Student_Level_Cohort == "Transfer") & (Credential_Seeker == 1) ~ "02",
                (Status == "FT") & (Student_Level_Cohort == "Continuing") & (Credential_Seeker == 1) ~ "03",
                (Status == "FT") & (Credential_Seeker == 0) ~ "07",
                
                (Status == "PT") & (Student_Level_Cohort == "First-time") & (Credential_Seeker == 1) ~ "15",
                (Status == "PT") & (Student_Level_Cohort == "Transfer") & (Credential_Seeker == 1) ~ "16",
                (Status == "PT") & (Student_Level_Cohort == "Continuing") & (Credential_Seeker == 1) ~ "17",
                (Status == "PT") & (Credential_Seeker == 0) ~ "21",
                
                TRUE ~ "99" ),
            Student_Level_G = if_else( Credential_Seeker == 1, "1", "2" ),
            Student_Level_B = "1",
            Distance_Courses = case_when(
                Distance_Courses == "None" ~ "NOTENROLL",
                Distance_Courses == "At least 1" ~ "ENROLL_SOME",
                Distance_Courses == "All" ~ "ENROLL_EXCLUSIVE",
                TRUE ~ "Unknown"
            ),
            Within_1Yr = if_else((HS_Grad_Diff > 0) & (HS_Grad_Diff < 366),1,0)
        ) #%>%
    # mutate( State_Code = coalesce(State_Code,"98"),
    #         Gender = coalesce(Gender,'U'),
    #         IPEDS_Race_Code = coalesce(IPEDS_Race_Code,EFRACE13) )

retention_students <- fall_enrollment( report_year-1 ) %>%
    rename( Campus_ID = ID ) %>%
    inner_join( ipeds_cohorts_ly_ft )


acad_credentials <- getColleagueData( "ACAD_CREDENTIALS" ) %>%
    filter( ACAD.INSTITUTIONS.ID == "0019844",
            # ACAD.END.DATE > report_term_start_date || ACAD.CAST.DATE > report_term_start_date,
            !(ACAD.ACAD.PROGRAM %in% c("AHS", "=GED", "HSEGED")) ) %>%
    select( Campus_ID = ACAD.PERSON.ID,
            Term_ID = ACAD.TERM, 
            Program = ACAD.ACAD.PROGRAM,
            End_Date = ACAD.END.DATE,
            Cast_Date = ACAD.CAST.DATE
    ) %>%
    collect() %>%
    mutate( Cast_Date = as.Date(Cast_Date) ) %>%
    
    # Reduce the credentials to those in the retention cohort
    inner_join( retention_students %>% select(Campus_ID,Cohort_Start_Date) ) %>%
    
    # Get term date information to compare to credential date
    left_join( terms %>% select(Term_ID, Term_End_Date ) ) %>%
    mutate( Graduation_Date = if_else(Cast_Date < Term_End_Date, Term_End_Date, Cast_Date),
            Graduated = 1 ) %>%
    
    # Keep any credentials earned after the cohort date
    filter( Term_End_Date >= Cohort_Start_Date ) %>%
    select( Campus_ID, Graduated ) %>%
    distinct()

retention <- retention_students %>%
    
    # Fix Status to be the status of their cohort, not the current term
    mutate( Status = substring(Cohort,5,6) ) %>%
    
    # Get graduation information
    left_join( acad_credentials ) %>%
    
    # Set all null values of Graduated to 0
    mutate( Graduated = coalesce(Graduated,0) ) %>%
    
    # Now, bring in credits from current fall term
    left_join( fall_students %>% select(Campus_ID,NY_Credits=Credits) ) %>%
    mutate( NY_Credits = coalesce(NY_Credits,0) ) %>%
        
    # Calculate retention
    mutate( Retained = if_else(NY_Credits+Graduated>0,1,0) )


if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( fall_students, retention_students, institutions_attend, acad_credentials, person )
}

#
# Part A is Fall Enrollment by Student Level, Attendance Status, Race/Ethnicity, and Gender
#
race_cols <- c( EFRACE01=NA_character_, EFRACE02=NA_character_,
                EFRACE25=NA_character_, EFRACE26=NA_character_,
                EFRACE27=NA_character_, EFRACE28=NA_character_,
                EFRACE29=NA_character_, EFRACE30=NA_character_,
                EFRACE31=NA_character_, EFRACE32=NA_character_,
                EFRACE33=NA_character_, EFRACE34=NA_character_,
                EFRACE35=NA_character_, EFRACE36=NA_character_,
                EFRACE37=NA_character_, EFRACE38=NA_character_,
                EFRACE13=NA_character_, EFRACE14=NA_character_ )
ef2_a_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, CIPCODE=NA_character_,
                 LINE=NA_character_, race_cols)

ipeds_ef2_a <- fall_students_with_credentials %>%
    
    # Grouping by the Student Level as defined by IPEDS
    group_by( Student_Level_A, IPEDS_Race_Code ) %>%
    
    # Add a dummy variable for summing
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    
    # Flip the data frame to run Race across columns
    spread( IPEDS_Race_Code, Students, fill="000000" ) %>%
    
    # Now add any missing Race columns and make them 0
    add_column( !!!race_cols[!names(race_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(race_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    
    # Now rename the Student Level column as per Import Specs
    rename( LINE = Student_Level_A ) %>%
    
    # Add additional columns as per Import Specs
    mutate( UNITID = '198668',
            SURVSECT = 'EF2',
            PART = 'A',
            CIPCODE = "99.0000" ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( all_of(names(ef2_a_cols)) ) %>%
    
    # Sort the final data by the Student Level to make it easier to debug
    arrange( as.integer(LINE) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( race_cols, ef2_a_cols )
}


#
# Part G is Distance Education Status
#
distance_cols <- c( ENROLL_EXCLUSIVE=NA_character_, ENROLL_SOME=NA_character_ )
distance_filler <- c( NOTENROLL=NA_character_, TOTAL=NA_character_ )
inus_cols <- c( INUS_PPS=NA_character_, INUS_NOTPPS=NA_character_, INUS_UNKNOWN_STATE=NA_character_, OUTSIDEUS=NA_character_ )

ef2_g_dc_cols <- c( LINE=NA_character_, distance_cols)
ef2_g_dce_cols <- c( LINE=NA_character_, inus_cols)

ef2_g_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, LINE=NA_character_, 
                 distance_cols, distance_filler, inus_cols )

# First, get the students by distance enrollment type
ipeds_ef2_g_dc <- fall_students_with_credentials %>%
    
    # Filter to report year which is previous fall term
    # Drop the Not Enrolled, that is calculated
    filter( Distance_Courses != "NOTENROLL" ) %>%
    
    # Grouping by the Student Level as defined by IPEDS
    group_by( Student_Level_G, Distance_Courses ) %>%
    
    # Add a dummy variable for summing
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%

    # Flip the data frame to run Race across columns
    spread( Distance_Courses, Students, fill="000000" ) %>%
    
    # Now add any missing Race columns and make them 0
    add_column( !!!distance_cols[!names(distance_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(distance_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    
    # Now rename the Student Level column as per Import Specs
    rename( LINE = Student_Level_G ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( all_of(names(ef2_g_dc_cols)) )
    
# Then get the students who are enrolled exclusively online    
ipeds_ef2_g_dce <- fall_students_with_credentials %>%
    
    # Filter to report year which is previous fall term
    filter( Distance_Courses == "ENROLL_EXCLUSIVE" ) %>%
    
    # Create INUS and OUTSIDEUS fields
    mutate( INUS_PPS = if_else(State_Code == "37", 1, 0),
            INUS_NOTPPS = if_else(State_Code != "37" & as.integer(State_Code) < 57, 1, 0),
            INUS_UNKNOWN_STATE = if_else(State_Code == "57", 1, 0),
            OUTSIDEUS = if_else(as.integer(State_Code) > 57, 1, 0) ) %>%
    
    # Grouping by the Student Level as defined by IPEDS
    group_by( Student_Level_G ) %>%
    
    # Add a dummy variable for summing
    mutate( student = 1 ) %>%
    summarise( INUS_PPS = sprintf("%06d", sum(INUS_PPS) ),
               INUS_NOTPPS = sprintf("%06d", sum(INUS_NOTPPS) ),
               INUS_UNKNOWN_STATE = sprintf("%06d", sum(INUS_UNKNOWN_STATE) ),
               OUTSIDEUS = sprintf("%06d", sum(OUTSIDEUS) ) ) %>%
    
    # Now rename the Student Level column as per Import Specs
    rename( LINE = Student_Level_G ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( all_of(names(ef2_g_dce_cols)) )

# Now put the final table together
ipeds_ef2_g <- ipeds_ef2_g_dc %>% 
    left_join( ipeds_ef2_g_dce ) %>%

        # Add additional columns as per Import Specs
    mutate( UNITID = '198668',
            SURVSECT = 'EF2',
            PART = 'G',
            NOTENROLL = '      ',
            TOTAL = '      ' ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( all_of(names(ef2_g_cols)) ) %>%
    
    # Sort the final data by the Student Level to make it easier to debug
    arrange( as.integer(LINE) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( distance_cols, inus_cols, ef2_g_dc_cols, ef2_g_dce_cols, ef2_g_cols,
        ipeds_ef2_g_dc, ipeds_ef2_g_dce )
}   


#
# Part B is Fall Enrollment by Age
#     (This is optional in this collection - IPEDS2019)
#
age_cols <- c( EFAGE01=NA_character_, EFAGE02=NA_character_ )
ef2_b_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, SLEVEL=NA_character_, Filler=NA_character_,
                 LINE=NA_character_, age_cols)

ipeds_ef2_b <- fall_students_with_credentials %>%
    
    # Fix the Gender to use the IPEDS field names
    # Create Status_Age_Category as per IPEDS definition
    mutate( Gender = case_when(Gender=="M" ~ "EFAGE01", Gender=="F" ~ "EFAGE02", TRUE ~ "Unknown"),
            Status_Age_Category = as.character(if_else(Status=="FT",Age_Category,as.character(as.integer(Age_Category)+12))) ) %>%
    
    # Grouping by the Student Level as defined by IPEDS
    group_by( Status_Age_Category, Gender ) %>%
    
    # Add a dummy variable for summing
    mutate( student = 1 ) %>%
    summarise( Students = sprintf("%06d", sum(student) ) ) %>%
    
    # Flip the data frame to run Race across columns
    spread( Gender, Students, fill="000000" ) %>%
    
    # Now add any missing Race columns and make them 0
    add_column( !!!age_cols[!names(age_cols) %in% names(.)] ) %>%
    mutate_at(.vars=names(age_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    
    # Now rename the Student Level column as per Import Specs
    rename( LINE = Status_Age_Category ) %>%
    
    # Add additional columns as per Import Specs
    mutate( UNITID = '198668',
            SURVSECT = 'EF2',
            PART = 'B',
            SLEVEL = "1",
            Filler = '      ' ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( all_of(names(ef2_b_cols)) ) %>%
    
    # Sort the final data by the Student Level to make it easier to debug
    arrange( as.integer(LINE) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( age_cols, ef2_b_cols )
}


#
# Part C is Residence of First-Time Certificate-Seeking Undergraduate Students
#     (This is mandatory in this collection - IPEDS2019)
#
res_cols <- c( EFRES01=NA_character_, EFRES02=NA_character_ )
ef2_c_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, Filler=NA_character_, 
                 LINE=NA_character_, res_cols)

ipeds_ef2_c <- fall_students_with_credentials %>%
    
    # Filter to report year which is previous fall term
    filter( Student_Level_Cohort == "First-time",
            Credential_Seeker == 1 ) %>%
    
    group_by( State_Code ) %>%
    
    summarise( EFRES01 = sprintf("%06d", n()),
               EFRES02 = sprintf("%06d", sum(Within_1Yr)) ) %>%
    
    # Now rename the Student Level column as per Import Specs
    rename( LINE = State_Code ) %>%
    
    # Add additional columns as per Import Specs
    mutate( UNITID = '198668',
            SURVSECT = 'EF2',
            PART = 'C',
            Filler = '       ' ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( all_of(names(ef2_c_cols)) ) %>%
    
    # Sort the final data by the Student Level to make it easier to debug
    arrange( as.integer(LINE) )

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( res_cols, ef2_c_cols )
}


#
# Part D is Total Undergraduate Entering Class
#
ef2_d_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, Filler=NA_character_,
                 EFE01=NA_character_)

ipeds_ef2_d <- fall_students_with_credentials %>%
    
    # Filter to report year which is previous fall term
    filter( Student_Level_Cohort == "First-time",
            Credential_Seeker == 0 ) %>%
    
    summarise( EFE01 = sprintf("%06d", n()) ) %>%
    
    # Add additional columns as per Import Specs
    mutate( UNITID = '198668',
            SURVSECT = 'EF2',
            PART = 'D',
            Filler = '         ' ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( all_of(names(ef2_d_cols)) ) 

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( ef2_d_cols )
}


#
# Part E is Retention of First-Time Degree/Certificate-Seeking Undergraduate Students
#
ef2_e_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, Filler=NA_character_,
                 FT_PY_COHORT=NA_character_, FT_EXCLUSIONS=NA_character_, FT_INCLUSIONS=NA_character_, 
                    FT_ADJUSTED_COHORT=NA_character_, FT_CY_COHORT=NA_character_, RET_PCF=NA_character_,
                 PT_PY_COHORT=NA_character_, PT_EXCLUSIONS=NA_character_, PT_INCLUSIONS=NA_character_, 
                    PT_ADJUSTED_COHORT=NA_character_, PT_CY_COHORT=NA_character_, RET_PCP=NA_character_ )

ipeds_ef2_e <- retention %>%

    #mutate( Junk = 1 ) %>%
    group_by( Status ) %>%

    summarise( PY_COHORT = sprintf("%06d", n()),
               CY_COHORT = sprintf("%06d", sum(Retained)) ) %>%
    
    gather( temp, COHORT, -Status ) %>%
    unite( temp1, Status, temp, sep="_" ) %>%

    # Flip the data frame to run Race across columns
    spread( temp1, COHORT, fill="000000" ) %>%
    
    # Add additional columns as per Import Specs
    mutate( UNITID = "198668",
            SURVSECT = "EF2",
            PART = 'E',
            Filler = "         ",
            FT_EXCLUSIONS="000000",
            FT_INCLUSIONS="000000",
            FT_ADJUSTED_COHORT="      ",
            RET_PCF="   ",
            PT_EXCLUSIONS="000000",
            PT_INCLUSIONS="000000",
            PT_ADJUSTED_COHORT="      ",
            RET_PCP = "   " ) %>%
    
    # Now add any missing Race columns and make them 0
    #add_column( !!!ef2_e_cols[!names(ef2_e_cols) %in% names(.)] ) %>%
    #mutate_at(.vars=names(ef2_e_cols), function(x) {return( coalesce(x,"000000") )} ) %>%
    
    # Reorder the columns to be in the proper order as per Import Specs
    select( all_of(names(ef2_e_cols)) ) 

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( ef2_e_cols )
}

#
# Part F is Student-to-Faculty Ratio
#
ef2_f_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, ST_STAFF_RATIO=NA_character_ )

if (report_year==2019) {
    ipeds_ef2_f <- data.frame( UNITID = '198668',
                               SURVSECT = 'EF2',
                               PART = 'F',
                               ST_STAFF_RATIO="000000" )
} else if (report_year==2018) {
    ipeds_ef2_f <- data.frame( UNITID = '198668',
                               SURVSECT = 'EF2',
                               PART = 'F',
                               ST_STAFF_RATIO="000013" )
} else if (report_year==2017) {
    ipeds_ef2_f <- data.frame( UNITID = '198668',
                               SURVSECT = 'EF2',
                               PART = 'F',
                               ST_STAFF_RATIO="000012" )
} else {
    ipeds_ef2_f <- data.frame( UNITID = '198668',
                               SURVSECT = 'EF2',
                               PART = 'F',
                               ST_STAFF_RATIO="000000" )
}

if (CLEANUP) {
    # Cleanup by removing the extra data frames
    rm( ef2_f_cols )
}

#
# Now, write all the parts out a flat file for import into IPEDS
#

if (WRITE_OUTPUT) {
    write.table( data.frame(ipeds_ef2_a), file.path(output_path, fn_EF2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
    write.table( data.frame(ipeds_ef2_g), file.path(output_path, fn_EF2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_ef2_b), file.path(output_path, fn_EF2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_ef2_c), file.path(output_path, fn_EF2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_ef2_d), file.path(output_path, fn_EF2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_ef2_e), file.path(output_path, fn_EF2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    write.table( data.frame(ipeds_ef2_f), file.path(output_path, fn_EF2), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
}

if (TEST) {
    write_csv( fall_students_with_credentials, file.path(output_path, str_c("students_",report_year,".csv")))
    write_csv( retention_students, file.path(output_path, str_c("retention_",report_year,".csv")))
    write_csv( ipeds_ef2_a, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"a",sep="_"),".csv")) )
    write_csv( ipeds_ef2_b, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"b",sep="_"),".csv")) )
    write_csv( ipeds_ef2_c, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"c",sep="_"),".csv")) )
    write_csv( ipeds_ef2_d, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"d",sep="_"),".csv")) )
    write_csv( ipeds_ef2_e, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"e",sep="_"),".csv")) )
    write_csv( ipeds_ef2_f, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"f",sep="_"),".csv")) )
    write_csv( ipeds_ef2_g, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"g",sep="_"),".csv")) )
}
