get_terms <- function( report_years = NA_integer_, report_semesters = NA_character_, ay_type = NA_character_, file_path = NA_character_ ) {

    if (is.na(ay_type)) {
        ay_type <- "FSS"
    } else {
        if (!(ay_type %in% c("FSS","SFS"))) {
            return( NA )
        }
    }

    if (is.na(file_path)) {
        if (ay_type)
        terms <- getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
            select( Term_ID,
                    Term_Index,
                    Term_Name = Semester,
                    Semester = Term_Abbreviation,
                    Term_Start_Date,
                    Term_Census_Date,
                    Term_End_Date,
                    Reporting_Year_FSS,
                    Reporting_Year_SFS,
                    Reporting_Academic_Year_FSS,
                    Reporting_Academic_Year_SFS ) %>%
            collect() %>%
            mutate( Term_Reporting_Year = case_when( 
                                              ay_year == "FSS" ~ Reporting_Year_FSS,
                                              ay_year == "SFS" ~ Reporting_Year_SFS,
                                              TRUE ~ NA_integer_ 
                                          ),
                    Academic_Year = case_when(
                                        ay_year == "FSS" ~ Reporting_Academic_Year_FSS,
                                        ay_year == "SFS" ~ Reporting_Academic_Year_SFS,
                                        TRUE ~ NA_character_ 
                                    ) ) %>%
            mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) - 1 )
    } else {
        terms_file <- file.path( file_path, "DW_TERMS.csv")
        terms <- readr::read_csv( terms_file, 
                                  col_types = cols( TERMS.ID = col_character(),
                                                    TERM.START.DATE = col_date(format = ""),
                                                    TERM.CENSUS.DATE = col_date(format = ""),
                                                    TERM.END.DATE = col_date(format = ""),
                                                    TERM.REPORTING.YEAR = col_integer(),
                                                    .default = col_character() ) ) %>%
            rename( Term_ID = TERMS.ID,
                    Term_Start_Date = TERM.START.DATE,
                    Term_End_Date = TERM.END.DATE,
                    Term_Census_Date = TERM.CENSUS.DATE,
                    Term_Reporting_Year = TERM.REPORTING.YEAR ) %>%
            mutate( Semester = substring(Term_ID, 5, 3) ) %>%
            mutate( Term_Reporting_Year = case_when(
                                              ay_year == "FSS" && Semester == "SU" ~ Term_Reporting_Year - 2,
                                              ay_year == "FSS" && Semester == "CE2" ~ Term_Reporting_Year - 2,
                                              TRUE ~ Term_Reporting_Year - 1
                                          ),
                    Term_Index_Value_FSS = case_when(
                                               Semester == "FA" ~ 1,
                                               Semester == "CE3" ~ 2,
                                               Semester == "SP" ~ 6,
                                               Semester == "CE1" ~ 7,
                                               Semester == "SU" ~ 8,
                                               Semester == "CE2" ~ 9,
                                               Semester == "CE" ~ 3,
                                               Semester == "WI" ~ 4,
                                               TRUE ~ 0
                                           ),
                    Term_Index_Value_SFS = case_when(
                                               Semester == "SU" ~ 1,
                                               Semester == "CE2" ~ 2,
                                               Semester == "FA" ~ 3,
                                               Semester == "CE3" ~ 4,
                                               Semester == "SP" ~ 8,
                                               Semester == "CE1" ~ 9,
                                               Semester == "CE" ~ 5,
                                               Semester == "WI" ~ 6,
                                               TRUE ~ 0
                                           ) ) %>%
            mutate( Term_Index = Term_Reporting_Year*10 + if_else(ay_year == "FSS", Term_Index_Value_FSS, Term_Index_Value_SFS),
                    Term_Name = case_when(
                                     Semester == "FA" ~ "Fall",
                                     Semester == "SP" ~ "Spring",
                                     Semester == "SU" ~ "Summer",
                                     Semester == "WI" ~ "Winter",
                                     Semester == "CE" ~ "Continuing Education",
                                     Semester == "CE1" ~ "Continuing Education 1",
                                     Semester == "CE2" ~ "Continuing Education 2",
                                     Semester == "CE3" ~ "Continuing Education 3",
                                     TRUE ~ "Other/Unknown"
                                 ),
                    Academic_Year = str_c( Term_Reporting_Year, "-", Term_Reporting_Year + 1 ) )

    }

    if (!is.na(report_years)) {
        if (length(report_years) == 1) {
            reporting_terms <- terms %>%
                filter( Term_Reporting_Year == report_years )
        } else {
            reporting_terms <- terms %>%
                filter( Term_Reporting_Year %in% report_years )
        }
    } else {
        reporting_terms <- terms
    }

    if (!is.na(report_semesters)) {
        if (length(report_semesters) == 1) {
            reporting_terms %<>%
                filter( Semester == report_semesters )
        } else {
            reporting_terms %<>%
                filter( Semester %in% report_semesters )
        }
    }

    return( reporting_terms )
}

#' Return enrollment for specified term as of the IPEDS reporting date of October 15
#'
#' All data comes from IERG SQL Server database
#'
#' @param report_years The ending year of the academic year of the data
#' @param report_semesters Either a single semester abbreviation or a list of semester abbreviations. If unspecified, all semesters are returned.
#' @export
#' @import magrittr
#' @import dplyr
#' @import stringr
#'
term_enrollment <- function( report_years = NA_integer_, report_semesters = NA_character_, file_path = NA_character_ ) {

    reporting_terms <- get_terms( file_path, report_years, report_semesters )


    if (is.na(file_path)) {
        # Need to get section location for distance learning courses
        # Right now, just take most recent. Probably need to do this the same way as SAC below.
        course_sections <- getColleagueData( "COURSE_SECTIONS" ) %>%
            select( Course_Section_ID = COURSE.SECTIONS.ID,
                    Term_ID = SEC.TERM,
                    Section_Location = SEC.LOCATION ) %>%
            collect()

        student_acad_cred <- getColleagueData( "STUDENT_ACAD_CRED", version="history" ) %>%
            filter( STC.ACAD.LEVEL == "CU",
                    STC.CRED > 0 ) %>%
            select( ID = STC.PERSON.ID,
                    Term_ID = STC.TERM,
                    Course_ID = STUDENT.ACAD.CRED.ID,
                    Credit = STC.CRED,
                    Course_Level = STC.COURSE.LEVEL,
                    Grade_Code = STC.VERIFIED.GRADE,
                    Course_Section = STC.SECTION.NO,
                    EffectiveDatetime,
                    Course_Section_ID = STC.COURSE.SECTION,
                    Course_Status = STC.STATUS ) %>%
            collect() %>%
            inner_join( terms %>%
                            select(Term_ID, Term_Reporting_Year, Semester, Term_Census_Date),
                        by = "Term_ID" ) %>%
            mutate( Keep_FA = ((Semester == "FA") & (EffectiveDatetime <= as.Date(str_c(Term_Reporting_Year,"-10-15")) )),
                    Keep_NF = (Semester != "FA") ) %>%
            filter( Keep_FA | Keep_NF ) %>%
            select( -c(Keep_FA, Keep_NF) ) %>%
            left_join( course_sections, by = c("Term_ID","Course_Section_ID") )
    } else {
        course_sections_file_pat <- "DW_COURSE_SECTIONS_.*"
        course_sections <- merge_files( path = file_path, pattern = course_sections_file_pat, 
                                        col_types = cols())

        student_acad_cred_file_pat <- "DW_STUDENT_ACAD_CRED_.*"
        student_acad_cred <- merge_files( path = file_path, pattern = student_acad_cred_file_pat, 
                                          col_types = cols())

    }

    #
    # Get most recent effective date for each person for each term for each course
    #
    sac_max_effdt <- student_acad_cred %>%
        group_by( ID, Term_ID, Course_ID ) %>%
        summarise( EffectiveDatetime = max(EffectiveDatetime) )

    #
    # Now get the course data for the latest courses.
    # Use Status of A,N for FA since we want only enrolled courses at the cutoff date
    #     (This will be taken care of later as we need the W credits to determine load)
    # Use Status A,N,W for SP,SU since these were all the courses enrolled in at census
    #
    sac_most_recent_all <- student_acad_cred %>%
        inner_join( sac_max_effdt,
                    by = c("ID", "Term_ID", "Course_ID", "EffectiveDatetime") ) %>%
        filter( Course_Status %in% c('A', 'N', 'W') ) %>%
        select( -EffectiveDatetime ) %>%
        distinct() %>%
        select( -Course_ID )

    #
    # Get list of students who are taking at least 1 non-developmental/audited course
    #
    sac_most_recent_non_dev_ids <- sac_most_recent_all %>%
        filter( coalesce(Course_Level,"ZZZ") != "DEV" ) %>%
        filter( coalesce(Grade_Code,'X') != '9' ) %>%
        select( ID, Term_ID ) %>%
        distinct()

    #
    # Get list of students who are taking at least 1 distance course
    #
    sac_most_recent_1_distance_ids <- sac_most_recent_all %>%
        inner_join( sac_most_recent_non_dev_ids, by = c("ID", "Term_ID") ) %>%
        filter( str_detect(coalesce(Course_Section,"ZZZ"), 'W') |
                    str_detect(coalesce(Course_Section,"ZZZ"), 'IN') |
                    Section_Location == "OL" ) %>%
        filter( coalesce(Grade_Code,'X') != '9' ) %>%
        select( ID, Term_ID ) %>%
        distinct()

    #
    # Get list of students who are taking at least 1 regular course
    #
    sac_most_recent_f2f_ids <- sac_most_recent_all %>%
        filter( !(str_detect(coalesce(Course_Section,"ZZZ"), 'W')) &
                    !(str_detect(coalesce(Course_Section,"ZZZ"), 'IN')) &
                    Section_Location != "OL" ) %>%
        filter( coalesce(Grade_Code,'X') != '9' ) %>%
        select( ID, Term_ID ) %>%
        distinct()

    sac_most_recent_all_distance_ids <- sac_most_recent_1_distance_ids %>%
        anti_join( sac_most_recent_f2f_ids, by = c("ID", "Term_ID") ) %>%
        mutate( Distance_Courses = "All" )

    sac_most_recent_distance_ids <- sac_most_recent_1_distance_ids %>%
        left_join( sac_most_recent_all_distance_ids, by = c("ID", "Term_ID") ) %>%
        mutate( Distance_Courses = coalesce(Distance_Courses,"At least 1") )

    # Determine which students have completely withdrawn at the end or by Oct 15
    sac_most_recent_all_withdraws <- sac_most_recent_all %>%
        filter( Course_Status %in% c('W') ) %>%
        anti_join( sac_most_recent_all %>% filter( Course_Status %in% c('A', 'N') ),
                by = c("ID", "Term_ID" ) ) %>%
        select( ID, Term_ID ) %>%
        distinct() %>%
        mutate( Enrollment_Status = "Withdrawn" )

    #
    # Now create a summary table to calculate load by term
    #
    sac_load_by_term <- sac_most_recent_all %>%
        inner_join( sac_most_recent_non_dev_ids, by = c("ID", "Term_ID") ) %>%
        inner_join( terms %>% select( Term_ID, Term_Reporting_Year, Semester ),
                    by = c("Term_ID", "Term_Reporting_Year", "Semester") ) %>%
        group_by( ID, Term_ID, Term_Reporting_Year, Semester ) %>%
        summarise( Credits = sum(Credit) ) %>%
        mutate( Status = if_else(Credits >= 12, "FT", "PT") ) %>%
        ungroup() %>%
        left_join( sac_most_recent_distance_ids, by = c("ID", "Term_ID") ) %>%
        left_join( sac_most_recent_all_withdraws, by = c("ID", "Term_ID") ) %>%
        mutate( Distance_Courses = coalesce(Distance_Courses,"None"),
                Enrollment_Status = coalesce(Enrollment_Status,"Enrolled") )

    #
    # Take term load table and reduce to the reporting terms
    #
    if (!is.na(report_years)) {
        sac_report_load_by_term <- sac_load_by_term %>%
            inner_join( reporting_terms %>% select( Term_ID, Term_Reporting_Year ),
                        by = c("Term_ID", "Term_Reporting_Year") )

        return( sac_report_load_by_term )
    } else {
        return( sac_load_by_term )
    }
}

#' A special function to call term_enrollment for just a fall term
#'
#' All data comes from IERG SQL Server database
#'
#' @param report_years The year of the fall term for the data
#' @export
#'
fall_enrollment <- function( report_years = NA, file_path = NA_character_ ) {
    return( term_enrollment( report_years, "FA" ) )
}

#' Return a data frame of students who are curriculum credential seekers (seeking an Associate's, Diploma, or Certificate)
#'
#' All data comes from IERG SQL Server database
#'
#' @param report_years The year or a list of years of the fall term for the data. If unspecified, all years are returned.
#' @param report_semesters Either a single semester abbreviation or a list of semester abbreviations. If unspecified, all semesters are returned.
#' @param exlude_hs Should function exclude high school students from being included as credential seekers. Default is to include high school students.
#' @export
#' @importFrom magrittr %<>%
#'
credential_seekers <- function( report_years = NA_integer_, report_semesters = NA_character_, exclude_hs = FALSE, file_path = NA_character_ ) {


    reporting_terms <- get_terms( file_path, report_years, report_semesters )

    #
    # Get program dates (this is a multi-valued field that needs to be joined with full table).
    #
    student_programs__dates <- getColleagueData( "STUDENT_PROGRAMS__STPR_DATES", version="history" ) %>%
        select( ID = STPR.STUDENT,
                Program = STPR.ACAD.PROGRAM,
                Program_Start_Date = STPR.START.DATE,
                Program_End_Date = STPR.END.DATE,
                EffectiveDatetime ) %>%
        filter( !(Program %in% c("BSP", "AHS", "CONED", "=GED", "=HISET", "C11", "C50")) ) %>%
        collect() %>%
        mutate( Program_End_Date = coalesce(Program_End_Date,as.Date("9999-12-31")) )

    if (exclude_hs) {
        student_programs__dates %<>% anti_join( high_school_programs, by = "Program" )
    }

    #
    # Credential-seekers are those in A, D, or C programs
    #
    credential_seeking <- getColleagueData( "STUDENT_PROGRAMS" ) %>%
        select( ID = STPR.STUDENT,
                Program = STPR.ACAD.PROGRAM,
                EffectiveDatetime ) %>%
        filter( !(Program %in% c("BSP", "AHS", "CONED", "=GED", "=HISET", "C11", "C50")) ) %>%
        collect() %>%
        inner_join( student_programs__dates, by = c("ID", "Program", "EffectiveDatetime") ) %>%
        select( -EffectiveDatetime ) %>%

        # Identify credential seekers.
        mutate( Credential_Seeker = case_when(
                    substring(Program,1,1) %in% c('A','D','C') ~ 1,
                    TRUE ~ 0
                ),
                j = 1 ) %>%

        # Cross join with terms to get all the terms they were enrolled in this credential program.
        full_join( terms %>%
                       select(Term_ID, Term_Start_Date, Term_Census_Date, Term_End_Date) %>%
                       mutate(j=1),
                   by = "j" ) %>%
        filter( Program_Start_Date <= Term_Census_Date,
                Program_End_Date >= Term_Census_Date,
                Credential_Seeker > 0 ) %>%
        select( ID, Term_ID, Credential_Seeker ) %>%
        distinct() %>%
        inner_join( reporting_terms %>% select(Term_ID), by = "Term_ID" )

    return( credential_seeking )
}

#' A special function to call credential_seekers for just a fall term
#'
#' All data comes from IERG SQL Server database
#'
#' @param report_years The year of the fall term for the data
#' @param exlude_hs Should function exclude high school students from being included as credential seekers. Default is to include high school students.
#' @export
#'
fall_credential_seekers <- function( report_years, exclude_hs = FALSE, file_path = NA_character_ ) {
    return( credential_seekers( report_years, "FA", exclude_hs ) )
}


