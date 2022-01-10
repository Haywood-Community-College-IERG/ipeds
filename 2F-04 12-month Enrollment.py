# %% [markdown]

# %% Define some global variables
__REPORT_GROUP__ = 'e12'
__COLLECTION_GROUP__ = 'fall'

__OVERRIDE_REPORT_YEAR__ = None
__WRITE_OUTPUT__ = True

logger_level = 'INFO'

# %% Initialize 
from datetime import datetime
from loguru import logger
import numpy as np
import os
import pandas as pd
from pathlib import Path
#import sys
from typing import Any, Dict
import yaml

# Import local packages
import pycolleague as colleague

__source__ = "2F-04 12-month Enrollment.py"
__version__ = "1.0.0"

def timestamp():
    return datetime.now().strftime("%Y%m%d-%H%M%S")

__current_datetime_str__ = timestamp()

__current_day__ = int(__current_datetime_str__[6:8])
__current_month__ = int(__current_datetime_str__[4:6] )
__current_year__ = int(__current_datetime_str__[0:4])

__log_path__ = Path("./logs")

__project_path__ =  Path('.')

# def main(sys_args):

conn = colleague.ColleagueConnection(source="ccdw")

config_path = "./config.yml"

with open(config_path,"r") as ymlfile:
    cfg_l = yaml.load(ymlfile, Loader=yaml.FullLoader)

    if cfg_l["config"]["location"] == "self":
        cfg = cfg_l.copy()
    else:
        with open(cfg_l["config"]["location"] + "config.yml","r") as ymlfile2:
            cfg = yaml.load(ymlfile2, Loader=yaml.FullLoader)

logger_types = ["TRACE", "DEBUG", "INFO", "SUCCESS", "WARNING", "ERROR", "CRITICAL"]

# Setup the new logger
logger.remove()
logger.add(
    os.path.join(__log_path__, f"log_{__current_datetime_str__}.log"),
    enqueue=True,
    backtrace=True,
    diagnose=True,
    level=logger_level,
    rotation="20 MB",
)

if (__OVERRIDE_REPORT_YEAR__):
    report_year = __OVERRIDE_REPORT_YEAR__
else:
    report_year = __current_year__ - (0 if __current_month__ < 7 else 1)

report_year_data_start = datetime.strptime(f'{report_year-1}-07-01', '%Y-%m-%d')
report_year_data_end = datetime.strptime(f'{report_year}-06-30', '%Y-%m-%d')

ft_cohort_year = f"{report_year}FT"
pt_cohort_year = f"{report_year}PT"
tf_cohort_year = f"{report_year}TF"
tp_cohort_year = f"{report_year}TP"
rf_cohort_year = f"{report_year}RF"
rp_cohort_year = f"{report_year}RP"

output_path = __project_path__ / "output"
ipeds_import_fn = f"ipeds_{report_year}_{__REPORT_GROUP__}_{__current_datetime_str__}.txt"

report_year_adjusted = report_year - 1


####
####  Now get the data
####

# %% Get Terms
term_indexes = {"FA":1,"SP":2,"SU":3}
term_names = {"FA":"Fall","SP":"Spring","SU":"Summer"}
def createAY(x):
    yr = x[:4]
    py = str(int(yr) - 1)
    ny = str(int(yr) + 1)

    if x[4:] in ['FA','CE3']:
        rtn = f"{yr}-{ny}"
    else:
        rtn = f"{py}-{yr}"

    return(rtn)

terms = (
    conn.get_data(
        "TERMS", 
        cols={
            "TERMS.ID"            : "Term_ID", 
            "TERM.REPORTING.YEAR" : "Term_Reporting_Year", 
            "TERM.START.DATE"     : "Term_Start_Date", 
            "TERM.CENSUS.DATES"   : "Term_Census_Date",
            "TERM.END.DATE"       : "Term_End_Date",
        }, 
        where=f"""
            [TERM.REPORTING.YEAR] >= {__current_year__ - 10}
            AND [TERM.REPORTING.YEAR] <= {__current_year__}
        """
    )
    .assign(Term_Abbreviation=lambda df: df.Term_ID.str[4:])
    .loc[lambda df: df.Term_Abbreviation.isin(['FA','SP','SU'])]
    #.query('Term_Abbreviation in ["FA","SP","SU"')
    .assign(Term_Index=lambda df: df.Term_Abbreviation.map(term_indexes),
            Term_Name=lambda df: df.Term_Abbreviation.map(term_names),
    #        Academic_Year=lambda df: df["Term_ID"].apply(createAY)
        )
    .pipe( assign_applyfunc, new_col='Academic_Year', param_col='Term_ID', func=createAY )
    .astype({'Term_ID':'str',
             'Term_Abbreviation':'str',
             'Term_Name':'str',
             'Academic_Year':'str',
             'Term_Start_Date':'datetime64',
             'Term_Census_Date':'datetime64',
             'Term_End_Date':'datetime64',
             'Term_Index':int,
             'Term_Reporting_Year':int})
    #.pipe(dfsnapshot)
)

term_ids = terms["Term_ID"]

reprting_term_ids = terms[terms.Term_Reporting_Year == report_year,"Term_ID"]

# %% ipeds_cohorts
# ipeds_cohorts includes Term_Cohort. We cannot use STUDENT_TERMS or the ipeds_cohorts() function
#     within the ipeds module as neither of these include the Term_Cohort.
ipeds_cohorts = (
    conn.get_data( 
        "ipeds_cohorts", 
        schema="local", 
        version="all",
        cols = ["ID", "Term_ID", "Cohort", "Term_Cohort"],
    )
    #.pipe(dfcsv, "ipeds_cohorts_1.csv", index=False)

    .astype({"Term_ID":str})
    #.pipe(dfsnapshot)
    .pipe((pd.merge, "left"), right=terms, on="Term_ID", how="inner")
    #.pipe(dfsnapshot)
    .rename(columns={"Term_Reporting_Year" : "Cohort_Year",
                        "Term_Start_Date"     : "Cohort_Start_Date" 
                        })
    .loc[lambda df: (df.Cohort_Year == report_year) & (df.Term_ID == report_term)]

    #.pipe(dfcsv, "ipeds_cohorts.csv", index=False)
)
#inner_join( terms %>% select(Term_ID,Cohort_Year=Term_Reporting_Year,Cohort_Start_Date=Term_Start_Date) ) %>%
#filter( Cohort_Year %in% c(report_year), Term_ID == report_term )

# %% PERSON
ethnic_race_map = {
    'FOR' : 'Nonresident Alien',
    'HIS' : 'Hispanic/Latino',
    'MULTI' : 'Two or more races',
    'AN' : 'American Indian or Alaska Native',
    'AS' : 'Asian',
    'BL' : 'Black or African American',
    'HP' : 'Native Hawaiian or Other Pacific Islander',
    'WH' : 'White',
}
person = (
    conn.get_data(
        'PERSON',
        schema='history',
        version='current',
        cols={
            'ID' : 'Campus_ID',
            'FIRST.NAME' : 'First_Name',
            'LAST.NAME' : 'Last_Name',
            'BIRTH.DATE' : 'Birth_Date',
            'GENDER' : 'Gender',
            'ETHNIC' : 'Ethnic',
            'CITIZENSHIP' : 'Citizenship',
            'RESIDENCE.COUNTRY' : 'Country',
            'VISA.TYPE' : 'Visa_Type',
            'PER.ETHNICS' : 'Per_Ethnics',
            'PER.RACES' : 'Per_Races',
            'X.ETHNICS.RACES' : 'X_Ethnics_Races',
            #'EffectiveDatetime' : 'EffectiveDatetime',
        },
        where="[FIRST.NAME] != ''",
    )
    #     filter( EffectiveDatetime <= as.Date(str_c(report_year,"-10-15")) ) %>%
    #.loc[lambda df: df.EffectiveDatetime <= datetime.datetime.strptime(f"{report_year}-10-15", "%Y-%m-%d")]     

    .assign(=lambda df: )
    .assign(Term_IPEDS_RaceIndex=lambda df: df.X_Ethnics_Races.map(ethnic_race_map),

    .loc[:, ["Campus_ID", "First_Name", "Last_Name", "Residence_State", "Tuition_Type"]]

    #.pipe(dfcsv, "person.csv", index=False)
)


###
###
###

# %% Define some supporting functions for pipes
def dfsnapshot(df, fn=lambda x: x.info(), msg=None):
    """ Custom Help function to print things in method chaining.
        Returns back the df to further use in chaining.
    """
    if msg:
        print(msg)
    logger.debug(f"{fn(df)}")
    return df

def coalesce(*arg):
  for el in arg:
    if el is not None:
      return el
  return None

def dfcsv(df, *args, **kwargs):
    if logger_level in ["TRACE","DEBUG"]:
        df.to_csv(*args, **kwargs)
    return df

def dfreplace(df, columns, svalue=np.nan, rvalue=''):
    df[columns] = df[columns].replace(svalue,rvalue)
    #df.loc[df]
    return df

def assign_applyfunc(df, new_col, func, param_col):
    df[new_col] = df[param_col].apply(func, axis=1)
    return df 

def __generate_SFA(loc_config: Dict[str,Any]):





    if report_year < 2015:
        # Raise error
        pass 

    # The code below only works for reports starting in 2015

    financial_aid_award_list = conn.get_data(
        "SA_ACYR__AWARD_LIST",
        schema="history",
        version="all",
        cols={
            "SA.STUDENT.ID" : "Campus_ID",
            "SA.YEAR" : "Term_Reporting_Year",
            "SA.AMOUNT" : "Award_Amount",
            "SA.XMIT.AMT" : "Distributed_Amount",
            "SA.AWARD" : "Award_Code",
            "SA.ACTION" : "Award_Action",
            "X.SA.AWARD.CATEGORY" : "Award_Category",
            "X.SA.AWARD.TYPE" : "Award_Type",
            "X.SA.AWARD.DESTINATION" : "Award_Destination",
            "EffectiveDatetime" : "EffectiveDatetime",
        },
        where=f"[SA.YEAR] == {report_year}",
    )
        
    financial_aid = (
        conn.get_data(
            "SA_ACYR",
            schema="history",
            version="current",
            cols={
                "SA.STUDENT.ID" : "Campus_ID",
                "SA.YEAR" : "Term_Reporting_Year",
                "SA.AWARDED" : "Award_Amount_Total",
                "EffectiveDatetime" : "EffectiveDatetime",
            },
            where=f"[SA.YEAR] == {report_year}",

        )
        .pipe(dfsnapshot)

        #         inner_join( financial_aid_award_list ) %>%
        .pipe((pd.merge,"left"), 
              right=financial_aid_award_list, 
              on=["Campus_ID", "Term_Reporting_Year", "EffectiveDatetime"], 
              how="inner")

        #         mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) ) %>%
        .astype({"Term_Reporting_Year":int})

        .pipe(dfsnapshot)
        
        #         filter( Award_Amount > 0.00 ) %>%
        .loc[lambda df: df.Award_Amount > 0.00]

        #         select( -EffectiveDatetime )
        .drop(['EffectiveDatetime'], axis=1)
    )  

    # #
    # # Group 1 includes all the fall enrolled students as of October 15. 
    # #

    sfa_group1 = report_year
    # sfa_group1 <- fall_enrollment( report_year ) %>%
    #     filter( Enrollment_Status != "Withdrawn" ) %>%
    #     select( ID, Term_ID, Term_Reporting_Year ) %>%
    #     left_join( person ) %>%
    #     left_join( ipeds_cohorts ) %>%
    #     left_join( cs_acyr ) %>%
    #     left_join( financial_aid ) %>%
    #     mutate( Living_Arrangement = coalesce(Living_Arrangement,"UNKNOWN"),
    #             Award_Amount_Total = coalesce(Award_Amount_Total,0),
    #             Award_Amount = coalesce(Award_Amount,0),
    #             Distributed_Amount = coalesce(Distributed_Amount,0),
    #             Award_Code = coalesce(Award_Code,"NONE"),
    #             Award_Action = coalesce(Award_Action,'-'),
    #             Award_Category = coalesce(Award_Category,"NONE"),
    #             Award_Type = coalesce(Award_Type,'-'),
    #             Award_Destination = coalesce(Award_Destination,'-') ) %>%
    #     mutate( IPEDS_Award_Category = case_when(
    #         Award_Category %in% c('SCH') & Award_Type == 'F' ~ "Federal grant",
    #         Award_Category %in% c('SCH') & Award_Type == 'S' ~ "State grant",
    #         Award_Category %in% c('SCH') & Award_Type == 'I' ~ "Institutional grant",
    #         Award_Category %in% c('SCH') & Award_Type == 'O' ~ "Other grant",
    #         Award_Category %in% c('ALT') & Award_Type == 'F' ~ "Federal loan",
    #         Award_Category %in% c('ALT') & Award_Type == 'O' ~ "Other loan",
    #         Award_Category %in% aid_federal_pell ~ "Federal Pell",
    #         Award_Category %in% aid_federal_grant ~ "Federal grant",
    #         Award_Category %in% aid_federal_loans ~ "Federal loan",
    #         Award_Category %in% aid_state_local ~ "State grant",
    #         Award_Category %in% aid_institution ~ "Institutional grant",
    #         Award_Category %in% aid_federal_ws ~ "Federal work-study",
    #         Award_Category %in% aid_other ~ "Other grant",
    #         Award_Category %in% aid_other_loans ~ "Other loan",
    #         TRUE ~ "UNDEFINED"
    #     ),
    #         IPEDS_Award_Veterans = if_else( Award_Code %in% aid_veterans, "Veterans", "Not Veterans" )
    #     )

    # if (CLEANUP) {
    #     rm( person, ipeds_cohorts, financial_aid )
    # }

    # #
    # # Group 2 are the first-time, full-time degree seeking students from Group 1.
    # #
    # sfa_group2 <- sfa_group1 %>%
    #     filter( Term_Cohort == report_cohort )

    # sfa_group2_ids <- sfa_group2 %>%
    #     select(ID) %>%
    #     distinct()

    # ipeds_award_categories2b <- unique(c(aid_loans,
    #                                     aid_federal_grant,
    #                                     aid_state_local,
    #                                     aid_institution))
    # sfa_group2b_ids <- sfa_group2 %>%
    #     filter( Award_Category %in% ipeds_award_categories2b ) %>%
    #     select(ID) %>%
    #     distinct()

    # # Group 3 students include in-state students awarded grants or scholarships from:
    # # federal government, state/local government, or the institution.
    # # 
    # # DO NOT include students who received aid only from other sources.
    # ipeds_award_categories_g3 <- unique(c(aid_federal_grant,
    #                                     aid_state_local,
    #                                     aid_institution))
    # sfa_group3 <- sfa_group2 %>%
    #     filter( Award_Category %in% ipeds_award_categories_g3,
    #             Award_Type %in% c('F','S','I') ) %>%
    #     filter( Tuition_Type == "In-State" ) #%>%
    #     #select( ID ) %>%
    #     #distinct() %>%
    #     #left_join( sfa_group2 )

    # sfa_group3_ids <- sfa_group3 %>%
    #     select(ID) %>%
    #     distinct()

    # # Group 4 students include in-state students awarded Title IV federal student aid:
    # # Federal Pell, Federal Supplemental Educational Opportunity Grant (FSEOG), 
    # # Academic Competitiveness Grant (ACG), National Science and Mathematics Access 
    # # to Retain Talent Grant (National SMART Grant), Teacher Education Assistance for 
    # # College and Higher Education (TEACH), Federal Work Study, Federal Perkins Loan,
    # # Subsidized Direct or FFEL Stafford Loan, and Unsubsidized Direct or FFEL Stafford Loan
    # ipeds_award_categories_g4 <- unique(c(aid_federal_grant,
    #                                     aid_federal_ws,
    #                                     aid_federal_loans))
    # sfa_group4 <- sfa_group2 %>%
    #     filter( Award_Category %in% ipeds_award_categories_g4,
    #             Award_Type == 'F' ) #%>%
    #     # select( ID ) %>%
    #     # distinct() %>%
    #     # left_join( sfa_group2 )

    # sfa_group1 %<>% 
    #     left_join( sfa_group2 %>% select(ID) %>% distinct() %>% mutate( In_Group2 = "Yes") ) %>%
    #     left_join( sfa_group3 %>% select(ID) %>% distinct() %>% mutate( In_Group3 = "Yes") ) %>%
    #     left_join( sfa_group4 %>% select(ID) %>% distinct() %>% mutate( In_Group4 = "Yes") ) %>%
    #     mutate_at(.vars=c("In_Group2","In_Group3","In_Group4"), function(x) {return( coalesce(x,"No") )} )
        
    # #####
    # #####
    # #
    # # Code to generate IPEDS import files
    # #
    # ####
    # ####

    # #
    # # Part A is ALL students
    # #
    # saf_a_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
    #                 SCFA2=NA_character_, SCFY2=NA_character_,
    #                 UGAIDN=NA_character_, UGAIDPN=NA_character_, UGLNFN=NA_character_,
    #                 UGAIDA=NA_character_, UGAIDPA=NA_character_, UGLNFA=NA_character_ )
    # a_cols_a <- c( UGAIDA=NA_character_, UGAIDPA=NA_character_, UGLNFA=NA_character_ )
    # n_cols_a <- c( SCFA2=NA_character_, UGAIDN=NA_character_, UGAIDPN=NA_character_, UGLNFN=NA_character_ )
    # b6_cols_a <- c( SCFY2=NA_character_ )

    # ipeds_award_categories_ugaida <- unique(c(aid_federal_grant,aid_state_local,aid_institution,aid_other))

    # ipeds_saf_a <- sfa_group1 %>%
        
    #     summarise( SCFA2 = n_distinct(ID),  # IPEDS SFA Part A, Line 01
    #             UGAIDN = n_distinct(ID[Award_Amount > 0], na.rm = TRUE),  # IPEDS Part B, Line 01, Column 1
    #             UGAIDPN = n_distinct(ID[Award_Category %in% aid_federal_pell & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part B, Line 02, Column 1
    #             UGLNFN = n_distinct(ID[Award_Category %in% aid_federal_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part B, Line 03, Column 1
    #             UGAIDA = sum(Award_Amount[Award_Category %in% ipeds_award_categories_ugaida], na.rm = TRUE),  # IPEDS SFA Part B, Line 01, Column 3
    #             UGAIDPA = sum(Award_Amount[Award_Category %in% aid_federal_pell], na.rm = TRUE),  # IPEDS SFA Part B, Line 02, Column 3
    #             UGLNFA = sum(Award_Amount[Award_Category %in% aid_federal_loans], na.rm = TRUE),  # IPEDS SFA Part B, Line 03, Column 3
    #     ) %>%
        
    #     add_column( !!!saf_a_cols[!names(saf_a_cols) %in% names(.)] ) %>%

    #     mutate_at(.vars=names(n_cols_a), function(x) {return( sprintf("%06d", x) )} ) %>%
    #     mutate_at(.vars=names(a_cols_a), function(x) {return( sprintf("%012.0f", x) )} ) %>%
    #     mutate_at(.vars=names(b6_cols_a), function(x) {return( coalesce(x, "      ") )} ) %>%

    #     # Add additional columns as per Import Specs
    #     mutate( UNITID = ipeds_unitid,
    #             SURVSECT = "SFA",
    #             PART = 'A' ) %>%
        
    #     # Reorder the columns to be in the proper order as per Import Specs
    #     select( !! names(saf_a_cols) )

    # if (CLEANUP) {
    #     # Cleanup by removing the extra data frames
    #     rm( saf_a_cols, a_cols_a, n_cols_a )
    # }

    # #
    # # Part B is First-time/First-Term, Counts of students (GROUP 2)
    # #
    # saf_b_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
    #                 SCFA1N=NA_character_, SCFA11N=NA_character_, SCFA12N=NA_character_, SCFA13N=NA_character_,
    #                 SCFY1N=NA_character_,  # IPEDS SFA Part A, Line 02
    #                 ANYAIDN=NA_character_, ANYAIDNF=NA_character_ )
    # n_cols_b <- c( SCFA1N=NA_character_, SCFA12N=NA_character_, SCFA13N=NA_character_,
    #             ANYAIDN=NA_character_, ANYAIDNF=NA_character_ )
    # b6_cols_b <- c( SCFY1N=NA_character_, SCFA11N=NA_character_  )

    # ipeds_award_categories_2a <- unique(c(aid_federal_ws,
    #                                     aid_loans,
    #                                     aid_federal_grant,
    #                                     aid_state_local,
    #                                     aid_institution,
    #                                     aid_other))

    # ipeds_award_categories_2b <- unique(c(aid_loans,
    #                                     aid_federal_grant,
    #                                     aid_state_local,
    #                                     aid_institution))

    # ipeds_saf_b <- sfa_group2 %>%
        
    #     summarise( SCFA1N = n_distinct(ID),  # IPEDS SFA Part A, Line 02
    #             SCFA12N = n_distinct(ID[Tuition_Type == "In-State"], na.rm = TRUE),  # IPEDS SFA Part C, Page 1, Line 01b
    #             SCFA13N = n_distinct(ID[Tuition_Type == "Out-of-State"], na.rm = TRUE),  # IPEDS SFA Part C, Page 1, Line 01c
                
    #             ANYAIDN = n_distinct(ID[Award_Category %in% ipeds_award_categories_2a & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part A, Line 02a
    #             ANYAIDNF = n_distinct(ID[Award_Category %in% ipeds_award_categories_2b & Award_Amount >0], na.rm = TRUE)  # IPEDS SFA Part A, Line 02b
    #     ) %>%
        
    #     add_column( !!!saf_b_cols[!names(saf_b_cols) %in% names(.)] ) %>%
        
    #     mutate_at(.vars=names(n_cols_b), function(x) {return( sprintf("%06d", x) )} ) %>%
    #     mutate_at(.vars=names(b6_cols_b), function(x) {return( coalesce(x, "      ") )} ) %>%

    #     # Add additional columns as per Import Specs
    #     mutate( UNITID = ipeds_unitid,
    #             SURVSECT = "SFA",
    #             PART = 'B',
    #             SCFA11N = "000000"   # IPEDS SFA Part C, Page 1, Line 01a
    #             ) %>%
        
    #     # Reorder the columns to be in the proper order as per Import Specs
    #     select( !! names(saf_b_cols) )

    # if (CLEANUP) {
    #     # Cleanup by removing the extra data frames
    #     rm( saf_b_cols, n_cols_b )
    # }

    # #
    # # Part C is First-time/First-Term, Counts and Total Awarded for various subgroups (GROUP 2)
    # #
    # saf_c_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
    #                 AGRNT_N=NA_character_, FGRNT_N=NA_character_, PGRNT_N=NA_character_, OFGRNT_N=NA_character_,
    #                 SGRNT_N=NA_character_, IGRNT_N=NA_character_, 
    #                 LOAN_N=NA_character_, FLOAN_N=NA_character_, OLOAN_N=NA_character_,
    #                 PGRNT_T=NA_character_, OFGRNT_T=NA_character_, SGRNT_T=NA_character_, IGRNT_T=NA_character_, 
    #                 FLOAN_T=NA_character_, OLOAN_T=NA_character_ )
    # n_cols_c <- c( AGRNT_N=NA_character_, FGRNT_N=NA_character_, PGRNT_N=NA_character_, OFGRNT_N=NA_character_,
    #             SGRNT_N=NA_character_, IGRNT_N=NA_character_, 
    #             LOAN_N=NA_character_, FLOAN_N=NA_character_, OLOAN_N=NA_character_ )
    # a_cols_c <- c( PGRNT_T=NA_character_, OFGRNT_T=NA_character_, SGRNT_T=NA_character_, IGRNT_T=NA_character_, 
    #             FLOAN_T=NA_character_, OLOAN_T=NA_character_ )

    # ipeds_award_categories_c02_q1 <- unique(c(aid_federal_grant,aid_state_local,aid_institution))

    # ipeds_award_categories_2a <- unique(c(aid_federal_ws,
    #                                     aid_loans,
    #                                     aid_federal_grant,
    #                                     aid_state_local,
    #                                     aid_institution,
    #                                     aid_other))

    # ipeds_award_categories_2b <- unique(c(aid_loans,
    #                                     aid_federal_grant,
    #                                     aid_state_local,
    #                                     aid_institution))

    # ipeds_saf_c <- sfa_group2 %>%
        
    #     summarise( AGRNT_N = n_distinct(ID[Award_Category %in% ipeds_award_categories_c02_q1 & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 01, Column 1
    #             FGRNT_N = n_distinct(ID[Award_Category %in% aid_federal_grant & Award_Type == 'F' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02, Column 1
    #             PGRNT_N = n_distinct(ID[Award_Category %in% aid_federal_pell & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02a, Column 1
    #             OFGRNT_N = n_distinct(ID[Award_Category %in% aid_federal_non_pell & Award_Type == 'F' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02b, Column 1
    #             SGRNT_N = n_distinct(ID[Award_Category %in% aid_state_local & Award_Type == 'S' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 03, Column 1
    #             IGRNT_N = n_distinct(ID[Award_Category %in% aid_institution & Award_Type == 'I' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 04, Column 1
    #             LOAN_N = n_distinct(ID[Award_Category %in% aid_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05, Column 1
    #             FLOAN_N = n_distinct(ID[Award_Category %in% aid_federal_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05a, Column 1
    #             OLOAN_N = n_distinct(ID[Award_Category %in% aid_other_loans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05b, Column 1
                
    #             PGRNT_T = sum(Award_Amount[Award_Category %in% aid_federal_pell], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02a, Column 3
    #             OFGRNT_T = sum(Award_Amount[Award_Category %in% aid_federal_non_pell & Award_Type == 'F'], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 02b, Column 3
    #             SGRNT_T = sum(Award_Amount[Award_Category %in% aid_state_local & Award_Type == 'S'], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 03, Column 3
    #             IGRNT_T = sum(Award_Amount[Award_Category %in% aid_institution & Award_Type == 'I'], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 04, Column 3
    #             FLOAN_T = sum(Award_Amount[Award_Category %in% aid_federal_loans], na.rm = TRUE),  # IPEDS SFA Part C, Page 2, Line 05a, Column 3
    #             OLOAN_T = sum(Award_Amount[Award_Category %in% aid_other_loans], na.rm = TRUE)  # IPEDS SFA Part C, Page 2, Line 05b, Column 3
    #     ) %>%
        
    #     #mutate( OFGRNT_N = FGRNT_N - PGRNT_N, OFGRNT_T = FGRNT_T - PGRNT_T ) %>%
        
    #     mutate_at(.vars=names(n_cols_c), function(x) {return( sprintf("%06d", x) )} ) %>%
    #     mutate_at(.vars=names(a_cols_c), function(x) {return( sprintf("%012.0f", x) )} ) %>%
        
    #     # Add additional columns as per Import Specs
    #     mutate( UNITID = ipeds_unitid,
    #             SURVSECT = "SFA",
    #             PART = 'C' ) %>%
        
    #     # Reorder the columns to be in the proper order as per Import Specs
    #     select( !! names(saf_c_cols) )

    # if (CLEANUP) {
    #     # Cleanup by removing the extra data frames
    #     rm( saf_c_cols, a_cols_c, n_cols_c )
    # }

    # groupc_pgrnt <- as.integer((ipeds_saf_c %>% collect %$% PGRNT_T)[1])
    # groupc_ofgrnt <- as.integer((ipeds_saf_c %>% collect %$% OFGRNT_T)[1])
    # groupc_sgrnt <- as.integer((ipeds_saf_c %>% collect %$% SGRNT_T)[1])
    # groupc_igrnt <- as.integer((ipeds_saf_c %>% collect %$% IGRNT_T)[1])
    # groupc_awards <- groupc_pgrnt + groupc_ofgrnt + groupc_sgrnt + groupc_igrnt

    # #
    # # Part D is first-time/first-Term in-state students who were awarded grants or scholarships from
    # #     federal government, state/local government, or the institution, Counts and Total Awarded 
    # #     for various subgroups (GROUP 3)
    # #
    # saf_d_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
    #                 FTFT_Y1=NA_character_, FTFT_Y2=NA_character_, FTFT_Y3=NA_character_,
    #                 ONCAM_Y1=NA_character_, ONCAM_Y2=NA_character_, ONCAM_Y3=NA_character_,
    #                 OF_W_FAM_Y1=NA_character_, OF_W_FAM_Y2=NA_character_, OF_W_FAM_Y3=NA_character_, 
    #                 OF_WO_FAM_Y1=NA_character_, OF_WO_FAM_Y2=NA_character_, OF_WO_FAM_Y3=NA_character_, 
    #                 TOTALG_Y1=NA_character_ )
    # n_cols_d <- c( FTFT_Y1=NA_character_, ONCAM_Y1=NA_character_, OF_W_FAM_Y1=NA_character_, OF_WO_FAM_Y1=NA_character_ )
    # a_cols_d <- c( TOTALG_Y1=NA_character_ )
    # b6_cols_d <- c( FTFT_Y2=NA_character_, FTFT_Y3=NA_character_,
    #                 ONCAM_Y2=NA_character_, ONCAM_Y3=NA_character_,
    #                 OF_W_FAM_Y2=NA_character_, OF_W_FAM_Y3=NA_character_, 
    #                 OF_WO_FAM_Y2=NA_character_, OF_WO_FAM_Y3=NA_character_
    #                 )

    # ipeds_saf_d <- sfa_group3 %>%
        
    #     summarise( FTFT_Y1 = n_distinct(ID[Award_Amount >0], na.rm = TRUE),   # IPEDS SFA Part A, Line 03
    #             ONCAM_Y1 = 0,  # IPEDS SFA Part D, Line 01a, Column 3
    #             OF_W_FAM_Y1 = n_distinct(ID[Living_Arrangement == "HOME" & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part D, Line 01b, Column 3
    #             OF_WO_FAM_Y1 = n_distinct(ID[Living_Arrangement == "AWAY" & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part D, Line 01c, Column 3

    #             TOTALG_Y1 = sum(Award_Amount, na.rm = TRUE)  # IPEDS SFA Part D, Line 02, Column 3
    #     ) %>%
        
    #     add_column( !!!saf_d_cols[!names(saf_d_cols) %in% names(.)] ) %>%
        
    #     mutate_at(.vars=names(n_cols_d), function(x) {return( sprintf("%06d", x) )} ) %>%
    #     mutate_at(.vars=names(a_cols_d), function(x) {return( sprintf("%012.0f", x) )} ) %>%
    #     mutate_at(.vars=names(b6_cols_d), function(x) {return( coalesce(x, "      ") )} ) %>%
        
    #     # Add additional columns as per Import Specs
    #     mutate( UNITID = ipeds_unitid,
    #             SURVSECT = "SFA",
    #             PART = 'D' ) %>%
        
    #     # Reorder the columns to be in the proper order as per Import Specs
    #     select( !! names(saf_d_cols) )

    # if (CLEANUP) {
    #     # Cleanup by removing the extra data frames
    #     rm( saf_d_cols, a_cols_d, n_cols_d )
    # }

    # group3_awards <- as.integer((ipeds_saf_d %>% collect %$% TOTALG_Y1)[1])

    # if( group3_awards > groupc_awards ) {
    #     stop( "Group 3 awards in Part D are greater than the Group 2 awards in Part C, Page 2")
    # }

    # #
    # # Part E is first-time/first-Term in-state students who were awarded awarded Title IV federal student aid 
    # #     which includes Federal Pell, Federal Supplemental Educational Opportunity Grant (FSEOG), Academic 
    # #     Competitiveness Grant (ACG), National Science and Mathematics Access to Retain Talent Grant 
    # #     (National SMART Grant), Teacher Education Assistance for College and Higher Education (TEACH),
    # #     Federal Work Study, Federal Perkins Loan, Subsidized Direct or FFEL Stafford Loan, and
    # #     Unsubsidized Direct or FFEL Stafford Loan, Counts by various subgroups (GROUP 4)
    # #
    # saf_e_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, 
    #                 YEAR=NA_character_, FTFT_T4=NA_character_, ONCAM_T4=NA_character_, OF_W_FAM_T4=NA_character_, OF_WO_FAM_T4=NA_character_ )
    # n_cols_e <- c( FTFT_T4=NA_character_, ONCAM_T4=NA_character_, OF_W_FAM_T4=NA_character_, OF_WO_FAM_T4=NA_character_ )

    # ipeds_saf_e <- sfa_group4 %>%
        
    #     summarise( YEAR = 1,   # IPEDS SFA Part E, Column 3
    #             ONCAM_T4=0,  # IPEDS SFA Part A, Line 04
    #             FTFT_T4 = n_distinct(ID[Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part E, Line 01a, Column 1/2/3
    #             OF_W_FAM_T4 = n_distinct(ID[Living_Arrangement == "HOME" & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part E, Line 01b, Column 1/2/3
    #             OF_WO_FAM_T4 = n_distinct(ID[Living_Arrangement == "AWAY" & Award_Amount >0], na.rm = TRUE)  # IPEDS SFA Part E, Line 01c, Column 1/2/3
    #     ) %>%
        
    #     mutate_at(.vars=names(n_cols_e), function(x) {return( sprintf("%06d", x) )} ) %>%

    #     # Add additional columns as per Import Specs
    #     mutate( UNITID = ipeds_unitid,
    #             SURVSECT = "SFA",
    #             PART = 'E' ) %>%
        
    #     # Reorder the columns to be in the proper order as per Import Specs
    #     select( !! names(saf_e_cols) )

    # if (CLEANUP) {
    #     # Cleanup by removing the extra data frames
    #     rm( saf_e_cols, n_cols_e )
    # }

    # #
    # # Part F is first-time/first-Term in-state students who were awarded awarded Title IV federal student aid 
    # #     which includes Federal Pell, Federal Supplemental Educational Opportunity Grant (FSEOG), Academic 
    # #     Competitiveness Grant (ACG), National Science and Mathematics Access to Retain Talent Grant 
    # #     (National SMART Grant), Teacher Education Assistance for College and Higher Education (TEACH),
    # #     Federal Work Study, Federal Perkins Loan, Subsidized Direct or FFEL Stafford Loan, and
    # #     Unsubsidized Direct or FFEL Stafford Loan, Counts and Total Awarded by various subgroups (GROUP 4)
    # #
    # saf_f_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, YEAR=NA_character_, 
    #                 INCOME_RANGE=NA_character_, T4AID_N=NA_character_, T4AID_G=NA_character_, T4AID_T=NA_character_ )
    # n_cols_f <- c( T4AID_N=NA_character_, T4AID_G=NA_character_ )
    # a_cols_f <- c( T4AID_T=NA_character_ )

    # ipeds_saf_f <- sfa_group4 %>%
        
    #     rename( INCOME_RANGE = Income_Range ) %>%
    #     group_by( INCOME_RANGE ) %>%   # IPEDS SFA Part E, Line a/b/c/d/e or 2/3/4
        
    #     summarise( T4AID_N = n_distinct(ID[Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part E, Line 2/3/4, Column 1
    #             T4AID_G = n_distinct(ID[Award_Category %in% aid_federal_grant & Award_Type == 'F' & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Part E, Line 2/3/4, Column 2

    #             T4AID_T = sum(Award_Amount[Award_Category %in% aid_federal_grant], na.rm = TRUE)  # IPEDS SFA Part E, Line 2/3/4, Column 3
    #     ) %>%
        
    #     mutate_at(.vars=names(n_cols_f), function(x) {return( sprintf("%06d", x) )} ) %>%
    #     mutate_at(.vars=names(a_cols_f), function(x) {return( sprintf("%012.0f", x) )} ) %>%
        
    #     # Add additional columns as per Import Specs
    #     mutate( UNITID = ipeds_unitid,
    #             SURVSECT = "SFA",
    #             PART = 'F',
    #             YEAR = '1'   # IPEDS SFA Part 
    #             ) %>%
        
    #     # Reorder the columns to be in the proper order as per Import Specs
    #     select( !! names(saf_f_cols) )

    # if (CLEANUP) {
    #     # Cleanup by removing the extra data frames
    #     rm( saf_f_cols, a_cols_f, n_cols_f )
    # }

    # #
    # # Part G is Veteran's benefits
    # #
    # saf_g_cols <- c( UNITID=NA_character_, SURVSECT=NA_character_, PART=NA_character_, SLEVEL=NA_character_,
    #                 GI_BEN_N=NA_character_, GI_BEN_T=NA_character_, DOD_ASSIST_N=NA_character_, DOD_ASSIST_T=NA_character_ )
    # n_cols_g <- c( GI_BEN_N=NA_character_, DOD_ASSIST_N=NA_character_ )
    # a_cols_g <- c( GI_BEN_T=NA_character_, DOD_ASSIST_T=NA_character_ )

    # ipeds_saf_g <- sfa_group1 %>%
        
    #     summarise( GI_BEN_N = n_distinct(ID[Award_Code %in% aid_veterans & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Section 2, Line 01, Column 1
    #             GI_BEN_T = sum(Award_Amount[Award_Code %in% aid_veterans_gi], na.rm = TRUE),  # IPEDS SFA Section 2, Line 01, Column 2
                
    #             DOD_ASSIST_N = n_distinct(ID[Award_Code %in% aid_veterans_dod & Award_Amount >0], na.rm = TRUE),  # IPEDS SFA Section 2, Line 03, Column 1
    #             DOD_ASSIST_T = sum(Award_Amount[Award_Code %in% aid_veterans_dod], na.rm = TRUE)  # IPEDS SFA Section 2, Line 03, Column 2
    #     ) %>%
        
    #     mutate_at(.vars=names(n_cols_g), function(x) {return( sprintf("%06d", x) )} ) %>%
    #     mutate_at(.vars=names(a_cols_g), function(x) {return( sprintf("%012.0f", x) )} ) %>%
        
    #     # Add additional columns as per Import Specs
    #     mutate( UNITID = ipeds_unitid,
    #             SURVSECT = "SFA",
    #             PART = 'G',
    #             SLEVEL = '1'   # IPEDS SFA Section 2, Line 1/3 
    #             ) %>%
        
    #     # Reorder the columns to be in the proper order as per Import Specs
    #     select( !! names(saf_g_cols) )

    # if (CLEANUP) {
    #     # Cleanup by removing the extra data frames
    #     rm( saf_g_cols, a_cols_g, n_cols_g )
    # }

    # #
    # # Now, write all the parts out a flat file for import into IPEDS
    # #

    # if (WRITE_OUTPUT) {
    #     write.table( data.frame(ipeds_saf_a), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE  )
    #     write.table( data.frame(ipeds_saf_b), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    #     write.table( data.frame(ipeds_saf_c), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    #     write.table( data.frame(ipeds_saf_d), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    #     write.table( data.frame(ipeds_saf_e), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    #     write.table( data.frame(ipeds_saf_f), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    #     write.table( data.frame(ipeds_saf_g), file.path(output_path, fn_SFA), sep="", col.names = FALSE, row.names = FALSE, quote=FALSE, append=TRUE  )
    # }

    # if (TEST) {
    #     write_csv( sfa_group1, file.path(output_path, str_c("sfa_group1_",report_year,".csv")), na = "")
    #     write_csv( sfa_group2, file.path(output_path, str_c("sfa_group2_",report_year,".csv")), na = "")
    #     write_csv( sfa_group3, file.path(output_path, str_c("sfa_group3_",report_year,".csv")), na = "")
    #     write_csv( sfa_group4, file.path(output_path, str_c("sfa_group4_",report_year,".csv")), na = "")
    #     write_csv( ipeds_saf_a, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"a",sep="_"),".csv")), na = "" )
    #     write_csv( ipeds_saf_b, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"b",sep="_"),".csv")), na = "" )
    #     write_csv( ipeds_saf_c, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"c",sep="_"),".csv")), na = "" )
    #     write_csv( ipeds_saf_d, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"d",sep="_"),".csv")), na = "" )
    #     write_csv( ipeds_saf_e, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"e",sep="_"),".csv")), na = "" )
    #     write_csv( ipeds_saf_f, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"f",sep="_"),".csv")), na = "" )
    #     write_csv( ipeds_saf_g, file.path(output_path, str_c(str_c("ipeds",report_year,fn_report_code,"g",sep="_"),".csv")), na = "" )
    # }



def parse_args(sys_args):
    #%% Set up Argument parsing
    parser = argparse.ArgumentParser(description="IPEDS data import creator",formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument(
        "--source",
        dest="source",
        required=False,
        default="ccdw",
        choices=["ccdw","informer","datamart"],
        help="Specify the source of the data [can be %(choices)s]",
    )
    parser.add_argument(
        "--collection",
        dest="collection",
        required=True,
        default="",
        choices=["fall", "winter", "spring", "winterspring", 
                 "ic", "com", "e12", 
                 "sfa", "gr", "gr2", "adm", "om", 
                 "ef", "fin", "hr", "lib"],
        help="Specify the collection for which to generate import data [can be %(choices)s] (REQUIRED)",
    )
    parser.add_argument(
        "--reportyear",
        dest="reportyear",
        default="Current",
        help="Specify year of the IPEDS report to generate [i.e., 2020]",
    )
    parser.add_argument(
        "--output",
        dest="output",
        default="XML",
        help="Output type, either CSV or XML",
    )
    parser.add_argument(
        "--outputlocation",
        dest="outputlocation",
        default="./output",
        help="Output location",
    )
    parser.add_argument(
        "--quiet",
        dest="quiet_flag",
        action="store_true",
        default=False,
        help="Supress all output",
    )
    parser.add_argument(
        "--debug",
        dest="debug_flag",
        action="store_true",
        default=False,
        help="Add debugging statements to output",
    )
    parser.add_argument(
        "--logging",
        dest="logger_level",
        default="INFO",
        choices=["TRACE", "DEBUG", "INFO", "SUCCESS", "WARNING", "ERROR", "CRITICAL"],
        help="Specify which level of logging to use in logging output",
    )
    parser.add_argument(
        "--config_path",
        dest="config_path",
        default=".",
        help="Specify the path to the config.yml file",
    )

    return parser.parse_args(sys_args)

if __name__ == "__main__":
    #sys.argv.append('--collection WINTER')
    main(sys.argv[1:])

# %%
