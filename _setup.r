coalesce_var <- function( var, default ) {
    sym <- deparse(substitute(var))
    env <- parent.frame()
    return(ifelse(exists(sym),var,default))
}
  
#coalesce_params <- function( var ) { ifelse(coalesce(var,"")=="",NA_character_,var) }

current_datetime <- lubridate::as_datetime(
                        ifelse(params$current_datetime == "",
                               Sys.time(),
                               params$current_datetime))
current_date <- lubridate::as_date(current_datetime)
current_month <- as.numeric(format(current_date, "%m"))
current_year <- as.numeric(format(current_date, "%Y"))

if (params$report_year != "") {
    report_year <- as.integer(params$report_year)
} else {
    report_year <- current_year - dplyr::if_else(current_month < 7, 1, 0)
}

report_data_year <- report_year - report_year_data_adjustment

WRITE_OUTPUT <- coalesce_var(WRITE_OUTPUT, params$write_output)
TEST <- coalesce_var(TEST, params$test)
CLEANUP <- coalesce_var(CLEANUP, params$cleanup)

root_path <- rprojroot::find_root("_IERG_ROOT_DIR_")

nsc_path <- file.path(root_path, "Data", "NSC")

# here::here() provides absolute paths so use fs::path_rel to get a relative path.
here_rel <- function(...) {fs::path_rel(here::here(...))}

project_path <- here_rel() # here::here()
input_path <- here_rel("input")
output_path <- here_rel("output")
scripts_path <- here_rel("scripts")

# Use the current year to determine the report time stamp and the folder for the data
report_time_str <- format(current_datetime,"%Y%m%d_%H%M")
report_year_folder <- str_c(current_year,as.integer(substring(current_year,3,4))+1,sep='-')

fn_report_code <- tolower(params$report_code)
fn_report_code2 <- tolower(params$report_code2)

getFN <- function(report_code) {
    return(stringr::str_glue("ipeds_{report_year}_{report_code}_{report_time_str}.txt"))
}
fn <- getFN(fn_report_code)
fn2 <- getFN(fn_report_code2)

cfg <- getCfg()

ipeds_unitid <- as.character(cfg$school$ipeds)

include_semesters <- coalesce_var(include_semesters,c("FA","SP","SU"))
