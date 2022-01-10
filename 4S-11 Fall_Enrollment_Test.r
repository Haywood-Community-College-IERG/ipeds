###
### Define GLOBAL USER variables
###

fn_report_code <- "ef2"
report_year_data_adjustment <- 0

#OVERRIDE_REPORT_YEAR <- 2018 # Set to NA for current academic year's fall term

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

#######################################
#
# HERE IS WHERE THE DATA IS RETRIEVED
#
#######################################


#
# Get the fall enrolled students
#
fall_students <- fall_enrollment( report_year ) 

t <- fall_students %>%
    group_by( Status ) %>%
    summarise( Students = n(),
               Credits = sum(Credits) )

t2 <- fall_students %>%
    filter( Enrollment_Status != "Withdrawn" ) %>%
    group_by( Status ) %>%
    summarise( Students = n(),
               Credits = sum(Credits) )
