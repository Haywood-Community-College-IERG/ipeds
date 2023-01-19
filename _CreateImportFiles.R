library(quarto)

ipeds_completed <- c("COM","ICA")
#ipeds_collection <- "FA" # Must be "FA", "WI", or "SP"

coalesce_var <- function( var, default ) {
    sym <- deparse(substitute(var))
    env <- parent.frame()
    return(ifelse(exists(sym),var,default))
}

#ipeds_collection <- coalesce_var(ipeds_collection,"")

current_datetime <- lubridate::as_datetime(Sys.time())
current_date <- lubridate::as_date(current_datetime)
current_month <- as.numeric(format(current_date, "%m"))
current_year <- as.numeric(format(current_date, "%Y"))

ipeds_year <- current_year - ifelse(current_month < 7, 1, 0)

if (coalesce_var(ipeds_collection,"") == "") {
    if ( (current_year == ipeds_year) & (current_month < 11) ) {
        ipeds_collection <- "FA"
    } else if ( ((current_year == ipeds_year) & (current_month > 11)) |
                ((current_year > ipeds_year) & (current_month < 3)) ) {
        ipeds_collection <- "WI"
    } else if ( ((current_year == ipeds_year) & (current_month > 11)) |
                ((current_year > ipeds_year) & (current_month < 5)) ) {
        ipeds_collection <- "SP"
    }
}

ipeds_year <- as.character(ipeds_year)
current_datetime <- as.character(current_datetime)

if (ipeds_collection == "FA") {
    
    print( glue::glue("Creating import files for the {ipeds_year} FALL collection on {current_datetime}") )

    if (!("COM" %in% ipeds_completed)) {
        print( "... Generating Completions file" )
        quarto_render( "2F-03 Completions.qmd",
                       execute_params = list(
                                            report_year=ipeds_year,
                                            current_datetime=current_datetime
                                        ) )
    } else {
        print( "*** Completions file is complete" )
    }
    
    if (!("E12" %in% ipeds_completed)) {
        print( "... Generating 12-month Enrollment file" )
        quarto_render( "2F-04 12-month Enrollment.qmd", 
                       execute_params = list(
                                            report_year=ipeds_year,
                                            current_datetime=current_datetime
                                        ) )
    } else {
        print( "*** 12-month Enrollment file is complete" )
    }
    
    if (!("ICA" %in% ipeds_completed)) {
        print( "... Generating Institutional Characteristics file" )
        warning( "You need to manually complete 2F-02 Institutional Characteristics - Response.xlsm" )
    } else {
        print( "*** Institutional Characteristics file is complete" )
    }
    
} else if (ipeds_collection == "WI") {
    
    print( glue::glue("Creating import files for the {ipeds_year} WINTER collection") )
    
    quarto_render( "3W-05 Student_Financial_Aid.qmd", 
                   execute_params = list(
                                        report_year=ipeds_year,
                                        current_datetime=current_datetime
                                    ) )
    quarto_render( "3W-06,07 Graduation_Rates.qmd", 
                   execute_params = list(
                                        report_year=ipeds_year,
                                        current_datetime=current_datetime
                                    ) )
    quarto_render( "3W-09 Outcome_Measures.qmd", 
                   execute_params = list(
                                        report_year=ipeds_year,
                                        current_datetime=current_datetime
                                    ) )
    
} else if (ipeds_collection == "SP") {
    
    print( glue::glue("Creating import files for the {ipeds_year} SPRING collection") )
    
    quarto_render( "4S-11 Fall_Enrollment.qmd", 
                   execute_params = list(
                                        report_year=ipeds_year,
                                        current_datetime=current_datetime
                                    ) )

    warning( "You need to complete the coding of the Human Resources template" )

    warning( "You need to manually complete the Finance Excel Template" )
    
}
