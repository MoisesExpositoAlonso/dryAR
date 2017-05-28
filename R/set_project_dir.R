#' @name   set_project_dir
#' @title  Attempt to set project directory within Rmd files
#' @author Gene Leynes and Scott Rodgers
#'
#' @param project_name   Directory name of the current project
#'
#' @description
#' 		Used within Knitr (Rmd) files when knitting
#'
#' @details
#' 		Used within Knitr (Rmd) files when the report file is not at the top
#' 		level of the project.  It changes the directory to be up one level
#' 		while knitting the report so that it can find directories like
#' 		./data
#'
#' ## 2014-03-31   SJR   Extracted this function from 00_Initialize.R
#' ## 2014-05-05   GWL   Adding function to geneorama package
#' ##
#'
#' ##
#' ## Usage: Call this function within Rmd files if they are located higher than
#' ##        the project root directory. This will navigate up one directory until
#' ##        it reaches either the root directory or "project_name"
#' ##
#'

set_project_dir <- function(project_name='field'){
    while (basename(getwd()) != project_name &&
               basename(getwd()) != basename(normalizePath(".."))){
        setwd("..")
    }
}


