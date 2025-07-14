#' Global Variables
#'
#' This file declares global variables used in the package to avoid
#' R CMD check NOTEs about undefined global functions or variables.

# Declare global variables used with the `.` pronoun in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
