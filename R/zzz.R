#' @import R6
#' @import utils
#' @import checkmate
#' @import lgr
#' @import backports
#' @import httr2
#' @import jsonlite
#' @import httpuv
#' @import withr
#' @import data.table
#' @import callr
#' @import chromote
"_PACKAGE"


# mypackage/R/mypackage-package.R
.onLoad <- function(...){
  assign(
    "lg",
    lgr::get_logger(name = "saxapi"),
    envir = parent.env(environment())
  )
}
