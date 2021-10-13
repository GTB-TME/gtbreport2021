#' ---
#' title: Data preparation, Global TB Report 2021
#' author: Philippe Glaziou
#' date: 19/07/2021
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

#' # Preamble
#' (Last updated: `r Sys.Date()`)
#'
#' Export files to GTB database
#'
library('data.table')
library('here')


source(here('report/dataprep.R'))  # uncomment whenever updated data should be released

pst <-
  list(
    'report/ch1.rmd',
    'report/ch1_text.rmd',
    'report/ch2-1.rmd',
    'report/ch2-1_text.rmd',
    'report/ch2-2.rmd',
    'report/ch2-2_text.rmd',
#    'report/ch2-3.rmd',
#    'report/ch2-3_text.rmd',
    'report/ch3.rmd'
  )

lapply(pst, function(x) rmarkdown::render(input = here(x), clean = FALSE, output_dir = here('report/html')))

