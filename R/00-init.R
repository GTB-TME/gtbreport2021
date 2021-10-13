#' ---
#' title: init GTB2019
#' author: Philippe Glaziou
#' date: 2021-06-03
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---
library(data.table)

load('../gtb2020/Rdata/est.Rdata')
load('../gtb2020/Rdata/cty.Rdata')
load('../gtb2020/Rdata/pop.Rdata')
load('../gtb2020/Rdata/vr.Rdata')
old <- copy(est)
vrcov <- vr[,.(iso3, year, vr.coverage, codqual)]


save(old, file = 'data/old.rda')
save(cty, file = 'data/cty.rda')
save(pop, file = 'data/pop.rda')
save(vrcov, file = 'data/vrcov.rda')
