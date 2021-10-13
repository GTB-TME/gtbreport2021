#' ---
#' title: Model output
#' author: Philippe Glaziou
#' date: 2021/06/07
#' output:
#'    html_document:
#'      mode: selfcontained
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      number_sections: true
#'      theme: flatly
#'      highlight: zenburn
#'      df_print: paged
#'      code_folding: hide
#' ---

#' Last updated: `r Sys.Date()`
#'
#'
#'
#' # Load libraries and data
#'

#' Load libraries and data
library(data.table)
library(readxl)
library(here)

load('data/old.rda')
load('data/mn.rda')
load('data/pop.rda')

source('R/fun.R')

# xlfn <- 'input/model/Outputs_20210620.xlsx'
# xlfn <- 'input/model/Outputs_20210708_corrected.xlsx'
# xlfn <- 'input/model/Outputs_20210726.xlsx'
xlfn <- 'input/model/Outputs_20210811.xlsx'

# format xl data
fmt <- function(M, snames) {
  setnames(M, new = snames)
  M[, index := rep(1:(nrow(M) / 6), each = 6)]

  M[1:12, iso3 := 'AGO']
  M[19:30, iso3 := 'BRA']
  M[49:60, iso3 := 'KEN']
  M[91:102, iso3 := 'UGA']
  M[(nrow(M)-12):nrow(M), iso3 := 'ZAF']

  M[c(13:18, 31:48, 61:90, 103:(nrow(M)-12)), iso3 := iso3[1], by = index]

  M[, index2 := rep(1:(nrow(M) / 3), each = 3)]
  M[, scenario := scenario[2], by = index2]

  M[, excess20 := gsub('--', '', excess20)]
  M[, excess2025 := gsub('--', '', excess2025)]
  M[, excess20 := as.numeric(excess20)]
  M[, excess2025 := as.numeric(excess2025)]

  M[iso3 == 'AGO', hiv := hiv[1], by = index]
  M[iso3 == 'BRA', hiv := hiv[1], by = index]
  M[iso3 == 'KEN', hiv := hiv[1], by = index]
  M[iso3 == 'UGA', hiv := hiv[1], by = index]
  M[iso3 == 'ZAF', hiv := hiv[1], by = index]

  M[, hiv := factor(hiv)]
  levels(M$hiv) <- c('a', 'pos')
  M[is.na(hiv), hiv := 'a']

  M[, index := NULL]
  M[, index2 := NULL]
  M[, country := NULL]
  return(M)
}

# incidence
M1 <-
  as.data.table(read_excel(xlfn,
                           sheet = 1,
                           skip = 1))
snames <- c(
  'country',
  'iso3',
  'hiv',
  'scenario',
  'stat',
  'excess20',
  'excess2025',
  as.character(as.Date(as.integer(names(
    M1
  )[8:67]), origin = "1899-12-30"))
)

M1 <- fmt(M1, snames)

# mortality
M2 <-
  as.data.table(read_excel(xlfn,
                           sheet = 2,
                           skip = 1))

M2 <- fmt(M2, snames)


# fix excel error
# M2[iso3=='UGA', hiv := c(rep('a', 6), rep('pos', 6))]


# reshape to long
fmt2 <- function(m) {
  .m <- melt(m, id.vars = c(1:6))
  .m[, excess20 := NULL]
  .m[, excess2025 := NULL]
  .m2 <- dcast(.m, ... ~ stat)
  return(.m2)
}

fmt2b <- function(m) {
  .mb <-
    m[scenario == 'Disruption', .(iso3, hiv, scenario, stat, excess20, excess2025)]
  .mb2 <- melt(.mb, id.vars = c(1:4))
  .mb3 <- dcast(.mb2, ... ~ stat)
}

m1 <- fmt2(M1)
m1b <- fmt2b(M1)

m2 <- fmt2(M2)
m2b <- fmt2b(M2)


# combine
setnames(m1, old = 'variable', new = 'date')
m1[, measure := 'inc']
setnames(m2, old = 'variable', new = 'date')
m2[, measure := 'mort']
m <-
  rbind(m1[, .(
    iso3,
    scenario,
    hiv,
    measure,
    date,
    best = Mid,
    lo = Low,
    hi = High
  )],
  m2[, .(
    iso3,
    scenario,
    hiv,
    measure,
    date,
    best = Mid,
    lo = Low,
    hi = High
  )])

mb <-
  rbind(m1b[, .(
    iso3,
    scenario,
    hiv,
    measure='inc',
    variable,
    best = Mid,
    lo = Low,
    hi = High
  )],
  m2b[, .(
    iso3,
    scenario,
    hiv,
    measure='mort',
    variable,
    best = Mid,
    lo = Low,
    hi = High
  )])


fwrite(m, file = here(paste0("csv/model_", Sys.Date(), '.csv')))
fwrite(mb, file = here(paste0("csv/model_excess_", Sys.Date(), '.csv')))



# times series plots
mm <- merge(m, old[year == 2019, .(iso3, country)], by = 'iso3')

# qplot(
#   as.Date(date),
#   best,
#   data = mm[measure == 'inc' &
#               hiv == 'a'],
#   colour = scenario,
#   geom = 'line'
# ) +
#   xlab('') + ylab('Monthly rate per 100,000') +
# #  expand_limits(y = 0) +
#   facet_wrap(~ country, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1') +
#   labs(title='TB incidence rate')
#
# qplot(
#   as.Date(date),
#   best,
#   data = mm[measure == 'mort' &
#               hiv == 'a'],
#   colour = scenario,
#   geom = 'line'
# ) +
#   xlab('') + ylab('Monthly rate per 100,000') +
#   # expand_limits(y = 0) +
#   facet_wrap(~ country, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1') +
#   labs(title='TB mortality rate (including HIV)')

# standardized
mm[ , rebest := best / best[1], by=.(iso3, scenario, hiv, measure)]
# qplot(
#   as.Date(date),
#   rebest * 100,
#   data = mm[measure == 'inc' &
#               hiv == 'a'],
#   colour = scenario,
#   geom = 'line'
# ) +
#   xlab('') + ylab('TB incidence change relative to January 2020 (%)') +
#   #  expand_limits(y = 0) +
#   facet_wrap(~ country) +
#   scale_color_brewer(palette = 'Set1') +
#   labs(title='Standardized TB incidence')
#
# qplot(
#   as.Date(date),
#   rebest * 100,
#   data = mm[measure == 'mort' &
#               hiv == 'a'],
#   colour = scenario,
#   geom = 'line'
# ) +
#   xlab('') + ylab('TB incidence change relative to January 2020 (%)') +
#   # expand_limits(y = 0) +
#   facet_wrap(~ country, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1') +
#   labs(title='Standardized TB mortality rate (including HIV)')
#





# aggregate by year
m[, sd := (hi - lo) / 3.92]
m[, year := substring(date, 1, 4)]

# sum of nearly identical rvs, we add cov
# Y = 12 * X, X ~ N(\mu, \sigma) then Y ~ N(12\mu, 12\sigma)
#
ma <-
  m[, .(best = sum(best), sd = sqrt(12 * sum(sd ^ 2))), by = .(iso3, scenario, hiv, measure, year)]



# rate ratios (disruption / baseline) including HIV
md <- merge(ma[scenario=='Baseline',.(iso3,year,measure,hiv,baseline=best,sd1=sd)],
             ma[scenario=='Disruption',.(iso3,year,measure,hiv,disrupt=best,sd2=sd)],
             by=c('iso3','year','measure','hiv'))

out <- with(md, divXY(disrupt, baseline, sd1, sd2))
md[, ratio := out[[1]]]
md[, ratio.sd := out[[2]]]
md[, year := as.integer(year)]

save(md, file=here('data/md.rda'))
fwrite(md, file = here(paste0('csv/md_', Sys.Date(), '.csv')))








# # predictive models
# #
# project <- function(y,
#                     time = 2017:2019,
#                     target = 2020) {
#   stopifnot(sum(!is.na(y)) >= 2)
#   stopifnot(y[!is.na(y)] >= 0 & y[!is.na(y)] <= 1)
#
#   period <- time[1]:target
#   yhat <-
#     predict(
#       glm(y ~ time, family = quasibinomial),
#       newdata = data.frame(time =
#                              period),
#       type = "response"
#     )
#   return(data.frame(time = period,
#                     y.hat = yhat))
# }
#
# trends <- old[year %in% 2017:2019 & g.income != 'HIC',.(iso3,year,inc,newinc,mort)]
# trends <- rbind(trends, trends[year==2019,.(iso3,year=2020,inc=NA,newinc=NA,mort=NA)])
# setkey(trends, iso3,year)
# trends[, inc.hat := project(inc[1:3]/1e5)$y.hat*1e5, by = iso3]
# trends[, newinc.hat := project(newinc[1:3]/1e5)$y.hat*1e5, by = iso3]
#
# mn2 <-
#   merge(mn[, .(iso3, c.newinc)], pop[year == 2020, .(iso3, pop)], by = 'iso3')
# mn2[, newinc := c.newinc / pop * 1e5]
#
# trends2 <- merge(trends, mn2[!is.na(newinc),.(iso3,year=2020,pnewinc=newinc)], by=c('iso3','year'), all.x=F)
# trends2[year==2020, newinc := pnewinc]
# trends2[, cdr := newinc/inc.hat]
# trends2[, nratio := newinc/newinc.hat]
# trends2[, pnewinc := NULL]
# trends2[, inc := NULL]
# trends2[, mort := NULL]
# trends3 <- merge(trends2, old[year==2019,.(iso3, tbhiv, onewinc=newinc, ocdr = newinc/inc)], by='iso3')
#
#
# # predict incidence ratio
# tinc <- merge(ma2, trends3, by=c('iso3','year'))
# tinc[, ccdr := cdr - ocdr]
#
# qplot(ratio, ccdr/ocdr, data=tinc)
# qplot(ratio, tbhiv, data=tinc)
# qplot(ratio, ccdr/ocdr, data=tinc, colour=tbhiv<.2)
#
# tinc[, chg.cdr := ccdr/ocdr]
#
# fit1 <- lm(ratio ~ ocdr + chg.cdr + tbhiv, data=tinc[measure=='inc'])
# (summary(fit1))
#
# fit1b <- stan_glm(ratio ~ ocdr + chg.cdr * tbhiv, data=tinc[measure=='inc'],
#                   family='gaussian')
# (summary(fit1b))
# plot(fit1b)
#
# fit1b |> mcmc_trace()
# fit1b |> rhat() |> mcmc_rhat() + yaxis_text()  # looks OK
#
# ypred <- posterior_predict(fit1b)
# plot(ypred)
#
#
#
#
# mort <- merge(tinc[measure=='inc',.(iso3,ocdr,cdr,ccdr,inc.ratio=ratio,tbhiv,nratio,nratio19 = newinc/onewinc)],
#               tinc[measure=='mort',.(iso3,mort.ratio=ratio)])
# mort[, chg.cdr := cdr / ocdr]
#
# qplot(nratio, mort.ratio, data=mort)
# qplot(nratio19, mort.ratio, data=mort)
# qplot(chg.cdr, mort.ratio, data=mort)
#
# fit2 <- lm(mort.ratio ~ ocdr + chg.cdr * tbhiv, data=mort)
# # fit2 <- lm(mort.ratio ~ nratio19 * tbhiv, data=mort)
# # fit2 <- lm(mort.ratio ~ ocdr + nratio + tbhiv, data=mort)
# # fit2 <- lm(mort.ratio ~ ocdr + nratio19 * tbhiv, data=mort)
# (summary(fit2))
#
# fit2b <- stan_glm(mort.ratio ~ chg.cdr * tbhiv, data=mort, family='gaussian')
#
# (summary(fit2b))
# plot(fit2b)
#
# fit2b |> mcmc_trace()
# fit2b |> rhat() |> mcmc_rhat() + yaxis_text()  # looks OK
#
# pp_check(fit2b, nreps=200) # posterior predictive checks
# # launch_shinystan(fit2b)
#
#
