#' ---
#' title: HIV among TB
#' author: Philippe Glaziou
#' date: 2021-06-29
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

#' (Last updated: `r Sys.Date()`)
#'
#' Prevalence of HIV among notified cases
#'

# Load libraries and data
#
suppressMessages(library(data.table))
suppressMessages(library(imputeTS))
suppressMessages(library(propagate))
# suppressMessages(library(rstanarm))
# suppressMessages(library(bayesplot))
library(here)

source(here('R/fun.R'))
load(here('data/tb.rda'))
load(here('data/cty.rda'))
load(here('data/unaids.rda'))
load(here('data/tbhiv.rda'))
load(here('data/pop.rda'))
load(here('data/grpmbr.rda'))
load(here('data/old.rda'))
load(here('data/md.rda'))

vlohi <- Vectorize(lohi, c('ev', 'sd'))
yr <- 2020
m <- 1e5




# incorporate IRQ update
irq <- fread('input/inc/upd.irq.csv')
rules <- fread('input/inc/rules_inc.csv')
setkey(rules, iso3)


irq[, inc.sd := (inc.hi - inc.lo) / 3.92]

old[iso3 == 'IRQ', inc := irq$inc]
old[iso3 == 'IRQ', inc.sd := irq$inc.sd]



# GNQ (comms in ~/doc)
# assume constant 40% CDR since 2004
old[iso3 == 'GNQ' & year >= 2004, inc := newinc / .4]
old[iso3 == 'GNQ' &
      year >= 2004, inc := imputeTS::na_interpolation(inc)]
old[iso3 == 'GNQ' & year < 2004, inc := 186.90] # 2004 value
old[iso3 == 'GNQ', inc.sd := inc * .2]
(old['GNQ', .(iso3, year, inc, inc.sd, newinc, cfr = newinc / inc, tbhiv)])



# Trends in inc and newinc before 2020
# as published in Global TB Report 2020,
# based on 2017-2019 series (excluding high income)
#
project <- function(y,
                    time = 2017:2019,
                    target = 2020) {
  stopifnot(sum(!is.na(y)) >= 2)
  stopifnot(y[!is.na(y)] >= 0 & y[!is.na(y)] <= 1)

  period <- time[1]:target
  yhat <-
    predict(
      glm(y ~ time, family = quasibinomial),
      newdata = data.frame(time =
                             period),
      type = "response"
    )

  if (any(y==0 & !is.na(y))) yhat <- mean(y, na.rm=TRUE)

  return(data.frame(time = period,
                    y.hat = yhat))
}

trends <-
  old[year %in% 2017:2019, .(iso3, year, inc, newinc, mort, mort.nh, mort.h)]
trends <-
  rbind(trends, trends[year == 2019, .(
    iso3,
    year = 2020,
    inc = NA,
    newinc = NA,
    mort = NA,
    mort.nh = NA,
    mort.h = NA
  )])
setkey(trends, iso3, year)
trends[, inc.hat := project(inc[1:3] / 1e5)$y.hat * 1e5, by = iso3]
trends[, mort.hat := project(mort[1:3] / 1e5)$y.hat * 1e5, by = iso3]
trends[, mort.nh.hat := project(mort.nh[1:3] / 1e5)$y.hat * 1e5, by = iso3]
trends[, mort.h.hat := project(mort.h[1:3] / 1e5)$y.hat * 1e5, by = iso3]

trends[, nnewinc := sum(!is.na(newinc)), by = iso3]
trends[nnewinc >= 2, newinc.hat := project(newinc[1:3] / 1e5)$y.hat * 1e5, by = iso3]
trends[nnewinc < 2, newinc.hat := mean(newinc, na.rm = TRUE, by = iso3)]




# new incidence series
#
(dim(tbhiv))
(dim(old))
est <-
  merge(
    tbhiv,
    old[, list(iso3, year, inc, inc.sd)],
    by = c('iso3', 'year'),
    all.x = TRUE,
    all.y = FALSE
  )
(dim(est))
est <-
  merge(
    est,
    pop[, .(iso3, year, pop = e.pop.num)],
    by = c('iso3', 'year'),
    all.x = TRUE,
    all.y = FALSE
  )
(dim(est))

# check missing values
sum(is.na(est$inc) &
      est$year < yr) == 0   # TRUE: only year==yr inc values are missing

(sum(is.na(est$newinc)))
(sum(is.na(est$newinc) & est$year == yr))



# add trends
#
est <-
  merge(est, trends[year == 2020, .(iso3,
                                    year,
                                    inc.hat,
                                    newinc.hat,
                                    mort.hat,
                                    mort.nh.hat,
                                    mort.h.hat)], by = c('iso3', 'year'), all.x = TRUE)
(dim(est))



# add country groupings
#
income <- grpmbr[group.type == 'g_income']
setnames(income, 'group.name', 'g.income')
est <-
  merge(est, income[, .(iso3, g.income)], by = 'iso3', all.x = TRUE)
est$g.income <- as.character(est$g.income)

gbd <- grpmbr[group.type == 'g_gbd']
setnames(gbd, 'group.name', 'g.gbd')
est <- merge(est, gbd[, .(iso3, g.gbd)], by = 'iso3', all.x = TRUE)

gest <- grpmbr[group.type == 'g_est']
eeur <- gest[group.name == 'EEUR', iso3]
est[, g.mdr := g.whoregion]
est[iso3 %in% eeur, g.mdr := 'EEU']

hbc <- as.character(grpmbr[group.type == 'g_hb_tb']$iso3)
est$g.hbc <- est$iso3 %in% hbc
hbmdr <- as.character(grpmbr[group.type == 'g_hb_mdr']$iso3)
est$g.hbmdr <- est$iso3 %in% hbmdr
hbtbhiv <- as.character(grpmbr[group.type == 'g_hb_tbhiv']$iso3)
est$g.hbtbhiv <- est$iso3 %in% hbtbhiv



# shortfall in 2020 notifs vs 2019
dim(est)
est <-
  merge(est, tb[year == yr - 1, .(iso3, newinc2019 = newinc)], by = 'iso3', all.x =
          TRUE)
dim(est)

(est[year == yr &
       newinc2019 > 0, weighted.mean(newinc / newinc2019, w = pop, na.rm = TRUE), by =
       g.income])
(est[year == yr &
       newinc2019 > 0 &
       g.income != 'HIC', weighted.mean(newinc / newinc2019, w = pop, na.rm =
                                          TRUE)])
(est[year == yr &
       newinc2019 > 0 &
       g.income != 'HIC', weighted.sd(newinc / newinc2019, w = pop, na.rm = TRUE)])

est[year == yr & newinc2019>0, short :=  newinc / newinc2019]
est[year==yr, summary(short)]



# predictive models of incidence
#
(dim(md))

# HIV+ and HIV-
md2 <-
  merge(md[hiv == 'a' &
             year == 2020 &
             measure == 'inc'], est[year == 2020 , .(iso3,
                                                     pop,
                                                     short,
                                                     cdr.base = newinc / inc.hat,
                                                     tbhiv)],
        by = 'iso3')

# replace missing 2020 tbhiv with 2019 value
for (i in c('BGD', 'IDN', 'PHL'))
  md2[iso3 == i, tbhiv := old[iso3 == i, last(tbhiv)]]

fit0 <- lm(ratio ~ cdr.base * short * tbhiv, data = md2)
fit <-
  step(fit0,
       direction = 'backward',
       scope = formula(fit0),
       trace = 0)
(summary(fit))

md.lst <- unique(md$iso3)

md2[, .(mean(short), mean(ratio), sd(ratio))]
est[year == 2020 &
      newinc > 0 &
      iso3 %ni% md.lst &
      g.income != 'HIC', mean(short, na.rm = T)]

# shorfall in modelled countries similar to that of non-modelled countries not in the
# group of HIC
#

#---------------------------------------------------------------
# decision 27/07/2021 assume inc ratio = 1.02, uncertainty 0.005
#---------------------------------------------------------------
inc.ratio <- 1.02
inc.ratio.sd <- 0.026








# check for outliers in newinc
est[, imp.newinc := newinc] # will hold imputed values

# check for outliers
#
(est[pop > 1e5, .(outlier = sum(imp.newinc > 3 * mean(imp.newinc, na.rm =
                                                        T)) > 0), by = iso3][outlier == T])
(est['STP', .(iso3, year, imp.newinc)])
sel <- est$iso3 == "STP" & est$year == 2003
est$imp.newinc[sel] <- NA # reset outlier to missing
(est['MDA', .(iso3, year, imp.newinc)])
sel <- est$iso3 == "MDA" & est$year == 2003
est$imp.newinc[sel] <- NA # reset outlier to missing
sel <- est$iso3 == "KGZ" & est$year == 2003
est$imp.newinc[sel] <- NA # reset outlier to missing

# list outliers in the notification series in countries with pop>1e5,
#
(est[pop > 1e5, .(outlier = sum(imp.newinc == 0)), by = iso3][outlier ==
                                                                T])

# KHM (Aug - NTP mentioned that 2014 peak due to 5000 false pos in children)
#
(est['KHM', .(iso3, year, imp.newinc)])
sel <- est$iso3 == 'KHM' & est$year == 2014
est$imp.newinc[sel] <- NA # reset outlier to missing


# Interpolation of missing notifications
# using Kalman smoothing on structural TS, where possible
#
B1 <- copy(est)  # backup point

interp <- c('SMR', 'MSR', 'VGB')

est[iso3 %in% interp, imp.newinc := na_interpolation(imp.newinc), by = iso3]
est[iso3 %ni% interp, imp.newinc := na_kalman(imp.newinc, type = 'trend'), by = iso3]

est[, test.ispos(imp.newinc)]



# check imputations
#
wr <- c('AMR', 'AFR', 'EMR', 'EUR', 'SEA', 'WPR')

for (i in wr) {
  p <-
    qplot(year, newinc, data = est[g.whoregion == i], geom = 'point') +
    geom_line(aes(year, imp.newinc), colour = I('red')) +
    facet_wrap( ~ iso3, scales = 'free_y')
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('output/checks/imputations', i, '_newinc.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}






# Incidence in HIC
# Standard adjustment
std <-
  function(x,
           ISO3,
           f.lo = 1,
           f.hi,
           ystart = 2000,
           h = 1,
           source = 'Standard adjustment',
           smooth = FALSE) {
    #' $I = f N$
    #'
    #' @param x datatable
    #' @param country iso3
    #' @param f low bound
    #' @param f.hi high bound
    #' @param h 1 - over diagnosis
    #' @param smooth MA with exponential weighting
    #' @export
    f <- mean(c(f.hi, f.lo))
    sel <- x$iso3 == ISO3 & x$year >= ystart

    if (smooth == FALSE) {
      x[sel, inc := imp.newinc * f * h]
    } else {
      x[sel, inc := na_ma(imp.newinc, k = 4, weighting = 'exponential') * f * h]
    }
    x[sel, inc.sd := inc * (f.hi - 1) / 3.92]
    x[sel, source.inc := source]
  }

hic <- est[g.income == 'HIC', unique(iso3)]
hic <- c(hic, 'ANT', 'WLF')                 # high-income countries
nhic <- setdiff(est[, unique(iso3)], hic)   # not high-income

B2 <- copy(est) # another backup point

for (i in hic) {
  # inefficient, but runs fast enough
  est <-
    std(
      est,
      ISO3 = i,
      f.lo = rules[i, lo],
      f.hi = rules[i, hi],
      ystart = rules[i, ystart],
      h = 1,
      source = 'Case notifications, Standard adjustment',
      smooth = ifelse(rules[i, hi] >= 1.5 &
                        !is.na(rules[i, hi]), TRUE, FALSE)
    )
}

est[!is.na(inc), test.ispos(inc)]
est[is.na(inc) & year == 2020, .(iso3, year, inc, newinc, inc.hat)]








# use inc.hat x predicted inc ratio in other countries with a shortfall
# (see decision about inc ratio above)
# inflate uncertainty accordingly
sel <- is.na(est$inc) & est$year == yr & est$iso3 %ni% md.lst
(table(sel))

est[sel, inc := inc.hat]
est[sel, source.inc := "Predicted incidence ratio"]

(sum(is.na(est$inc)) == 0)

setkey(est, iso3, year)

dim(est)
est <-
  merge(est, est[year == yr - 1, .(iso3, inc2019 = inc)], by = 'iso3', all.x =
          TRUE)
dim(est)

est[iso3 %in% nhic, inc.sd := imputeTS::na_locf(inc.sd) * inc / inc2019, by = iso3]
(est['ZAF', .(iso3, year, inc, inc.sd)])
(est['NGA', .(iso3, year, inc, inc.sd)])
sum(is.na(est$inc.sd))
(est[is.na(inc.sd), .(iso3, year, inc, inc.sd)])
est[is.na(inc.sd), inc.sd := inc * .2]
(sum(is.na(est$inc.sd)) == 0)



sel <- est$iso3 %in% nhic & est$iso3 %ni% md.lst & est$year == yr & est$short < 1
table(sel)
out <-
  with(est[sel], prodXY(inc, inc.ratio, inc.sd, inc.ratio.sd))

est[sel, inc := out$mean]
est[sel, inc.sd := out$sd]







# use inc.hat in low & middle income countries with no shortfall
sel <- est$iso3 %in% nhic & est$iso3 %ni% md.lst & est$year == yr & est$short >= 1
table(sel)
est[sel, inc := inc.hat]
est[sel, source.inc := "Current trends"]








# carry over 2019 values where an increase looks doubtful
lst <-
  c('FJI', 'MHL', 'TUV', 'BGD', 'BTN', 'LBY', 'LBN', 'SUR', 'COK')
(est['FJI', .(iso3, year, inc, inc.sd)])
est[iso3 %in% lst, inc := c(inc[1:length(inc) - 1], inc[length(inc) - 1]), by =
      iso3]
est[iso3 %in% lst, inc.sd := c(inc.sd[1:length(inc.sd) - 1], inc.sd[length(inc.sd) -
                                                                      1]), by = iso3]
(est['FJI', .(iso3, year, inc, inc.sd)])







# use model output in modeled countries
B3 <- copy(est)
dim(est)
est <-
  merge(est, md[measure == 'inc' &
                  hiv == 'a' &
                  year == yr, .(iso3, year, inc.md = disrupt, inc.md.sd = sd2)],
        by = c('iso3', 'year'), all.x = TRUE)
dim(est)

est[iso3 %in% md.lst & year == yr, inc := inc.md]
est[iso3 %in% md.lst & year == yr, inc.sd := inc.md.sd]
est[iso3 %in% md.lst & year == yr, source.inc := "Model"]

(sum(is.na(est$inc)) == 0)


# plot modeled inc
p <- qplot(year,
           inc,
           data = subset(est, iso3 %in% unique(md$iso3)),
           geom = 'line') +
  geom_ribbon(
    aes(
      year,
      ymin = inc - 1.96 * inc.sd,
      ymax = inc + 1.96 * inc.sd
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('output/checks/inc_model.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))







# Comparison plots with last year's report, focus on recent trends
#
for (i in wr) {
  p <- qplot(
    year,
    inc,
    data = subset(est, g.whoregion == i & year > 2013),
    geom = 'line',
    colour = I('grey90')
  ) +
    geom_ribbon(
      aes(
        year,
        ymin = inc - 1.96 * inc.sd,
        ymax = inc + 1.96 * inc.sd
      ),
      fill = I('blue'),
      alpha = I(.4)
    ) +
    geom_line(
      aes(year, inc),
      data = subset(old, g.whoregion == i & year > 2013),
      colour = I('red'),
      linetype = I(2)
    ) +
    geom_line(aes(year, newinc)) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')

  suppressWarnings(ggsave(
    here(paste(
      'output/checks/inc', i, '_compare.pdf', sep = ''
    )),
    plot = p,
    width = 14,
    height = 8
  ))
}






# Indirect estimation of HIV prev in TB
#
# the rest of missing values is imputed using
# a prediction model based on UNAIDS estimates of the
# prevalence of HIV in the general population
B4 <- copy(est)
(dim(est))
est <-
  merge(
    est,
    unaids[, .(
      iso3,
      year,
      hiv.num,
      hiv.lo.num,
      hiv.hi.num,
      mort.hiv.num,
      mort.hiv.lo.num,
      mort.hiv.hi.num
    )],
    by = c('iso3', 'year'),
    all.x = T,
    all.y = F
  )
(dim(est))

est[, hiv := hiv.num / pop]
est[, hiv.lo := hiv.lo.num / pop]
est[, hiv.hi := hiv.hi.num / pop]
est[, hiv.sd := (hiv.hi - hiv.lo) / 3.92]

est[, mort.hiv := mort.hiv.num / pop * m]
est[, mort.hiv.lo := mort.hiv.lo.num / pop * m]
est[, mort.hiv.hi := mort.hiv.hi.num / pop * m]
est[, mort.hiv.sd := (mort.hiv.hi - mort.hiv.lo) / 3.92]




# inc.h
inc.h <- with(est, prodXY(inc, tbhiv, inc.sd, tbhiv.sd))
est$inc.h <- inc.h[[1]]
est$inc.h.sd <- inc.h[[2]]


# inc.nh
inc.nh <-
  with(est, prodXY(inc, (1 - tbhiv), inc.sd, tbhiv.sd))
est$inc.nh <- inc.nh[[1]]
est$inc.nh.sd <- inc.nh[[2]]


# force of infection in HIV+
fi.h <-
  with(est, divXY(inc.h / m, hiv, (inc.h.sd / m), hiv.sd))
est$fi.h <- fi.h[[1]]
est$fi.h.sd <- fi.h[[2]]


# force of infection in HIV-
fi.nh <-
  with(est, divXY(inc.nh / m, (1 - hiv), (inc.nh.sd / m), hiv.sd))
est$fi.nh <- fi.nh[[1]]
est$fi.nh.sd <- fi.nh[[2]]


# incidence rate ratio, ignoring covariance
irr <- with(est, divXY(fi.h, fi.nh, fi.h.sd, fi.nh.sd))
est$irr <- irr[[1]]
est$irr.sd <- irr[[2]]
sel <- !is.na(est$irr) & est$irr > 1e3
table(sel)
est[sel, .(iso3, year, inc, tbhiv, inc.nh, inc.h, hiv, fi.h, fi.nh, irr)]
est$irr[sel] <- est$irr.sd[sel] <- NA

sel <-
  est$irr.sd == 0 & est$hiv > 0 &
  !is.na(est$irr) & !is.na(est$irr.sd)
table(sel)
est$irr[sel] <- NA
est$irr.sd[sel] <- NA
lst <- unique(as.character(est$iso3[sel]))

est[iso3 %in% lst, irr := na.interpolation(irr), by = iso3]
est[iso3 %in% lst, irr.sd := na.interpolation(irr.sd), by = iso3]




# check IRR series
#
# for (i in wr) {
#   p <-
#     qplot(
#       year,
#       0,
#       data = subset(est, g.whoregion == i),
#       geom = 'line',
#       colour = I('grey90')
#     ) +
#     geom_line(aes(year, irr)) +
#     geom_ribbon(
#       aes(
#         year,
#         ymin = pmax(irr - 1.96 * irr.sd, 0),
#         ymax = irr + 1.96 * irr.sd
#       ),
#       fill = I('blue'),
#       alpha = I(.4)
#     ) +
#     facet_wrap( ~ iso3, scales = 'free_y') + xlab('') + ylab('IRR')
#   suppressWarnings(print(p))
# }




# tbhiv based on IRR
#
(est[, .(median(irr, na.rm = T), mean(irr, na.rm = T)), by = year])
# use last non-missing r = irr backwards in time
#
# $t = \frac{h r}{1 + h (r - 1)}$
#

B5 <- copy(est)

lcty <- unique(as.character(est$iso3))

est[!is.na(tbhiv), test.isbinom(tbhiv)]
est[!is.na(irr), test.ispos(irr)]



# no irr, impute it
#
est[, ghiv := hiv > 0.1]
est[is.na(ghiv), ghiv := F]
est[, hincome := g.income == 'HIC']
est[is.na(hincome), hincome := T]

out <-
  est[, .(n = .N, irr = weighted.mean(irr, w = pop, na.rm = TRUE)),
      by = list(year, ghiv, hincome)]

# out <-
#   est[, .(n = .N, irr = weighted.mean((inc * tbhiv / hiv) / (inc * (1 - tbhiv) /
#                                                                (1 - hiv)), w = pop, na.rm = TRUE),
#           irr.sd = sd(irr)),
#       by = list(year, ghiv, hincome)]
(out)

out[is.infinite(irr), irr := NA]
out[is.nan(irr), irr := NA]
# out[24, irr := NA]
out[, irr := na_interpolation(irr), by = list(ghiv, hincome)]

# mirr <- out[ghiv == F & hincome == T, mean(irr, na.rm = T)]
# out[is.na(irr), irr := mirr]
(out)

# use g.income and generalized HIV as predictors of IRR
#
(dim(est))
est <- merge(est,
             out[, .(year, ghiv, hincome, e.irr = irr)],
             by = c('year', 'ghiv', 'hincome'),
             all.x = T)
(dim(est))
setkey(est, iso3, year)


# one IRR value available
#
est[, n.irr := sum(!is.na(irr) & irr > 0), by = iso3]
est[n.irr == 1, f.irr := irr / e.irr]
est[n.irr == 1, f.irr.sd := irr.sd / irr]
est[n.irr == 1, f.irr := max(f.irr, na.rm = TRUE), by = iso3]
est[n.irr == 1, f.irr.sd := max(f.irr.sd, na.rm = TRUE), by = iso3]
est[n.irr == 1, irr := e.irr * f.irr]
est[n.irr == 1, irr.sd := irr * f.irr.sd]


# multiple IRR available (todo: fix this hack)
#
est[n.irr > 1, f.irr := mean(irr, na.rm = T) / mean(e.irr, na.rm = T)]
est[n.irr > 1, f.irr.sd := mean(irr.sd, na.rm = T) / mean(irr, na.rm = T)]
est[n.irr > 1, f.irr := max(f.irr, na.rm = TRUE), by = iso3]
est[n.irr > 1, f.irr.sd := max(f.irr.sd, na.rm = TRUE), by = iso3]
est[n.irr > 1, irr := e.irr * f.irr]
est[n.irr > 1, irr.sd := irr * f.irr.sd]

# no IRR available
#
est[n.irr == 0, irr := e.irr]
est[n.irr == 0, irr.sd := e.irr * 0.25]


est[!is.na(irr), test.ispos(irr)]


est[, n.irr := NULL]
est[, f.irr := NULL]
est[, f.irr.sd := NULL]
est[is.infinite(irr.sd), irr.sd := irr * .25]
est[is.na(hiv.sd) & !is.na(hiv), hiv.sd := hiv * .25]

# e.tbhiv using imputed IRR
#
sel <- !is.na(est$irr) & is.na(est$irr.sd)
table(sel)
# est$irr.sd[sel] <- .25 * est$irr[sel] # arbitrary
# est[irr.sd > irr * .25, irr.sd := irr * .25]   # as well

excl <- c('BLZ')
sel <-
  !is.na(est$irr) & !is.na(est$hiv) & est$hiv > 0 &
  est$iso3 %ni% excl
table(sel)

out <- est[sel, {
  tmp = h2t(hiv, hiv.sd, irr, irr.sd)$prop

  list(e.tbhiv = tmp[2],
       e.tbhiv.sd = tmp[4])
},
by = .(iso3, year)]


# out[is.infinite(e.tbhiv), e.tbhiv := NA]
# out[is.infinite(e.tbhiv.sd), e.tbhiv.sd := NA]

# smooth e.tbhiv series
out[, e.tbhiv := predict(loess(e.tbhiv ~ year, span = 0.6)), by = iso3]
out[, e.tbhiv.sd := predict(loess(e.tbhiv.sd ~ year, span = 0.6)), by =
      iso3]
out[e.tbhiv < 0, e.tbhiv := 0]
out[e.tbhiv.sd < 0, e.tbhiv.sd := 0]

out[!is.na(e.tbhiv), test.isbinom(e.tbhiv)]
out[!is.na(e.tbhiv), test.isbinom(e.tbhiv.sd)]

est$e.tbhiv[sel] <- out$e.tbhiv
est$e.tbhiv.sd[sel] <- out$e.tbhiv.sd





# impute missing tbhiv with e.tbhiv
#
sel <- is.na(est$tbhiv) & !is.na(est$e.tbhiv)
table(sel)
est$tbhiv[sel] <- est$e.tbhiv[sel]
est$tbhiv.sd[sel] <- est$e.tbhiv.sd[sel]

est[!is.na(tbhiv), test.isbinom(tbhiv)]
est[!is.na(tbhiv.sd), test.isbinom(tbhiv.sd)]

sum(is.na(est$tbhiv))
sum(is.na(est$tbhiv.sd))




# fixes
#
sel <- est$iso3 == 'KHM' & est$year %in% 2000:2009
sel2 <- old$iso3 == 'KHM' & old$year %in% 2000:2009
est$tbhiv[sel] <- old$tbhiv[sel2]
est$tbhiv.sd[sel] <- old$tbhiv.sd[sel2]

incl <- c('ZAF', 'ZMB', 'ZWE')
sel <-
  est$year < 2010 & is.na(est$tbhiv.routine.ok) & est$iso3 %in% incl
table(sel)
est$tbhiv[sel] <- est$e.tbhiv[sel]


# retrieve old source.inc
dim(est)
est <-
  merge(est, old[year == 2019, .(iso3, old.source.inc = source.inc)], by =
          'iso3', all.x = TRUE)
dim(est)




# check imputed series
#
for (i in wr) {
  p <-
    qplot(
      year,
      0,
      data = subset(est, g.whoregion == i),
      geom = 'line',
      colour = I('grey90')
    ) +
    geom_line(aes(year, tbhiv)) +
    geom_ribbon(
      aes(
        year,
        ymin = tbhiv - 1.96 * tbhiv.sd,
        ymax = tbhiv + 1.96 * tbhiv.sd
      ),
      fill = I('blue'),
      alpha = I(.4)
    ) +
    geom_point(aes(year, tbhiv.routine),
               colour = I('black'),
               shape = I(4)) +
    geom_point(aes(year, tbhiv.routine.ok),
               colour = I('blue'),
               shape = I(4)) +
    geom_point(aes(year, tbhiv.surv),
               colour = I('green'),
               shape = I(2)) +
    geom_point(aes(year, tbhiv.sentin),
               colour = I('red'),
               shape = I(3)) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('HIV prevalence in TB')
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('output/checks/imputed_tbhiv_', i, '.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}



# compare with last year
#
for (i in wr) {
  p <-
    qplot(
      year,
      0,
      data = subset(est, g.whoregion == i),
      geom = 'line',
      colour = I('grey90')
    ) +
    geom_ribbon(
      aes(
        year,
        ymin = tbhiv - 1.96 * tbhiv.sd,
        ymax = tbhiv + 1.96 * tbhiv.sd
      ),
      fill = I('blue'),
      alpha = I(.4)
    ) +
    geom_line(
      aes(year, tbhiv),
      data = subset(old, g.whoregion == i),
      colour = I('red'),
      linetype = I(2)
    ) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('HIV prevalence in TB')
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('output/checks/tbhiv', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}



# global TBHIV
#
(est[, weighted.mean(tbhiv, w = inc * pop / 1e5, na.rm = T), by = year])

# inc.h
inc.h <- with(est, prodXY(inc, tbhiv, inc.sd, tbhiv.sd))
est$inc.h <- inc.h[[1]]
est$inc.h.sd <- inc.h[[2]]

# inc.nh
inc.nh <-
  with(est, prodXY(inc, (1 - tbhiv), inc.sd, tbhiv.sd))
est$inc.nh <- inc.nh[[1]]
est$inc.nh.sd <- inc.nh[[2]]

# force of infection in HIV+
fi.h <-
  with(est, divXY(inc.h / m, hiv, (inc.h.sd / m), hiv.sd))
est$fi.h <- fi.h[[1]]
est$fi.h.sd <- fi.h[[2]]

# force of infection in HIV-
fi.nh <-
  with(est, divXY(inc.nh / m, (1 - hiv), (inc.nh.sd / m), hiv.sd))
est$fi.nh <- fi.nh[[1]]
est$fi.nh.sd <- fi.nh[[2]]

# incidence rate ratio, ignoring covariance
irr <- with(est, divXY(fi.h, fi.nh, fi.h.sd, fi.nh.sd))
est$irr <- irr[[1]]
est$irr.sd <- irr[[2]]
sel <- !is.na(est$irr) & est$irr > 1e3
table(sel)

(est[sel, .(iso3, year, inc, tbhiv, inc.nh, inc.h, hiv, fi.h, fi.nh, irr)])
est$irr[sel] <- est$irr.sd[sel] <- NA




# bounds
#
B6 <- copy(est)

sel1 <- est$inc > 0
table(sel1)
table(sel1 &
        (est$inc.sd / 1e5) ^ 2 >= est$inc / 1e5 * (1 - est$inc / 1e5))
est[sel1 &
      (est$inc.sd / 1e5) ^ 2 >= est$inc / 1e5 * (1 - est$inc / 1e5), .(iso3, year, inc, inc.sd)]
est[sel1 &
      (est$inc.sd / 1e5) ^ 2 >= est$inc / 1e5 * (1 - est$inc / 1e5), inc.sd := inc * 0.2]

out1 <- vlohi(est$inc[sel1] / m, est$inc.sd[sel1] / m)

sel2 <- est$inc.nh > 0 & !is.na(est$inc.nh)
table(sel2)
table(sel2 &
        (est$inc.nh.sd / 1e5) ^ 2 >= est$inc.nh / 1e5 * (1 - est$inc.nh / 1e5))
est[sel2 &
      (est$inc.nh.sd / 1e5) ^ 2 >= est$inc.nh / 1e5 * (1 - est$inc.nh / 1e5), .(iso3, year, inc.nh, inc.nh.sd)]
est[sel2 &
      (est$inc.nh.sd / 1e5) ^ 2 >= est$inc.nh / 1e5 * (1 - est$inc.nh / 1e5), inc.nh.sd := inc.nh * 0.2]
out2 <- vlohi(est$inc.nh[sel2] / m, est$inc.nh.sd[sel2] / m)

sel3 <-
  est$inc.h > 0 &
  !is.na(est$inc.h) & est$inc.h.sd > 0 & !is.na(est$inc.h.sd)
table(sel3)
table(sel3 &
        (est$inc.h.sd / 1e5) ^ 2 >= est$inc.h / 1e5 * (1 - est$inc.h /
                                                         1e5))
est[sel3 &
      (est$inc.h.sd / 1e5) ^ 2 >= est$inc.h / 1e5 * (1 - est$inc.h / 1e5), .(iso3, year, inc.h, inc.h.sd)]
est[sel3 &
      (est$inc.h.sd / 1e5) ^ 2 >= est$inc.h / 1e5 * (1 - est$inc.h / 1e5), inc.h.sd := inc.h * 0.2]
out3 <- vlohi(est$inc.h[sel3] / m, est$inc.h.sd[sel3] / m)

est$inc.lo[sel1] <- out1[1,] * m
est$inc.hi[sel1] <- out1[2,] * m
est$inc.lo[!sel1] <- est$inc.hi[!sel1] <- est$inc[!sel1]

est$inc.nh.lo[sel2] <- out2[1,] * m
est$inc.nh.hi[sel2] <- out2[2,] * m
est$inc.nh.lo[!sel2 & est$inc.nh == 0 & est$inc.nh.sd == 0] <- 0
est$inc.nh.hi[!sel2 & est$inc.nh == 0 & est$inc.nh.sd == 0] <- 0

est$inc.h.lo[sel3] <- out3[1,] * m
est$inc.h.hi[sel3] <- out3[2,] * m
est$inc.h.lo[!sel3 & est$inc.h == 0 & est$inc.h.sd == 0] <- 0
est$inc.h.hi[!sel3 & est$inc.h == 0 & est$inc.h.sd == 0] <- 0

sel4 <-
  (est$inc.h.lo > est$inc.h) |
  (est$inc.h.hi < est$inc.h) &
  (!is.na(est$inc.h) & !is.na(est$inc.h.lo) & !is.na(est$inc.h.hi))
table(sel4)
est[sel4, .(iso3, year, inc, inc.h, inc.h.sd, inc.h.lo, inc.h.hi)]
est[sel4, inc.h.lo := 0]
est[sel4, inc.h.hi := inc.h + 1.96 * inc.h.sd]

sel <-
  (est$inc.nh.lo > est$inc.nh) |
  (est$inc.nh.hi < est$inc.nh) &
  (!is.na(est$inc.nh) &
     !is.na(est$inc.nh.lo) & !is.na(est$inc.nh.hi))
table(sel)
est[sel, .(iso3, year, inc, inc.nh, inc.nh.sd, inc.nh.lo, inc.nh.hi)]
# est[sel, inc.nh.hi := inc.nh + 1.96 * inc.nh.sd]

est[is.na(inc.nh.lo), inc.nh.lo := 0]
est[is.na(inc.nh.hi), inc.nh.hi := inc.nh + 1.96 * inc.nh.sd]

sel <- !is.na(est$inc.h)
est[sel & is.na(inc.h.lo), inc.h.lo := 0]
est[sel & is.na(inc.h.hi), inc.h.hi := inc.h + 1.96 * inc.h.sd]


sel <- est$tbhiv > 0 & est$tbhiv < 1 & !is.na(est$tbhiv)
table(sel)
table(sel & (est$tbhiv.sd) ^ 2 >= est$tbhiv * (1 - est$tbhiv))
est[sel &
      (est$tbhiv.sd) ^ 2 >= est$tbhiv * (1 - est$tbhiv), .(iso3, year, tbhiv, tbhiv.sd)]
est[sel &
      (est$tbhiv.sd) ^ 2 >= est$tbhiv * (1 - est$tbhiv), tbhiv.sd := tbhiv * 0.2]

out <- vlohi(est$tbhiv[sel], est$tbhiv.sd[sel])
est$tbhiv.lo[sel] <- out[1,]
est$tbhiv.hi[sel] <- out[2,]


sel <- est$tbhiv == 0 & !is.na(est$tbhiv)
table(sel)
est$tbhiv.lo[sel] <- 0
est$tbhiv.hi[sel] <- est$tbhiv.sd[sel] * 1.96

sel <- est$tbhiv == 1 & !is.na(est$tbhiv)
table(sel)
est$tbhiv.hi[sel] <- 1
est$tbhiv.lo[sel] <- pmax(1 - est$tbhiv.sd[sel] * 1.96, 0)

est[tbhiv.hi < tbhiv, .(iso3, tbhiv, tbhiv.sd, tbhiv.lo, tbhiv.hi)]
est[tbhiv.hi < tbhiv, tbhiv.hi := tbhiv + 1.96 * tbhiv.sd]

# checks
#
est[, test.isbinom(inc / m)]
est[!is.na(inc.nh), test.isbinom(inc.nh / m)]
est[!is.na(tbhiv), test.isbinom(tbhiv)]
est[!is.na(inc.h), test.isbinom(inc.h / m)]

est[, test.bounds(inc, inc.lo, inc.hi)]
est[!is.na(inc.nh), test.bounds(inc.nh, inc.nh.lo, inc.nh.hi)]
est[!is.na(inc.h), test.bounds(inc.h, inc.h.lo, inc.h.hi)]
est[!is.na(tbhiv), test.bounds(tbhiv, tbhiv.lo, tbhiv.hi)]

est[!is.na(inc.h), sum(abs(inc.h + inc.nh - inc) > 1) == 0]
est[!is.na(inc.h), test.ispos(inc.h)]
est[!is.na(inc.h), test.ispos(inc.h.sd)]
est[!is.na(inc.h), test.ispos(inc.nh)]
est[!is.na(inc.h), test.ispos(inc.nh.sd)]
est[!is.na(irr), test.ispos(irr)]



# Comparison plots with focus on recent trends, HIV+ incidence
for (i in wr) {
  p <- qplot(
    year,
    inc.h,
    data = subset(est, g.whoregion == i & year > 2013),
    geom = 'line',
    colour = I('grey90')
  ) +
    geom_ribbon(
      aes(
        year,
        ymin = inc.h - 1.96 * inc.h.sd,
        ymax = inc.h + 1.96 * inc.h.sd
      ),
      fill = I('blue'),
      alpha = I(.4)
    ) +
    geom_line(
      aes(year, inc.h),
      data = subset(old, g.whoregion == i & year > 2013),
      colour = I('red'),
      linetype = I(2)
    ) +
    facet_wrap( ~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('output/checks/inc.h_', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}


# Comparison plots with last year's global TB report
for (i in wr) {
  p <- qplot(
    year,
    hiv,
    data = subset(est, g.whoregion == i),
    geom = 'line',
    colour = I('grey90')
  ) +
    geom_ribbon(
      aes(
        year,
        ymin = hiv - 1.96 * hiv.sd,
        ymax = hiv + 1.96 * hiv.sd
      ),
      fill = I('blue'),
      alpha = I(.4)
    ) +
    geom_line(
      aes(year, hiv),
      data = subset(old, g.whoregion == i),
      colour = I('red'),
      linetype = I(2)
    ) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('HIV prevalence')
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('output/checks/hiv', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}



# Comparison plots with last year's global TB report
for (i in wr) {
  p <- qplot(
    year,
    tbhiv,
    data = subset(est, g.whoregion == i),
    geom = 'line',
    colour = I('grey90')
  ) +
    geom_ribbon(
      aes(
        year,
        ymin = tbhiv - 1.96 * tbhiv.sd,
        ymax = tbhiv + 1.96 * tbhiv.sd
      ),
      fill = I('blue'),
      alpha = I(.4)
    ) +
    geom_line(
      aes(year, tbhiv),
      data = subset(old, g.whoregion == i),
      colour = I('red'),
      linetype = I(2)
    ) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('HIV prevalence in TB')
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('output/checks/tbhiv', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}



# global aggregates
#
(est[, .(inc.num = as.integer(sum(inc * pop / 1e5))), by = year])
(est[, .(inc.h.num = as.integer(sums(inc.h * pop / 1e5))), by =
       year])



# save
#
save(est, file = here('data/est.rda'))
fwrite(est, file = here(paste0('csv/est_04inc_', Sys.Date(), '.csv')))
