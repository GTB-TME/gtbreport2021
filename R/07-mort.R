#' ---
#' title: Mortality
#' author: Philippe Glaziou
#' date: 13/07/2021
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

#' # Preamble
#' (Last updated: `r Sys.Date()`)
#'
# Mortality HIV-neg
#
# Load libraries and data
#
library(data.table)
library(imputeTS)
library(propagate)
library(here)


load(here('data/cty.rda'))
# load(here('data/est.rda'))
load(here('data/vr.rda'))
load(here('data/old.rda'))
load(here('data/md.rda'))
load(here('data/unaids.rda'))



# !!! update the following line as needed !!!
est <- fread('csv/est_04inc_2021-10-08.csv')
setkey(est, iso3, year)


m <- 1e5
yr <- 2020

source(here('R/fun.R'))


# vectorized lohi
#
vlohi <- Vectorize(lohi, c('ev', 'sd'))


# import vr
dim(est)
est <-
  merge(est, vr[, .(
    iso3,
    year,
    vr.keep = keep.vr,
    vr.garbage = garbage,
    vr.coverage,
    vr.quality = codqual,
    vr.env = env,
    #                           ghe.env, ghe.env.lo, ghe.env.hi,
    vr.mort.nh = tb.adj * m / pop,
    vr.raw = tb * m / pop,
    vr.mort.nh.sd = tb.adj.sd * m / pop
  )],
  by = c('iso3', 'year'), all.x = TRUE)
dim(est)


# incorporate old values of mortality
est <-
  merge(est, old[, .(iso3,
                     year,
                     mort.nh,
                     mort.nh.sd,
                     mort.h,
                     mort.h.sd,
                     mort,
                     mort.sd,
                     old.source.mort = source.mort)], by = c('iso3', 'year'), all.x = TRUE)
dim(est)


# check missing values
sum(is.na(est$mort.nh) &
      est$year < yr) == 0   # TRUE: only year==yr inc values are missing

# check VR updates where usable
est[year < yr &
      vr.keep == TRUE, sum(vr.mort.nh / mort.nh > 1.1, na.rm = T)]
est[year < yr &
      vr.keep == TRUE, sum(vr.mort.nh / mort.nh < .9, na.rm = T)]





# update indirect estimates
out1 <-
  est[, {
    tmp = inc2mort(inc, inc.sd, imp.newinc, tbhiv, tbhiv.sd, noHIV =
                     T)$prop

    list(mort.nh = tmp[2],
         mort.nh.sd = tmp[4])
  },
  by = .(iso3, year)]

out2 <-
  est[, {
    tmp = inc2mort(inc, inc.sd, imp.newinc, tbhiv, tbhiv.sd, noHIV =
                     F)$prop

    list(mort.h = tmp[2],
         mort.h.sd = tmp[4])
  },
  by = .(iso3, year)]

est[, e.mort.nh := out1$mort.nh]
est[, e.mort.nh.sd := out1$mort.nh.sd]
est[, e.mort.h := out2$mort.h]
est[, e.mort.h.sd := out2$mort.h.sd]
est[, e.mort := mort.h + mort.nh]
est[, e.mort.sd := sqrt(mort.h.sd ^ 2 + mort.nh.sd ^ 2)]




# update indirect estimates in GNQ
#
sel <- est$iso3 == 'GNQ' & est$year < yr

(est['GNQ', .(iso3,
              year,
              newinc,
              imp.newinc,
              tbhiv,
              inc,
              mort,
              mort.nh,
              mort.h,
              mort.sd,
              e.mort.nh,
              e.mort.h)])
est[sel, mort.nh := e.mort.nh]
est[sel, mort.nh.sd := e.mort.nh.sd]
est[sel, mort.h := e.mort.h]
est[sel, mort.h.sd := e.mort.h.sd]
est[sel, mort := mort.h + mort.nh]
est[sel, mort.sd := sqrt(mort.h.sd ^ 2 + mort.nh.sd ^ 2)]
(est['GNQ', .(iso3,
              year,
              newinc,
              imp.newinc,
              tbhiv,
              inc,
              mort,
              mort.nh,
              mort.h,
              mort.sd,
              cfr = mort / inc, mort.hat)])
est[iso3=='GNQ' & year == yr, mort.hat := 96]






# predictive models of mortality (incl HIV+)
#
(dim(md))

# HIV+ and HIV-
md2 <-
  merge(md[hiv == 'a' &
             year == 2020 &
             measure == 'mort'], est[year == 2020 , .(
               iso3,
               pop,
               short = newinc / newinc2019,
               cdr.base = newinc / inc.hat,
               cfr.base = mort.hat / inc.hat,
               tbhiv
             )],
        by = 'iso3')


fit0 <-
  lm(ratio ~ cdr.base * short * cfr.base, data = md2) # discarding tbhiv at this stage
# fit0 <-
#   lm(ratio ~ cdr.base * short * cfr.base, data = md2, weights=pop)
# fit0 <-
#   lm(ratio ~ cdr.base * short * cfr.base, data = md2[iso3 %ni% 'CHN'], weights=pop)
fit <-
  step(fit0,
       direction = 'backward',
       scope = formula(fit0),
       trace = 0)
(summary(fit))
# plot(fit)

md2[, ratio.hat := predict(fit)]
md2[, ratio.sd.hat := predict(fit, se = TRUE)$se.fit]
(md2)
# qplot(ratio.hat, ratio, data=md2) + geom_abline(slope=1)
md2[, .(mean(ratio.hat), sd(ratio.hat))]


# list of modelled countries
(md.lst <- unique(md2$iso3))


est[year == yr, cdr.base := newinc / inc.hat]
est[year == yr, cfr.base := mort.hat / inc.hat]
est[year == yr, short := newinc / newinc2019]

sel <- est$year == yr & est$iso3 %ni% md.lst
table(sel)
out <- predict(fit, se = TRUE, newdata = est[sel])
summary(out$fit)

est[sel, ratiom.hat := out$fit]
est[sel, ratiom.sd.hat := out$se.fit]


# implausible values?
est[ratiom.hat < 1, .(iso3,
                        year,
                        ratiom.hat,
                        mort.hat,
                        inc,
                        newinc,
                        cdr.base,
                        cfr.base,
                        short,
                        tbhiv)]

est[ratiom.hat < 1, ratiom.hat := 1]


est[ratiom.hat > 1.5, .(iso3,
                      year,
                      ratiom.hat,
                      mort.hat,
                      inc,
                      newinc,
                      cdr.base,
                      short,
                      tbhiv)]

lst <- est[ratiom.hat > 1.5, iso3]
est[sel & iso3 %in% lst, ratiom.hat := NA]
est[sel & iso3 %in% lst, ratiom.sd.hat := NA]



# missing predicted ratios
sel <- est$year==yr & is.na(est$ratiom.hat) & est$iso3 %ni% md.lst
table(sel)
est[sel, .(iso3, g.whoregion, year, inc, newinc, tbhiv, short, mort.hat)]
est[sel, mean(short, na.rm=T)]

est[sel, ratiom.hat := 1]
est[sel, ratiom.sd.hat := .2]





# plug in 2020 model values
B1 <- copy(est)

dim(est)
est <-
  merge(est, md2[, .(iso3, year, model.mort = disrupt, model.mort.sd = sd2)], by =
          c('iso3', 'year'), all.x = TRUE)
dim(est)
sel <- est$year == yr & est$iso3 %in% md.lst
est[sel, .(iso3, year, mort, mort.sd, mort.hat, model.mort, model.mort.sd)]

est[sel, mort := model.mort]
est[sel, mort.sd := model.mort.sd]

# est[, model.mort := NULL]
# est[, model.mort.sd := NULL]

est[sel, source.mort := 'Model']





# 2020 mort from mort.ratio except in HIC, modeled countries, and countries with no shortfall
hic <- est[g.income == 'HIC', unique(iso3)]
hic <- c(hic, 'ANT', 'WLF')                 # high-income countries

(noshort <- est[year==yr & short>1 & iso3 %ni% md.lst, iso3])
allc <- unique(est$iso3)
(mortratio.lst <- setdiff(allc, c(hic, noshort, md.lst)))

sel <- est$year == yr & est$iso3 %in% mortratio.lst
table(sel)
est[sel, mort := mort.hat * ratiom.hat]
est[sel, source.mort := "Predicted mortality ratio"]
table(est$source.mort)


# mort.sd
B2 <- copy(est)

dim(est)
est <-
  merge(est, est[year == yr-1, .(iso3, mort2019 = mort, mort2019.sd = mort.sd)], by =
          'iso3', all.x = TRUE)
dim(est)

est[iso3 %in% mortratio.lst &
      year == yr, mort.hat.sd := mort2019.sd * mort.hat / mort2019]


out <-
  with(est[iso3 %in% mortratio.lst &
             year == yr], prodXY(mort.hat, ratiom.hat, mort.hat.sd, ratiom.sd.hat))$sd
est[iso3 %in% mortratio.lst & year == yr, mort.sd := out]




# HIC and countries with no shortfall
(trends.lst <- setdiff(allc, c(md.lst, mortratio.lst)))

sel <- est$iso3 %in% trends.lst & est$year == yr
table(sel)
est[sel, mort := mort.hat]
est[sel, mort.sd := mort.hat.sd]
est[year==yr & is.na(mort.sd), unique(iso3)]
est[year==yr & is.na(mort.sd), mort.sd := mort2019.sd * mort/mort2019]
est[year==yr & is.na(mort.sd) & iso3=='SMR', mort.sd := 0]

est[sel, source.mort := 'Current trends']
table(est$source.mort)


est[year==yr, sum(is.na(mort))==0]
est[year==yr, sum(is.na(mort.sd))==0]

(sum(is.na(est$mort)) == 0)
(sum(is.na(est$mort.sd)) == 0)




# check aggregates
#
md2[, sum(baseline * pop / 1e5)]
md2[, sum(disrupt * pop / 1e5)]

(est[iso3 %in% md.lst, sum(mort * pop / 1e5, na.rm = T), by = year])
(est[, sum(mort * pop / 1e5, na.rm = T), by = year])
(est[year == yr, sum(mort.hat * pop / 1e5, na.rm = T)])

# prelim excess 2020 mortality
(est[year == yr, sum(mort * pop / 1e5, na.rm = T) - sum(mort.hat * pop /
                                                          1e5, na.rm = T)])





# update mort.nh based on new VR data prior to 2020
#
B3 <- copy(est)

est[iso3 %in% c('ZAF', 'MNG') & !is.na(vr.keep), vr.keep := FALSE]

sel <-
  est$vr.keep == T & !is.na(est$vr.keep) & !is.na(est$vr.mort.nh)

table(sel) # use valid VR values
est[sel, table(year)]
(vr.lst <-
    unique(as.character(est$iso3[sel])))



# impute missing VR in the series
#
est[, imp.vr.mort.nh := vr.mort.nh]
est[, imp.vr.mort.nh.sd := vr.mort.nh.sd]

sel2 <- is.na(est$vr.mort.nh) & est$iso3 %in% vr.lst
table(sel2)
(vr.imp <- unique(as.character(est$iso3[sel2])))

sel <- est$iso3 %in% vr.imp
est[sel, nna := sum(!is.na(vr.mort.nh)), by = iso3] # nna non-missing values in each series
table(est$nna)


# only one not missing -> LOCF
#
est[nna == 1, .(iso3, year, vr.mort.nh)]
# est[nna == 1, mort.nh := imputeTS::na_locf(mort.nh, na_remaining = "keep"), by = iso3]
# est[nna == 1, mort.nh.sd := imputeTS::na_locf(mort.nh.sd, na_remaining = "keep"), by = iso3]

est[sel, nonzero := sum(vr.mort.nh > 0 & !is.na(vr.mort.nh)), by = iso3]
table(est$nonzero)


# more than one not missing, all non-missing are zeros -> LOCF
#
est[nna > 1 & nonzero == 0, .(iso3, year, vr.mort.nh)]
est[nna > 1 &
      nonzero == 0, imp.vr.mort.nh := imputeTS::na_locf(imp.vr.mort.nh, na_remaining = "keep"), by = iso3]
est[nna > 1 &
      nonzero == 0, imp.vr.mort.nh.sd := imputeTS::na_locf(imp.vr.mort.nh.sd, na_remaining = 'keep'), by = iso3]


# exactly 2 are not missing, at least one is not a zero -> simple interpolation
#
est[nna == 2 &
      nonzero > 0, imp.vr.mort.nh := imputeTS::na_interpolation(imp.vr.mort.nh), by = iso3]
est[nna == 2 &
      nonzero > 0, imp.vr.mort.nh.sd := imputeTS::na_interpolation(imp.vr.mort.nh.sd), by = iso3]

nokalman <-
  c(
    'SYR',
    'KWT',
    'MKD',
    'MNE',
    'LKA',
    'TJK',
    'BLR',
    'BGR',
    'EGY',
    'EST',
    'HND',
    'ZAF',
    'AZE',
    'CYP',
    'JPN',
    'THA'
  ) # use simple interpolation to avoid predicting implausible trends
est[sel, mm := mean(imp.vr.mort.nh, na.rm = T), by = iso3]

# very low average mort.nh (<1) or ISO3 in nokalman list -> simple interpolation
#
nokalman2 <-
  unique(as.character(est$iso3[est$mm < 1 &
                                 !is.na(est$mm)])) # simple interpolation in low mortalites
est[sel &
      iso3 %in% union(nokalman, nokalman2), imp.vr.mort.nh := imputeTS::na_interpolation(imp.vr.mort.nh), by =
      iso3]
est[sel &
      iso3 %in% union(nokalman, nokalman2), imp.vr.mort.nh.sd := imputeTS::na_interpolation(imp.vr.mort.nh.sd), by =
      iso3]


# use Kalman filter in the remaining series
#
est[sel &
      nna > 2, imp.vrmort.nh := imputeTS::na_kalman(imp.vr.mort.nh), by = iso3]
est[sel &
      nna > 2, imp.vr.mort.nh.sd := imputeTS::na_interpolation(imp.vr.mort.nh.sd), by = iso3]
summary(est$imp.vr.mort.nh[sel & est$nna > 2])

est[sel, nna := sum(!is.na(imp.vr.mort.nh.sd)), by = iso3]
table(est$nna[sel])


est[sel &
      nna > 1, imp.vr.mort.nh.sd := imputeTS::na_interpolation(imp.vr.mort.nh.sd), by = iso3]
sel2 <- sel & is.na(est$imp.vr.mort.nh.sd)
table(sel2) # should be all F

(est['GBR', .(iso3, year, mort.nh, mort.nh.sd, vr.mort.nh, vr.mort.nh.sd, imp.vr.mort.nh, imp.vr.mort.nh.sd)])


# fix TJK long flat trend, assuming cst CFR
#
sel <- est$iso3 == 'TJK' & est$year %in% 2006:2016
est$imp.vr.mort.nh[sel] <- est$inc[sel] * 0.07
est$imp.vr.mort.nh.sd[sel] <-
  est$imp.vr.mort.nh.sd[sel] * est$imp.vr.mort.nh[sel] / 12.816
est['TJK', imp.vr.mort.nh := imputeTS::na_interpolation(imp.vr.mort.nh)]
est['TJK', imp.vr.mort.nh.sd := imputeTS::na_interpolation(imp.vr.mort.nh.sd)]

est['TJK', .(iso3, year, inc, imp.vr.mort.nh, cfr = imp.vr.mort.nh / inc, imp.vr.mort.nh.sd)]


# remaining missing
est[iso3 %in% vr.lst & is.na(imp.vr.mort.nh), sum(is.na(imp.vr.mort.nh))]
est[iso3 %in% vr.lst, imp.vr.mort.nh := imputeTS::na_interpolation(imp.vr.mort.nh), by=iso3]
est[iso3 %in% vr.lst, imp.vr.mort.nh.sd := imputeTS::na_interpolation(imp.vr.mort.nh.sd), by=iso3]

est[iso3 %in% vr.lst & is.na(imp.vr.mort.nh), sum(is.na(imp.vr.mort.nh))==0]

est[mort.nh < 0, .(iso3, year, inc, newinc, mort.nh, mort.nh.sd, mort, tbhiv)]
# est[mort.nh < 0, mort.nh.sd := NA]
# est[mort.nh < 0, mort.nh := NA]

est[iso3 %in% vr.lst & imp.vr.mort.nh/mort.nh>1.5,.(iso3,year,mort.nh,imp.vr.mort.nh,vr.mort.nh)]
est[iso3 %in% vr.lst & imp.vr.mort.nh/mort.nh<0.5,.(iso3,year,mort.nh,imp.vr.mort.nh,vr.mort.nh)]


# clean-up
#
est[, nna := NULL]
est[, nonzero := NULL]
est[, mm := NULL]


# visualize vr imputations
#
# qplot(year, vr.mort.nh, data=est[iso3 %in% vr.lst], geom='point', size=I(1)) +
#   geom_line(aes(year, imp.vr.mort.nh), colour=I('red')) +
#   facet_wrap(~iso3, scales='free_y')




# update HIV-neg mortality estimates in vr.lst
#
est[iso3 %in% vr.lst, mort.nh := imp.vr.mort.nh]
est[iso3 %in% vr.lst, mort.nh.sd := imp.vr.mort.nh.sd]

est[iso3 %in% vr.lst & year<yr, source.mort := 'VR']
est[iso3 %in% setdiff(vr.lst, md.lst) & year==yr, source.mort := 'VR']
est[year < yr & is.na(source.mort), source.mort := old.source.mort]

est[year < yr, table(source.mort)]
est[year == yr, table(source.mort)]

est[, sum(is.na(source.mort))==0]



# update HIV-pos and total TB mortality estimates in vr.lst
#
est[iso3 %in% vr.lst, mort.h := e.mort.h]
est[iso3 %in% vr.lst, mort.h.sd := e.mort.h.sd]
est[iso3 %in% vr.lst & !is.na(mort.h), mort := mort.nh + mort.h]
est[iso3 %in% vr.lst & !is.na(mort.h), mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]
est[iso3 %in% vr.lst & is.na(mort.h), mort := mort.nh]
est[iso3 %in% vr.lst & is.na(mort.h), mort.sd := mort.nh.sd]

est[iso3 %in% vr.lst, test.AgeB(mort, mort.nh)]




# mortality by HIV, 2020, modeled countries
#
B4 <- copy(est)

est[year == yr, table(is.na(mort.nh))]

# predict mort distributions by HIV in lst.md
md3 <- merge(md2,
             md[year == yr &
                  measure == 'mort' &
                  hiv == 'pos', .(
                    iso3,
                    baseline.h = baseline,
                    sd1.h = sd1,
                    disrupt.h = disrupt,
                    sd2.h = sd2,
                    ratio.h = ratio
                  )],
             by = 'iso3')


md3[, baseline.prop.h := baseline.h / baseline]
md3[, disrupt.prop.h := disrupt.h / disrupt]

(md3[])

# qplot(baseline.prop.h, disrupt.prop.h, data = md3) + geom_abline(slope = 1)
# nearly perfect equivalence
# will then forward mort.prop.h of 2019 to 2020




# mortality by HIV, 2020, non-VR, non-modelled countries
#
est[year==yr-1, sum(is.na(mort.h))]
est[year==yr-1, prop.mort.h := mort.h / mort]
est[mort == 0, prop.mort.h := 0]
est[year==yr-1, test.isbinom(prop.mort.h)]


# if missing, prop.mort.h in 2020 equals that of 2019
est[year>= yr-1, prop.mort.h := na_locf(prop.mort.h), by=iso3]
est[year==yr, test.isbinom(prop.mort.h)]


sel <- est$iso3 %ni% unique(md3$iso3) & est$year == yr & est$source.mort != 'VR'
table(sel)

est[sel, mort.nh := mort * (1 - prop.mort.h)]
est[sel, mort.nh.sd := mort.sd * (1 - prop.mort.h)]
est[sel, mort.h := mort * prop.mort.h]
est[sel, mort.h.sd := mort.sd * prop.mort.h]

est[mort.nh>mort, table(iso3)]


# mort.h from model
sel <- est$iso3 %in% unique(md3$iso3) & est$year == yr
table(sel)

(est[sel, .(iso3, year, mort, mort.sd, mort.nh, mort.nh.sd, mort.h, mort.h.sd)])

est[sel, prop.mort.h := md3$disrupt.prop.h]
est[sel, mort.h := md3$disrupt.h]
est[sel, mort.h.sd := mort.sd * sqrt(prop.mort.h)]  # model SD too small

est[sel, mort.nh := mort * (1 - prop.mort.h)]
est[sel, mort.nh.sd := mort.sd * sqrt(1 - prop.mort.h)] # model SD too small

(est[sel, .(iso3, year, mort, mort.sd, mort.nh, mort.nh.sd, mort.h, mort.h.sd)])

sel <- est$iso3 == 'MCO' & est$year == yr
est[sel, mort.sd := mort * .2]
est[sel, mort.nh.sd := mort * .2]
est[sel, mort.h.sd := 0]


# RUS: model mort a little too low in 2020 (doesn't fit mort.nh from VR and mort.h)
est['RUS',.(iso3,year,model.mort,mort,mort.nh,mort.h,vr.mort.nh, vr.mort.nh.sd, vr.mort.nh+mort.h)]

sel <- est$iso3=='RUS' & est$year==yr
est[sel, mort.nh := vr.mort.nh]
est[sel, mort.nh.sd := vr.mort.nh.sd]
est[sel, mort := mort.nh + mort.h]
est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]


bak <- copy(est)
# CHN: NTP reported VR data for 2020 VERY LATE on 12/10/2021 (1.8/100k)
est['CHN',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]

CHN.mort.nh <- 2.08                      # reported by NTP
CHN.mort.nh.sd <- 0.053 * CHN.mort.nh    # same CV as in 2019
CHN.mort.h <- 0.071 * CHN.mort.nh        # same ratio h/nh as in 2019
CHN.mort.h.sd <- CHN.mort.h * 0.14      # same CV as in 2019

sel <- est$iso3=='CHN' & est$year==yr


est[sel, mort.nh := CHN.mort.nh]
est[sel, mort.nh.sd := CHN.mort.nh.sd]
est[sel, mort.h := CHN.mort.h]
est[sel, mort.h.sd := CHN.mort.h.sd]
est[sel, mort := mort.nh + mort.h]
est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]

est['CHN',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]

est[, sum(is.na(mort))==0]



# sel <- est$iso3 %in% c('RUS','UKR','BRA')
# est[sel & is.na(prop.mort.h), table(iso3,year)]
# est[sel, sum(is.na(mort))==0]
# est[sel & year==yr, source.mort]
#
# est[sel, mort := mort.nh + mort.h]
# est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]
# est[sel & !is.na(mort), test.AgeB(mort, mort.nh)]



# mort.h greater than mort.hiv?
sel <- est$mort.h >= est$mort.hiv & est$mort.h>0 & est$mort.hiv > 0
table(sel)
est[sel, unique(iso3)]
est[sel, table(year)]
est[sel, summary(mort.h/mort.hiv)]
est[!sel & mort.hiv>0, summary(mort.h/mort.hiv)]
# set cap at 60%
est[sel, mort.h := mort.hiv * 0.6]
est[sel, mort.h.sd := mort.h * 0.25]
est[sel, mort := mort.nh + mort.h]
est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]

est[is.na(mort.h.sd), ]
est[is.na(mort.h.sd), mort.h.sd := 0]


# checks
(sum(is.na(est$mort)) == 0)
(sum(is.na(est$mort.sd)) == 0)
(sum(is.na(est$mort.nh)) == 0)
(sum(is.na(est$mort.nh.sd)) == 0)
(sum(is.na(est$mort.h)) == 0)
(sum(is.na(est$mort.h.sd)) == 0)

est[, test.AgeB(mort, mort.nh)]
est[mort.hiv>0 & mort.h>0, test.AgeB(mort.hiv, mort.h)]




# add bounds and counts
#
B5 <- copy(est)

sel <- est$mort > 0 & est$mort.sd > 0
table(sel)

est[sel & (mort.sd/1e5)^2 > mort/1e5 * (1-mort/1e5), mort.sd := mort * .2]

out <- vlohi(est$mort[sel] / m, est$mort.sd[sel] / m)

est$mort.lo[sel] <- out[1, ] * m
est$mort.hi[sel] <- out[2, ] * m

sel <- est$mort.sd == 0 & !is.na(est$mort.sd)
table(sel)
est$mort.lo[sel] <- est$mort[sel]
est$mort.hi[sel] <- est$mort[sel]

sel <-
  (est$mort.lo > est$mort) |
  (est$mort.hi < est$mort) &
  (!is.na(est$mort) &
     !is.na(est$mort.lo) & !is.na(est$mort.hi))
table(sel)
# est[sel, .(iso3, year, inc, mort, mort.sd, mort.lo, mort.hi)]
# est[sel, mort.hi := mort + 1.96 * mort.sd]
est[mort.lo == 0, .(iso3, year, mort, mort.sd, mort.lo, mort.hi)]
est[mort > 0, test.bounds(mort, mort.lo, mort.hi)]

est[is.na(mort.lo), mort.lo := 0]
est[is.na(mort.hi), mort.hi := mort + 1.96 * mort.sd]



# HIV-neg
sel <- est$mort.nh > 0 & est$mort.nh.sd > 0
table(sel)
est[sel & (mort.nh.sd/1e5)^2 > mort.nh/1e5 * (1-mort.nh/1e5)]
est[sel & (mort.nh.sd/1e5)^2 > mort.nh/1e5 * (1-mort.nh/1e5), mort.nh.sd := mort.nh * .2]

out <- vlohi(est$mort.nh[sel] / m, est$mort.nh.sd[sel] / m)

est$mort.nh.lo[sel] <- out[1, ] * m
est$mort.nh.hi[sel] <- out[2, ] * m

sel <- est$mort.nh.sd == 0 & !is.na(est$mort.nh.sd)
table(sel)
est$mort.nh.lo[sel] <- est$mort.nh[sel]
est$mort.nh.hi[sel] <- est$mort.nh[sel]

sel <-
  (est$mort.nh.lo > est$mort.nh) |
  (est$mort.nh.hi < est$mort.nh) &
  (!is.na(est$mort.nh) &
     !is.na(est$mort.nh.lo) & !is.na(est$mort.nh.hi))
table(sel)
# est[sel, .(iso3, year, inc, mort.nh, mort.nh.sd, mort.nh.lo, mort.nh.hi)]
# est[sel, mort.nh.hi := mort.nh + 1.96 * mort.nh.sd]
est[mort.nh.lo == 0, .(iso3, year, mort.nh, mort.nh.sd, mort.nh.lo, mort.nh.hi)]
est[mort.nh > 0, test.bounds(mort.nh, mort.nh.lo, mort.nh.hi)]

est[is.na(mort.nh.lo), mort.nh.lo := 0]
est[is.na(mort.nh.hi), mort.nh.hi := mort.nh + 1.96 * mort.nh.sd]



# HIV-pos
sel <- est$mort.h > 0 & est$mort.h.sd > 0
table(sel)

out <- vlohi(est$mort.h[sel] / m, est$mort.h.sd[sel] / m)

est$mort.h.lo[sel] <- out[1, ] * m
est$mort.h.hi[sel] <- out[2, ] * m

sel <- est$mort.h.sd == 0 & !is.na(est$mort.h.sd)
table(sel)
est$mort.h.lo[sel] <- est$mort.h[sel]
est$mort.h.hi[sel] <- est$mort.h[sel]

sel <-
  (est$mort.h.lo > est$mort.h) |
  (est$mort.h.hi < est$mort.h) &
  (!is.na(est$mort.h) &
     !is.na(est$mort.h.lo) & !is.na(est$mort.h.hi))
table(sel)
# est[sel, .(iso3, year, inc, mort.h, mort.h.sd, mort.h.lo, mort.h.hi)]
# est[sel, mort.h.hi := mort.h + 1.96 * mort.h.sd]
est[mort.h.lo == 0, .(iso3, year, mort.h, mort.h.sd, mort.h.lo, mort.h.hi)]
est[mort.h > 0, test.bounds(mort.h, mort.h.lo, mort.h.hi)]

est[is.na(mort.h.lo), mort.h.lo := 0]
est[is.na(mort.h.hi), mort.h.hi := mort.h + 1.96 * mort.h.sd]

sel <- est$iso3=='MSR' & est$year==yr
est[sel, .(iso3,year,mort,mort.sd,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort.h.lo,mort.h.hi)]
est[sel, mort.sd := 0]
est[sel, mort.hi := 0]
est[sel, mort.nh.sd := 0]
est[sel, mort.nh.hi := 0]
est[sel, mort.h.sd := 0]
est[sel, mort.h.hi := 0]

est <- within(est, {
  mort.num <- mort * pop / m
  mort.lo.num <- mort.lo * pop / m
  mort.hi.num <- mort.hi * pop / m

  mort.nh.num <- mort.nh * pop / m
  mort.nh.lo.num <- mort.nh.lo * pop / m
  mort.nh.hi.num <- mort.nh.hi * pop / m

  mort.h.num <- mort.h * pop / m
  mort.h.lo.num <- mort.h.lo * pop / m
  mort.h.hi.num <- mort.h.hi * pop / m

  inc.num <- inc * pop / m
  inc.lo.num <- inc.lo * pop / m
  inc.hi.num <- inc.hi * pop / m
  inc.nh.num <- inc.nh * pop / m
  inc.nh.lo.num <- inc.nh.lo * pop / m
  inc.nh.hi.num <- inc.nh.hi * pop / m
  inc.h.num <- inc.h * pop / m
  inc.h.lo.num <- inc.h.lo * pop / m
  inc.h.hi.num <- inc.h.hi * pop / m

})


# checks
#
est[, test.bounds(mort, mort.lo, mort.hi)]
est[, test.bounds(mort.nh, mort.nh.lo, mort.nh.hi)]
est[, test.bounds(mort.h, mort.h.lo, mort.h.hi)]

est[, .(
  sums(mort.nh.num),
  sums(inc.num),
  sums(inc.nh.num),
  sums(mort.nh.num) / sums(inc.nh.num)
), by = year]
old[, .(
  sums(mort.nh.num),
  sums(inc.num),
  sums(inc.nh.num),
  sums(mort.nh.num) / sums(inc.nh.num)
), by = year]

wr <- unique(as.character(est$g.whoregion))




for (i in wr) {
  p <-
    qplot(
      year,
      mort,
      data = subset(est, g.whoregion == i),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.lo, ymax = mort.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort),
      data = subset(old, g.whoregion == i),
      colour = I('red'),
      linetype = I(2)
    ) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('output/checks/mort_', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}




for (i in wr) {
  p <-
    qplot(
      year,
      mort.h,
      data = subset(est, g.whoregion == i),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.h.lo, ymax = mort.h.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort.h),
      data = subset(old, g.whoregion == i),
      colour = I('red'),
      linetype = I(2)
    ) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('output/checks/mort.h_', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}



for (i in wr) {
  p <-
    qplot(
      year,
      mort.nh,
      data = subset(est, g.whoregion == i),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.nh.lo, ymax = mort.nh.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort.nh),
      data = subset(old, g.whoregion == i),
      colour = I('red'),
      linetype = I(2)
    ) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('output/checks/mort.nh_', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}


for (i in wr) {
  p <-
    qplot(
      year,
      mort.nh,
      data = subset(est, g.whoregion == i & year>=yr-5),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.nh.lo, ymax = mort.nh.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort.nh),
      data = subset(old, g.whoregion == i & year>=yr-5),
      colour = I('red'),
      linetype = I(2)
    ) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('output/checks/mort.nh_', i, '_5y.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}


est[is.na(source.mort), source.mort := old.source.mort]


# check sources in 2020 and prior to 2019
# set source.inc prior to 2020 to last year's 2019 values
#
table(est[year==2020, source.mort])
table(est[year==2019, source.mort])
table(old[year==2019, source.mort])
table(est[year<=2019, source.mort])
table(old[, source.mort])

table(est[year==2020, source.inc])
table(est[year==2019, source.inc])
table(old[year==2019, source.inc])
table(est[year<=2019, source.inc])
table(est[year<=2019, old.source.inc])
table(old[, source.inc])
est[, old.source.inc := NULL]

dim(est)
est <-
  merge(est, old[year == 2019, .(iso3, old.source.inc = source.inc)], by =
          'iso3', all.x = TRUE)
dim(est)
est[year<=2019, source.inc := old.source.inc]




# check global aggregates
#
(est[, sum(mort.num), by=year])
(est[iso3 %in% md.lst, sum(mort.num), by=year])
(a <- est[year==2020, sum(mort.hat * pop / 1e5)])
(b <- est[year==2020, sum(mort * pop / 1e5)])
(b-a)
rm(a, b)
(est[, sum(mort.h.num), by=year])
(est[, sum(mort.nh.num), by=year])



# late update from CHN and RUS
est[iso3=='CHN' & year==yr, source.mort := 'VR']
est[iso3=='RUS' & year==yr, source.mort := 'VR']


# save
#
save(est, file = here('data/est.rda'))
fwrite(est, file = here(paste0('csv/est_', Sys.Date(), '.csv')))

