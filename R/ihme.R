#' ---
#' title: IHME data
#' author: Philippe Glaziou
#' date: 2021-06-11
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

#' # Preamble
#' (Last updated: `r sys.Date()`)
#'
#' Process IHME files
#'
library(data.table)

ihmeall <- fread('input/IHME/IHME_GBD_AllCauses_all_countries.csv')
ihmetb <- fread('input/IHME/IHME_GBD_TB_all_countries.csv')

load('data/cty.rda') # iso3 codes

# check country names
(bad <- setdiff(ihmetb[, unique(location)],
                   cty[, country]))
(all.equal(bad, setdiff(ihmeall[, unique(location)],
                               cty[, country])) == TRUE)

(newnm <- grep('Ivoire', cty[, country], value = TRUE))
ihmetb[location == grep('Ivoire', bad, value = TRUE), location := newnm]
ihmeall[location == grep('Ivoire', bad, value = TRUE), location := newnm]

(newnm <- grep('Palestin', cty[, country], value = TRUE))
ihmetb[location == grep('Palestin', bad, value = TRUE), location := newnm]
ihmeall[location == grep('Palestin', bad, value = TRUE), location := newnm]

(newnm <- grep('United Kingdom', cty[, country], value = TRUE))
ihmetb[location == grep('United Kingdom', bad, value = TRUE), location := newnm]
ihmeall[location == grep('United Kingdom', bad, value = TRUE), location := newnm]

(setdiff(ihmeall[, unique(location)],
         cty[, country]))
(setdiff(ihmetb[, unique(location)],
         cty[, country]))

setnames(ihmetb, 'location', 'country')
setnames(ihmeall, 'location', 'country')

ihmetb <- merge(ihmetb, cty[, .(iso3, country)], by = 'country', all.x = TRUE)
ihmeall <- merge(ihmeall, cty[, .(iso3, country)], by = 'country', all.x = TRUE)

setkey(ihmetb, iso3, year)
setkey(ihmeall, iso3, year)


#' save
#'
save(ihmetb, file = 'data/ihmetb.rda')
fwrite(ihmetb, file = paste0('csv/ihmetb_', Sys.Date(), '.csv'))

save(ihmeall, file = 'data/ihmeall.rda')
fwrite(ihmeall, file = paste0('csv/ihmeall_', Sys.Date(), '.csv'))


