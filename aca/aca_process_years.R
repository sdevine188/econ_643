library(readstata13)
library(dplyr)
library(stringr)


setwd("C:/Users/Stephen/Desktop/stata/econ_643/aca/aca_2008")
list.files()

aca <- read.dta13("aca_2008.dta")

# pre-process year files
# create fpg
aca$hiunpers <- as.numeric(as.character(aca$hiunpers))
aca$hiufpgbase <- as.numeric(as.character(aca$hiufpgbase))
aca$hiufpginc <- as.numeric(as.character(aca$hiufpginc))
aca$fpg <- aca$hiufpgbase + aca$hiufpginc * (aca$hiunpers - 1)

# create hiu income
# http://www.shadac.org/publications/using-shadac-health-insurance-unit-hiu-and-federal-poverty-guideline-fpg-microdata
# NA is coded as 9999999 for some reason??
aca$inctot[aca$inctot == 9999999] <- NA
aca$inctot_adj <- aca$inctot * aca$adjust
hiu_income <- aca %>% group_by(year, hiuid) %>% summarize(hiu_total_income = sum(inctot_adj, na.rm = TRUE))

# merge hiu_income to aca
aca <- left_join(aca, hiu_income, by = "hiuid")
aca <- aca %>% arrange(year.x, hiuid)
aca %>% select(year.x, hiuid, hiu_total_income, inctot, inctot_adj) %>% head(., 20)

# create percent of fpg
aca$pct_fpg <- aca$hiu_total_income / aca$fpg
aca %>% select(year.x, hiuid, hiu_total_income, inctot, inctot_adj, fpg, pct_fpg) %>% head(., 20)

# create perwt * pct_fpg
aca$pct_fpg_perwt_product <- aca$pct_fpg * aca$perwt

# create uninsurance rates by sub-population
aca %>% select(statefip, hiuid, hiunpers, pernum, perwt, pct_fpg, pct_fpg_perwt_product, hinscaid) %>% 
        head(.)
aca %>% filter(statefip == "Alabama", hinscaid == "Has insurance through Medicaid") %>% 
        summarize(
                avg_pct_fpg = sum(pct_fpg_perwt_product) / sum(perwt),
                min_pct_fpg = min(pct_fpg),
                max_pct_fpg = max(pct_fpg)
        )
# only 88k above 2.33 fpg 
aca %>% filter(statefip == "Alabama", hinscaid == "Has insurance through Medicaid", pct_fpg > 2.33) %>%
        summarize(count = sum(perwt))
aca %>% filter(statefip == "Alabama", hinscaid == "Has insurance through Medicaid", pct_fpg < 1.33) %>%
        summarize(count = sum(perwt), avg_pct_fpg = sum(pct_fpg_perwt_product) / sum(perwt))

median_pct_fpg <- rep(aca$pct_fpg, aca$perwt)

