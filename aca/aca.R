library(readstata13)
library(dplyr)
library(stringr)
library(readr)
# https://cran.r-project.org/web/packages/readstata13/README.html

setwd("C:/Users/Stephen/Desktop/stata/econ_643/aca/aca_2008-2011")
list.files()

# josh ulrich says faster to read .dta than csv / text, because .dta is binary
aca_2008_2011 <- read.dta13("aca_2008_2011.dta")

# setwd("C:/Users/Stephen/Desktop/stata/econ_643/aca/aca_2012-2014")
# list.files()
# aca_2012_2014 <- read.dta13("aca_2012_2014.dta")

# errors, too big trying to combine
# aca <- rbind(aca_2008_2011, aca_2012_2014)

# summarize aca_2008_2011
aca <- aca_2008_2011

# need to break into years, it's still too big to manipulate
aca_2008 <- filter(aca, year == "2008")
write_csv(aca_2008, "aca_2008.csv")

# create fpg
aca$hiunpers <- as.numeric(as.character(aca$hiunpers))
aca$hiufpgbase <- as.numeric(as.character(aca$hiufpgbase))
aca$hiufpginc <- as.numeric(as.character(aca$hiufpginc))
aca$fpg <- aca$hiufpgbase + aca$hiufpginc * (aca$hiunpers - 1)

# create hiu income
# NA is coded as 9999999 for some reason??
aca$inctot[aca$inctot == 9999999] <- NA
aca$inctot_adj <- aca$inctot * aca$adjust
hiu_income <- aca %>% group_by(year, hiuid) %>% summarize(hiu_total_income = sum(inctot_adj, na.rm = TRUE))

# merge hiu_income to aca
aca <- left_join(aca, hiu_income, by = "hiuid")
# aca2 <- merge(aca, hiu_income, by = "hiuid")
aca <- aca %>% arrange(year, hiuid)
aca %>% select(hiuid, hiu_total_income, inctot, inctot_adj) %>% head(., 20)




