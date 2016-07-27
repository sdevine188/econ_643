library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(reshape2)
library(caret)
library(multiwayvcov)
library(lmtest)
library(ggplot2)
library(scales)
library(stargazer)
library(ggthemes)
library(gridExtra)
library(grid)
library(gtable)


# set wd
setwd("C:/Users/Stephen/Desktop/stata/econ_643/aca/shadac_2008_2014")
list.files()

# read in data
aca <- read.csv("shadac_2008_2014.csv", stringsAsFactors = FALSE)

# clean data
# aca <- aca %>% select(-MOE) %>% filter(Data.Type == "Percent")
aca <- aca %>% select(-MOE)

# read in expansion_dates
exp <- read.csv("expansion_dates.csv", stringsAsFactors = FALSE)

# add exp_date to aca
exp2 <- exp %>% select(state, exp_date)
aca <- left_join(aca, exp2, by = c("Location" = "state"))
aca$exp_date <- mdy(aca$exp_date)

# add exp to signify proportion of year for which expanded coverage was available
aca$exp <- 0
for(i in 1:nrow(aca)){
        if(!is.na(aca$exp_date[i])){
                if(year(aca$exp_date[i]) == aca$TimeFrame[i]) {aca$exp[i] <- (12 - month(aca$exp_date[i])) / 12}
                if(year(aca$exp_date[i]) < aca$TimeFrame[i]) {aca$exp[i] <- 1}
                if(year(aca$exp_date[i]) > aca$TimeFrame[i]) {aca$exp[i] <- 0}
        }
}

# create dummy for expansion
aca$exp_dummy <- ifelse(is.na(aca$exp_date), 0, 1)

# convert "N/A" in Data column for some states to NA
aca$Data[which(aca$Data == "N/A")] <- NA

# dcast to get column for Race/Ethnicity insured/uninsured percentages
aca <- dcast(aca, Fips + Location + Coverage.Type + TimeFrame + exp_date + exp + exp_dummy
             ~ Race...Ethnicity + Data.Type, value.var = "Data")

aca$pop <- 0
for(i in 1:nrow(aca)) {
        print(i)
        state_pop <- aca %>% group_by(Location, TimeFrame) %>% 
        summarize(pop = as.numeric(Total_Count[Coverage.Type == "Uninsured"]) + 
                as.numeric(Total_Count[Coverage.Type == "Insured"]))
        
        selected_pop <- filter(state_pop, Location == aca$Location[i], TimeFrame == aca$TimeFrame[i]) %>%
                select(pop)
        aca$pop[i] <- selected_pop$pop
}

# rename columns
names(aca) <- c("fips", "location", "coverage_type", "year", "exp_date", "exp", "exp_dummy",
                 "black_count", "black_pct", "asian_count", "asian_pct", "hispanic_count", "hispanic_pct",
                "other_race_count", "other_race_pct", "total_count", "total_pct", "white_count",
                "white_pct", "pop")

write_csv(aca, "aca_2008_2014_clean2.csv")
aca <- read_csv("aca_2008_2014_clean2.csv", col_types = cols(pop = col_number()))

# filter dataset to just uninsured coverage stats
unique(aca2$coverage_type)
aca_unins <- filter(aca, coverage_type == "Uninsured")

# manual diff-in-diff from 2012 to 2014, removing 6 early expansion states
aca_unins %>% filter(!location %in% c("California", "Connecticut", "Dist. of Columbia",
        "Minnesota", "New Jersey", "Washington"), year %in% c(2012, 2014)) %>% 
        group_by(exp_dummy, year) %>% summarize(avg_unins = mean(as.numeric(total_pct))) %>% ungroup() %>%
        summarize(diff_non_exp = avg_unins[2] - avg_unins[1], 
                  diff_exp = avg_unins[4] - avg_unins[3],
                  diff_in_diff = diff_exp - diff_non_exp)

# diff_non_exp    diff_exp diff_in_diff
# 1  -0.02353208 -0.03269619 -0.009164107

# manual diff-in-diff from 2012 to 2014, all states, expansion in 2014
aca_unins %>% filter(year %in% c(2012, 2014)) %>% 
        group_by(exp_dummy, year) %>% summarize(avg_unins = mean(as.numeric(total_pct))) %>% ungroup() %>%
        summarize(diff_non_exp = avg_unins[2] - avg_unins[1], 
                  diff_exp = avg_unins[4] - avg_unins[3],
                  diff_in_diff = diff_exp - diff_non_exp)

# exp_dummy year  avg_unins
# 1         0 2012 0.14610960
# 2         0 2014 0.12353680
# 3         1 2012 0.12158346
# 4         1 2014 0.08891462

# diff_non_exp    diff_exp diff_in_diff
# 1   -0.0225728 -0.03266885  -0.01009605
        
# filter dataset to just medicaid/chip coverage stats
unique(aca$coverage_type)
aca_medicaid <- filter(aca, coverage_type == "Medicaid/CHIP")

# manual diff-in-diff from 2012 to 2014, removing 6 early expansion states
aca_medicaid %>% filter(!location %in% c("California", "Connecticut", "Dist. of Columbia",
                                      "Minnesota", "New Jersey", "Washington"), year %in% c(2012, 2014)) %>% 
        group_by(exp_dummy, year) %>% summarize(avg_unins = mean(as.numeric(total_pct))) %>% ungroup() %>%
        summarize(diff_non_exp = avg_unins[2] - avg_unins[1], 
                  diff_exp = avg_unins[4] - avg_unins[3],
                  diff_in_diff = diff_exp - diff_non_exp)

# diff_non_exp  diff_exp diff_in_diff
# 1  0.001774167 0.0229881   0.02121393

# manual diff-in-diff from 2012 to 2014, all states, expansion in 2014
aca_medicaid %>% filter(year %in% c(2012, 2014)) %>% 
        group_by(exp_dummy, year) %>% summarize(avg_unins = mean(as.numeric(total_pct))) %>% ungroup() %>%
        summarize(diff_non_exp = avg_unins[2] - avg_unins[1], 
                  diff_exp = avg_unins[4] - avg_unins[3],
                  diff_in_diff = diff_exp - diff_non_exp)

# exp_dummy year avg_unins
# 1         0 2012 0.1697596
# 2         0 2014 0.1721916
# 3         1 2012 0.1741438
# 4         1 2014 0.1972085

# diff_non_exp   diff_exp diff_in_diff
# 1     0.002432 0.02306462   0.02063262

# diff-in-diff regression with state and year fixed effects
# medicaid, pct of pop

m1_medicaid <- lm(total_pct ~ exp, data = aca_medicaid)
coeftest(m1_medicaid)
# exp         0.0244318  0.0067925  3.5969 0.0003677 ***
# exp         0.0402727  0.0088665  4.5421 7.642e-06 ***
m1.vcovCL <- cluster.vcov(m1_medicaid, aca_medicaid$location)
coeftest(m1_medicaid, m1.vcovCL)
# exp         0.0244318  0.0082534  2.9602  0.003281 **
# exp         0.0402727  0.0070521  5.7108  2.38e-08 ***

m2_medicaid <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_medicaid)
coeftest(m2_medicaid)
# exp                                0.0157022  0.0022031   7.1275 7.696e-12 ***
# exp                                0.0249940  0.0026956   9.2720 < 2.2e-16 ***
m2_medicaid.vcovCL <- cluster.vcov(m2_medicaid, aca_medicaid$location)
coeftest(m2_medicaid, m2_medicaid.vcovCL)
# exp                                1.5702e-02  4.5324e-03  3.4644e+00 0.0006091 ***
# exp                                2.4994e-02  4.7224e-03  5.2927e+00 2.337e-07 ***

m3_medicaid <- lm(total_pct ~ exp + factor(year) + factor(location) * year, data = aca_medicaid)
coeftest(m3_medicaid)
# exp                                     9.3864e-03  2.4903e-03  3.7692 0.0002045 ***
# exp                                     1.8311e-02  2.6657e-03  6.8690 5.143e-11 ***
m3_medicaid.vcovCL <- cluster.vcov(m3_medicaid, aca_medicaid$location)
coeftest(m3_medicaid, m3_medicaid.vcovCL)
# same coeff on exp and tvalue in stata, though stata doesn't have NaNs
# with 2014 as exp_date: exp                                     1.8311e-02  5.2945e-03   3.4584 0.0006390 ***

m4_medicaid <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_medicaid, weights = pop)
coeftest(m4_medicaid)
# exp                                0.0100708  0.0016919   5.9524 7.392e-09 ***
# exp                                0.0235422  0.0021105  11.1548 < 2.2e-16 ***
m4_medicaid.vcovCL <- cluster.vcov(m4_medicaid, aca_medicaid$location)
coeftest(m4_medicaid, m4_medicaid.vcovCL)
# same as stata
# exp                                1.0071e-02  3.3985e-03      2.9633  0.003288 **
# exp                                2.3542e-02  4.6813e-03  5.0289e+00 8.517e-07 ***

m5_medicaid <- lm(total_pct ~ exp + factor(location) * year + factor(year), data = aca_medicaid, weights = pop)
coeftest(m5_medicaid)
# exp                                     2.0255e-02  1.8991e-03 10.6655 < 2.2e-16 ***
m5_medicaid.vcovCL <- cluster.vcov(m5_medicaid, aca_medicaid$location)
coeftest(m5_medicaid, m5_medicaid.vcovCL)
# exp                                     2.0255e-02  4.4661e-03     4.5353 8.949e-06 ***

# medicaid
# pct growth of count

# aca_medicaid, percentage growth over time
aca_medicaid$pct_growth_count <- 0
for(i in 2:nrow(aca_medicaid)) {
        if(!aca_medicaid$year[i] == 2008) {
                aca_medicaid$pct_growth_count[i] <- (as.numeric(aca_medicaid$total_pct[i]) - as.numeric(aca_medicaid$total_pct[i - 1])) / as.numeric(aca_medicaid$total_pct[i - 1])
        }
}
for(i in 1:nrow(aca_medicaid)) {
        if(aca_medicaid$year[i] == 2008) {
                aca_medicaid$pct_growth_count[i] <- aca_medicaid$pct_growth_count[i + 1] 
        }
}

m1_medicaid <- lm(pct_growth_count ~ exp, data = filter(aca_medicaid, !year == 2008))
coeftest(m1_medicaid)
# exp         0.0428267  0.0110899  3.8618 0.0001338 ***
# exp         0.1012927  0.0141838  7.1414 6.846e-12 ***
m1.vcovCL <- cluster.vcov(m1_medicaid, aca_medicaid$location)
coeftest(m1_medicaid, m1.vcovCL)
# exp         0.0428267  0.0168592  2.5403   0.0115 *
# exp         0.1012927  0.0070521 14.3635 < 2.2e-16 ***

m2_medicaid <- lm(pct_growth_count ~ exp + factor(location) + factor(year), data = filter(aca_medicaid, !year == 2008))
coeftest(m2_medicaid)
# exp                                5.9247e-02  1.4194e-02  4.1741 3.927e-05 ***
# exp                                0.10786629  0.01879876  5.7379 2.772e-08 ***
m2_medicaid.vcovCL <- cluster.vcov(m2_medicaid, aca_medicaid$location)
coeftest(m2_medicaid, m2_medicaid.vcovCL)
# exp                                5.9247e-02  2.3023e-02  2.5734e+00   0.01055 *
# exp                                1.0787e-01  4.7224e-03  2.2841e+01 < 2.2e-16 ***

m3_medicaid <- lm(pct_growth_count ~ exp + factor(year) + factor(location) * year, data = aca_medicaid)
coeftest(m3_medicaid)
# exp                                     6.5778e-02  1.9513e-02  3.3709 0.0008683 ***
# exp                                     1.2068e-01  2.1328e-02  5.6581 4.194e-08 ***
m3_medicaid.vcovCL <- cluster.vcov(m3_medicaid, aca_medicaid$location)
coeftest(m3_medicaid, m3_medicaid.vcovCL)
# exp                                     6.5778e-02  3.6307e-02   1.8117 0.0712320
# exp                                     1.2068e-01  3.4162e-02  3.5326e+00 0.0004904 ***

m4_medicaid <- lm(pct_growth_count ~ exp + factor(location) + factor(year), data = aca_medicaid, weights = pop)
coeftest(m4_medicaid)
# exp                                0.05180837  0.00935451   5.5383 6.694e-08 ***
# exp                                1.1098e-01  1.2202e-02   9.0947 < 2.2e-16 ***
m4_medicaid.vcovCL <- cluster.vcov(m4_medicaid, aca_medicaid$location)
coeftest(m4_medicaid, m4_medicaid.vcovCL)
# exp                                5.1808e-02  1.4369e-02     3.6056 0.0003648 ***
# exp                                1.1098e-01  2.0487e-02     5.4170 1.248e-07 ***





# regression using aca_uninsured
# pct of pop
m1_unins <- lm(total_pct ~ exp, data = aca_unins)
coeftest(m1_unins)
# exp         -0.0377481  0.0069497 -5.4316 1.039e-07 ***
# exp         -0.0510863  0.0091459 -5.5857 4.637e-08 ***
m1_unins.vcovCL <- cluster.vcov(m1_unins, aca_unins$location)
coeftest(m1_unins, m1_unins.vcovCL)
# exp         -0.0377481  0.0080993 -4.6606 4.467e-06 ***
# exp         -0.0510863  0.0054571 -9.3615 < 2.2e-16 ***

m2_unins <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_unins)
coeftest(m2_unins)
# exp                               -6.7579e-03  1.8637e-03  -3.6261 0.0003382 ***
# exp                               -0.01364777  0.00231338  -5.8995 9.860e-09 ***
m2_unins.vcovCL <- cluster.vcov(m2_unins, aca_unins$location)
coeftest(m2_unins, m2_unins.vcovCL)
# exp                               -6.7579e-03  3.4739e-03 -1.9453e+00 0.0526722 .
# exp                               -1.3648e-02  4.2865e-03 -3.1839e+00  0.001606 **

m3_unins <- lm(total_pct ~ exp + factor(year) + factor(location) * year, data = aca_unins)
coeftest(m3_unins)
# exp                                    -6.0035e-03  2.2517e-03 -2.6662 0.0081734 **
# exp                                    -1.2557e-02  2.4680e-03 -5.0878 7.137e-07 ***
m3_unins.vcovCL <- cluster.vcov(m3_unins, aca_unins$location)
coeftest(m3_unins, m3_unins.vcovCL)
# exp                                    -6.0035e-03  4.4293e-03 -1.3554e+00 0.1765121
# exp                                    -1.2557e-02  4.3381e-03  -2.8945  0.004134 ** 

m4_unins <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_unins, weights = pop)
coeftest(m4_unins)
# exp                               -0.00232353  0.00150479  -1.5441 0.1236263  
# exp                               -0.01123593  0.00201857  -5.5663 5.791e-08 ***
m4_unins.vcovCL <- cluster.vcov(m4_unins, aca_unins$location)
coeftest(m4_unins, m4_unins.vcovCL)
# exp                               -2.3235e-03  3.0169e-03     -0.7702  0.441813
# exp                               -1.1236e-02  6.7101e-03     -1.6745 0.0950838 .

m5_unins <- lm(total_pct ~ exp + factor(location) * year + factor(year), data = aca_unins, weights = pop)
coeftest(m5_unins)
# exp                                    -1.1073e-02  1.9739e-03 -5.6095 5.386e-08 ***
m5_unins.vcovCL <- cluster.vcov(m5_unins, aca_unins$location)
coeftest(m5_unins, m5_unins.vcovCL)
# exp                                    -1.1073e-02  6.6406e-03    -1.6674 0.0966862 . 

# aca_unins, pct growth of count
aca_unins$pct_growth_count <- 0
for(i in 2:nrow(aca_unins)) {
        if(!aca_unins$year[i] == 2008) {
                aca_unins$pct_growth_count[i] <- (as.numeric(aca_unins$total_pct[i]) - as.numeric(aca_unins$total_pct[i - 1])) / as.numeric(aca_unins$total_pct[i - 1])
        }
}
for(i in 1:nrow(aca_unins)) {
        if(aca_unins$year[i] == 2008) {
                aca_unins$pct_growth_count[i] <- aca_unins$pct_growth_count[i + 1] 
        }
}

m1_unins <- lm(pct_growth_count ~ exp, data = aca_unins)
coeftest(m1_unins)
# exp         -0.1729997  0.0140813 -12.286   <2e-16 ***
m1_unins.vcovCL <- cluster.vcov(m1_unins, aca_unins$location)
coeftest(m1_unins, m1_unins.vcovCL)
# exp         -0.1729997  0.0263612 -6.5627 1.875e-10 ***

m2_unins <- lm(pct_growth_count ~ exp + factor(location) + factor(year), data = aca_unins)
coeftest(m2_unins)
# exp                               -8.0709e-02  1.5245e-02  -5.2941 2.320e-07 ***
m2_unins.vcovCL <- cluster.vcov(m2_unins, aca_unins$location)
coeftest(m2_unins, m2_unins.vcovCL)
# exp                               -8.0709e-02  1.9859e-02 -4.0640e+00 6.171e-05 ***

m3_unins <- lm(pct_growth_count ~ exp + factor(year) + factor(location) * year, data = aca_unins)
coeftest(m3_unins)
# exp                                    -8.3742e-02  2.1583e-02 -3.8800 0.0001338 ***
m3_unins.vcovCL <- cluster.vcov(m3_unins, aca_unins$location)
coeftest(m3_unins, m3_unins.vcovCL)
# exp                                    -8.3742e-02  3.3557e-02  -2.4955 0.0132247 *

m4_unins <- lm(pct_growth_count ~ exp + factor(location) + factor(year), data = aca_unins, weights = pop)
coeftest(m4_unins)
# exp                               -4.8668e-02  9.3645e-03  -5.1971 3.756e-07 ***
m4_unins.vcovCL <- cluster.vcov(m4_unins, aca_unins$location)
coeftest(m4_unins, m4_unins.vcovCL)
# exp                               -4.8668e-02  1.3228e-02   -3.6792 0.0002774 ***






# exploratory graphs 
# aca_unins, percentage growth over time
agg_unins <- aca_unins %>% group_by(exp_dummy, year) %>% summarize(sum_count = sum(as.numeric(total_count)))
agg_unins$pct_growth <- 0
for(i in 2:nrow(agg_unins)) {
        if(!agg_unins$year[i] == 2008) {
                agg_unins$pct_growth[i] <- (agg_unins$sum_count[i] - agg_unins$sum_count[i - 1]) / agg_unins$sum_count[i - 1]
        }
}

# graph agg_unins over time
ggplot(agg_unins, aes(x = year, y = pct_growth, group = exp_dummy, color = factor(exp_dummy))) + 
        geom_line() + ggtitle("uninsured, pct growth in count")

# aca_unins, mean percent of pop uninsured
agg_unins <- aca_unins %>% group_by(exp_dummy, year) %>% summarize(avg_pct_pop = mean(as.numeric(total_pct)))

# graph agg_unins over time
ggplot(agg_unins, aes(x = year, y = avg_pct_pop, group = exp_dummy, color = factor(exp_dummy))) + 
        geom_line() + ggtitle("uninsured, avg pct of pop")


# aca_medicaid, percentage growth over time
ggplot(aca_medicaid, aes(x = year, y = pct_growth_count, group = location, color = factor(exp_dummy))) + 
        geom_line() + ggtitle("medicaid, pct growth of count")

agg_medicaid <- aca_medicaid %>% group_by(exp_dummy, year) %>% summarize(sum_count = sum(as.numeric(total_count)))
agg_medicaid$pct_growth <- 0
for(i in 2:nrow(agg_medicaid)) {
        if(!agg_medicaid$year[i] == 2008) {
                agg_medicaid$pct_growth[i] <- (agg_medicaid$sum_count[i] - agg_medicaid$sum_count[i - 1]) / agg_medicaid$sum_count[i - 1]
        }
}

# graph medicaid pct of pop growth over time over time
ggplot(agg_medicaid, aes(x = year, y = pct_growth, group = exp_dummy, color = factor(exp_dummy))) + 
        geom_line() + ggtitle("medicaid, pct growth in count")

# aca_medicaid, count of pop over time
ggplot(agg_medicaid, aes(x = year, y = sum_count, group = exp_dummy, color = factor(exp_dummy))) +
        geom_line() + ggtitle("medicaid, count of pop") + scale_y_continuous(labels = comma)

# aca_medicaid, mean percent of pop 
agg_medicaid <- aca_medicaid %>% group_by(exp_dummy, year) %>% summarize(avg_pct_pop = mean(as.numeric(total_pct)))

# graph medicaid, mean percent of pop 
ggplot(agg_medicaid, aes(x = year, y = avg_pct_pop, group = exp_dummy, color = factor(exp_dummy))) + 
        geom_line() + ggtitle("medicaid, avg pct of pop")


# population growth over time
agg_pop <- aca_medicaid %>% group_by(exp_dummy, year) %>% summarize(sum_count = sum(as.numeric(pop)))
agg_pop$pct_growth <- 0
for(i in 2:nrow(agg_pop)) {
        if(!agg_pop$year[i] == 2008) {
                agg_pop$pct_growth[i] <- (agg_pop$sum_count[i] - agg_pop$sum_count[i - 1]) / agg_pop$sum_count[i - 1]
        }
}

# graph agg_pop over time
ggplot(agg_pop, aes(x = year, y = pct_growth, group = exp_dummy, color = factor(exp_dummy))) + 
        geom_line() + ggtitle("population growth")

# population count over time
ggplot(agg_pop, aes(x = year, y = sum_count, group = factor(exp_dummy), color = factor(exp_dummy))) + 
        geom_line() + ggtitle("population count") + scale_y_continuous(labels = comma)

# early expansion states
early_exp <- aca %>% filter(location %in% c("California", "Connecticut", "Dist. of Columbia",
        "Minnesota", "New Jersey", "Washington"), coverage_type == "Medicaid/CHIP") %>% 
        select(year, location, total_count, total_pct, coverage_type, 
               exp_date, exp)

ggplot(early_exp, aes(x = year, y = as.numeric(total_pct), group = location, color = location)) + geom_line() +
        ggtitle("medicaid coverage, pct of pop in early expansion states")

ggplot(early_exp, aes(x = year, y = as.numeric(total_count), group = location, color = location)) + geom_line() +
        ggtitle("medicaid, early exp states count of pop")






# diff-in-diff regression with state and year fixed effects
# medicaid, pct of pop, black
agg_medicaid <- aca_medicaid %>% group_by(exp_dummy, year) %>% 
        summarize(avg_black_pct = mean(as.numeric(black_pct), na.rm = TRUE))

ggplot(agg_medicaid, aes(x = year, y = avg_black_pct, group = exp_dummy, 
        color = factor(exp_dummy))) + geom_line() + ggtitle("medicaid, pct of black pop")

m1_medicaid <- lm(black_pct ~ exp, data = aca_medicaid)
coeftest(m1_medicaid)
# exp         0.0384400  0.0216596  1.7747  0.07686 .
m1.vcovCL <- cluster.vcov(m1_medicaid, aca_medicaid$location)
coeftest(m1_medicaid, m1.vcovCL)
# exp         0.038440   0.016797  2.2885  0.02274 * 

m2_medicaid <- lm(black_pct ~ exp + factor(location) + factor(year), data = aca_medicaid)
coeftest(m2_medicaid)
# exp                                0.0284904  0.0138280   2.0603 0.0403032 *  
m2_medicaid.vcovCL <- cluster.vcov(m2_medicaid, aca_medicaid$location)
coeftest(m2_medicaid, m2_medicaid.vcovCL)
# exp                                2.8490e-02  1.4901e-02  1.9119e+00  0.056918 . 

m3_medicaid <- lm(black_pct ~ exp + factor(year) + factor(location) * year, data = aca_medicaid)
coeftest(m3_medicaid)
# exp                                     2.6758e-02  1.7273e-02  1.5491  0.122733 
m3_medicaid.vcovCL <- cluster.vcov(m3_medicaid, aca_medicaid$location)
coeftest(m3_medicaid, m3_medicaid.vcovCL)
# exp                                     2.6758e-02  1.5143e-02  1.7670e+00  0.078566 . 

m4_medicaid <- lm(black_pct ~ exp + factor(location) + factor(year), data = aca_medicaid, weights = pop)
coeftest(m4_medicaid)
# exp                                0.0332958  0.0079227   4.2026 3.567e-05 ***
m4_medicaid.vcovCL <- cluster.vcov(m4_medicaid, aca_medicaid$location)
coeftest(m4_medicaid, m4_medicaid.vcovCL)
# exp                                3.3296e-02  8.0199e-03      4.1517 4.403e-05 ***




# private insurance, pct of pop
aca_priv <- filter(aca, coverage_type == "Private")

agg_priv <- aca_priv %>% group_by(exp_dummy, year) %>% 
        summarize(avg_total_pct = mean(as.numeric(total_pct)))

ggplot(agg_priv, aes(x = year, y = avg_total_pct, group = exp_dummy, color = factor(exp_dummy))) + 
        geom_line() + ggtitle("Private, pct of pop")

aca_priv %>% filter(year %in% c(2012, 2014)) %>% 
        group_by(exp_dummy, year) %>% summarize(avg_priv = mean(as.numeric(total_pct))) %>% ungroup() %>%
        summarize(diff_non_exp = avg_priv[2] - avg_priv[1], 
                  diff_exp = avg_priv[4] - avg_priv[3],
                  diff_in_diff = diff_exp - diff_non_exp)

# exp_dummy year  avg_priv
# 1         0 2012 0.6623404
# 2         0 2014 0.6782784
# 3         1 2012 0.6856258
# 4         1 2014 0.6914235

# diff_non_exp    diff_exp diff_in_diff
# 1     0.015938 0.005797692  -0.01014031

m1_priv <- lm(total_pct ~ exp, data = aca_priv)
coeftest(m1_priv)
# exp         0.0043165  0.0139272   0.3099   0.7568   
m1.vcovCL <- cluster.vcov(m1_priv, aca_priv$location)
coeftest(m1_priv, m1.vcovCL)
# exp         0.0043165  0.0097733  0.4417    0.659 

m2_priv <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_priv)
coeftest(m2_priv)
# exp                               -1.0220e-02  2.7741e-03  -3.6842 0.0002723 ***
m2_priv.vcovCL <- cluster.vcov(m2_priv, aca_priv$location)
coeftest(m2_priv, m2_priv.vcovCL)
# exp                               -1.0220e-02  3.4823e-03 -2.9349e+00  0.003595 ** 

m3_priv <- lm(total_pct ~ exp + factor(year) + factor(location) * year, data = aca_priv)
coeftest(m3_priv)
# exp                                    -4.9414e-03  2.9802e-03  -1.6581 0.0985627 . 
m3_priv.vcovCL <- cluster.vcov(m3_priv, aca_priv$location)
coeftest(m3_priv, m3_priv.vcovCL)
# exp                                    -4.9414e-03  3.9233e-03  -1.2595 0.2090244    

m4_priv <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_priv, weights = pop)
coeftest(m4_priv)
# exp                               -1.0346e-02  2.0834e-03  -4.9657 1.153e-06 ***
m4_priv.vcovCL <- cluster.vcov(m4_priv, aca_priv$location)
coeftest(m4_priv, m4_priv.vcovCL)
# exp                               -1.0346e-02  5.1841e-03     -1.9957 0.0468769 *  

m5_priv <- lm(total_pct ~ exp + factor(location) * year + factor(year), data = aca_priv, weights = pop)
coeftest(m5_priv)
# exp                                    -8.2480e-03  1.8628e-03  -4.4277 1.427e-05 ***
m5_priv.vcovCL <- cluster.vcov(m5_priv, aca_priv$location)
coeftest(m5_priv, m5_priv.vcovCL)
# exp                                    -8.2480e-03  4.2923e-03    -1.9216 0.0558023 .




# individual insurance, pct of pop
aca_indiv <- filter(aca, coverage_type == "Individual")

agg_indiv <- aca_indiv %>% group_by(exp_dummy, year) %>% 
        summarize(avg_total_pct = mean(as.numeric(total_pct)))

ggplot(agg_indiv, aes(x = year, y = avg_total_pct, group = exp_dummy, color = factor(exp_dummy))) + 
        geom_line() + ggtitle("individual, pct of pop")

aca_indiv %>% filter(year %in% c(2012, 2014)) %>% 
        group_by(exp_dummy, year) %>% summarize(avg_indiv = mean(as.numeric(total_pct))) %>% ungroup() %>%
        summarize(diff_non_exp = avg_indiv[2] - avg_indiv[1], 
                  diff_exp = avg_indiv[4] - avg_indiv[3],
                  diff_in_diff = diff_exp - diff_non_exp)

# exp_dummy year avg_indiv
# 1         0 2012 0.1323364
# 2         0 2014 0.1399832
# 3         1 2012 0.1254769
# 4         1 2014 0.1294508

# diff_non_exp    diff_exp diff_in_diff
# 1    0.0076468 0.003973846 -0.003672954

m1_indiv <- lm(total_pct ~ exp, data = aca_indiv)
coeftest(m1_indiv)
# exp         -0.0057837  0.0060123  -0.962   0.3367     
m1.vcovCL <- cluster.vcov(m1_indiv, aca_indiv$location)
coeftest(m1_indiv, m1.vcovCL)
# exp         -0.0057837  0.0048210 -1.1997   0.2311 

m2_indiv <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_indiv)
coeftest(m2_indiv)
# exp                               -0.00627862  0.00244589  -2.5670 0.0107446 *  
m2_indiv.vcovCL <- cluster.vcov(m2_indiv, aca_indiv$location)
coeftest(m2_indiv, m2_indiv.vcovCL)
# exp                               -6.2786e-03  3.3023e-03 -1.9013e+00  0.058227 . 

m3_indiv <- lm(total_pct ~ exp + factor(year) + factor(location) * year, data = aca_indiv)
coeftest(m3_indiv)
# exp                                    -4.8166e-03  2.5423e-03 -1.8946 0.0593050 .   
m3_indiv.vcovCL <- cluster.vcov(m3_indiv, aca_indiv$location)
coeftest(m3_indiv, m3_indiv.vcovCL)
# exp                                    -4.8166e-03  2.5184e-03 -1.9125e+00 0.0569534 .     

m4_indiv <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_indiv, weights = pop)
coeftest(m4_indiv)
# exp                               -0.00788384  0.00148474  -5.3099 2.143e-07 ***
m4_indiv.vcovCL <- cluster.vcov(m4_indiv, aca_indiv$location)
coeftest(m4_indiv, m4_indiv.vcovCL)
# exp                               -7.8838e-03  1.9812e-03     -3.9794 8.677e-05 ***

m5_indiv <- lm(total_pct ~ exp + factor(location) * year + factor(year), data = aca_indiv, weights = pop)
coeftest(m5_indiv)
# exp                                    -5.6967e-03  1.5218e-03  -3.7435 0.0002254 ***
m5_indiv.vcovCL <- cluster.vcov(m5_indiv, aca_indiv$location)
coeftest(m5_indiv, m5_indiv.vcovCL)
# exp                                    -5.6967e-03  3.0508e-03    -1.8673  0.063037 .  


# employer insurance, pct of pop
aca_emp <- filter(aca, coverage_type == "Employer")

agg_emp <- aca_emp %>% group_by(exp_dummy, year) %>% 
        summarize(avg_total_pct = mean(as.numeric(total_pct)))

ggplot(agg_emp, aes(x = year, y = avg_total_pct, group = exp_dummy, color = factor(exp_dummy))) + 
        geom_line() + ggtitle("employer, pct of pop")

aca_emp %>% filter(year %in% c(2012, 2014)) %>% 
        group_by(exp_dummy, year) %>% summarize(avg_emp = mean(as.numeric(total_pct))) %>% ungroup() %>%
        summarize(diff_non_exp = avg_emp[2] - avg_emp[1], 
                  diff_exp = avg_emp[4] - avg_emp[3],
                  diff_in_diff = diff_exp - diff_non_exp)

# exp_dummy year   avg_emp
# 1         0 2012 0.5686180
# 2         0 2014 0.5657580
# 3         1 2012 0.5969496
# 4         1 2014 0.5895496

# diff_non_exp diff_exp diff_in_diff
# 1     -0.00286  -0.0074     -0.00454

m1_emp <- lm(total_pct ~ exp, data = aca_emp)
coeftest(m1_emp)
# exp         -0.0023236  0.0130153  -0.1785   0.8584        
m1.vcovCL <- cluster.vcov(m1_emp, aca_emp$location)
coeftest(m1_emp, m1.vcovCL)
# exp         -0.0023236  0.0090668 -0.2563   0.7979   

m2_emp <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_emp)
coeftest(m2_emp)
# exp                               -4.4762e-03  3.0569e-03  -1.4643 0.1441739   
m2_emp.vcovCL <- cluster.vcov(m2_emp, aca_emp$location)
coeftest(m2_emp, m2_emp.vcovCL)
# exp                               -4.4762e-03  3.4459e-03 -1.2990e+00     0.195 

m3_emp <- lm(total_pct ~ exp + factor(year) + factor(location) * year, data = aca_emp)
coeftest(m3_emp)
# exp                                     1.5843e-03  3.2968e-03   0.4806 0.6312426  
m3_emp.vcovCL <- cluster.vcov(m3_emp, aca_emp$location)
coeftest(m3_emp, m3_emp.vcovCL)
# exp                                     1.5844e-03  4.1366e-03  3.8300e-01 0.7020427 

m4_emp <- lm(total_pct ~ exp + factor(location) + factor(year), data = aca_emp, weights = pop)
coeftest(m4_emp)
# exp                               -3.1391e-03  2.1563e-03  -1.4557   0.14651  
m4_emp.vcovCL <- cluster.vcov(m4_emp, aca_emp$location)
coeftest(m4_emp, m4_emp.vcovCL)
# exp                               -3.1391e-03  5.6073e-03     -0.5598   0.57602  

m5_emp <- lm(total_pct ~ exp + factor(location) * year + factor(year), data = aca_emp, weights = pop)
coeftest(m5_emp)
# exp                                    -1.7809e-03  1.8735e-03  -0.9506 0.3427316
m5_emp.vcovCL <- cluster.vcov(m5_emp, aca_emp$location)
coeftest(m5_emp, m5_emp.vcovCL)
# exp                                    -1.7809e-03  3.0641e-03    -0.5812  0.561616




# medicaid table
stargazer(coeftest(m2_medicaid, m2_medicaid.vcovCL), coeftest(m3_medicaid, m3_medicaid.vcovCL), 
          coeftest(m4_medicaid, m4_medicaid.vcovCL), coeftest(m5_medicaid, m5_medicaid.vcovCL),
          type = "html", dep.var.labels   = "Medicaid insurance coverage, % of state pop.",
          omit = c("factor", "year"), out = "medicaid.html", covariate.labels = c("Medicaid expansion"),
          add.lines = list(c("State trends", "No", "Yes", "No", "Yes"), 
                           c("Pop. weights", "No", "No", "Yes", "Yes")), notes.align = "c", 
          keep = "exp",
        notes = "This table reports OLS regression DD estimates of expanded Medicaid insurance coverage on the percent of state population with Medicaid insurance coverage.  All models include state and year fixed effects.  Models (2) and (3) include state-specific linear time trends, and models (3) and (4) include weights by state population.  Standard errors clustered on states are reported in parenthesis.")

# uninsured table
stargazer(coeftest(m2_unins, m2_unins.vcovCL), coeftest(m3_unins, m3_unins.vcovCL), 
          coeftest(m4_unins, m4_unins.vcovCL), coeftest(m5_unins, m5_unins.vcovCL),
          type = "html", dep.var.labels   = "Uninsured, % of state pop.",
          omit = c("factor", "year"), out = "unins.html", covariate.labels = c("Medicaid expansion"),
          add.lines = list(c("State trends", "No", "Yes", "No", "Yes"), 
                           c("Pop. weights", "No", "No", "Yes", "Yes")), notes.align = "c", 
          keep = "exp",
          notes = "This table reports OLS regression DD estimates of expanded Medicaid insurance coverage on the percent of state population without insurance coverage.  All models include state and year fixed effects.  Models (2) and (3) include state-specific linear time trends, and models (3) and (4) include weights by state population.  Standard errors clustered on states are reported in parenthesis.")

# private insurance table
stargazer(coeftest(m2_priv, m2_priv.vcovCL), coeftest(m3_priv, m3_priv.vcovCL), 
          coeftest(m4_priv, m4_priv.vcovCL), coeftest(m5_priv, m5_priv.vcovCL),
          type = "html", dep.var.labels   = "Private insurance, % of state pop.",
          omit = c("factor", "year"), out = "priv.html", covariate.labels = c("Medicaid expansion"),
          add.lines = list(c("State trends", "No", "Yes", "No", "Yes"), 
                           c("Pop. weights", "No", "No", "Yes", "Yes")), notes.align = "c", 
          keep = "exp",
          notes = "This table reports OLS regression DD estimates of expanded Medicaid insurance coverage on the percent of state population with private insurance coverage.  All models include state and year fixed effects.  Models (2) and (3) include state-specific linear time trends, and models (3) and (4) include weights by state population.  Standard errors clustered on states are reported in parenthesis.")

# individual insurance table
stargazer(coeftest(m2_indiv, m2_indiv.vcovCL), coeftest(m3_indiv, m3_indiv.vcovCL), 
          coeftest(m4_indiv, m4_indiv.vcovCL), coeftest(m5_indiv, m5_indiv.vcovCL),
          type = "html", dep.var.labels   = "Individual-provided insurance, % of state pop.",
          omit = c("factor", "year"), out = "indiv.html", covariate.labels = c("Medicaid expansion"),
          add.lines = list(c("State trends", "No", "Yes", "No", "Yes"), 
                           c("Pop. weights", "No", "No", "Yes", "Yes")), notes.align = "c", 
          keep = "exp",
          notes = "This table reports OLS regression DD estimates of expanded Medicaid insurance coverage on the percent of state population with individual-provided insurance coverage.  All models include state and year fixed effects.  Models (2) and (3) include state-specific linear time trends, and models (3) and (4) include weights by state population.  Standard errors clustered on states are reported in parenthesis.")

# employer-provided insurance table
stargazer(coeftest(m2_emp, m2_emp.vcovCL), coeftest(m3_emp, m3_emp.vcovCL), 
          coeftest(m4_emp, m4_emp.vcovCL), coeftest(m5_emp, m5_emp.vcovCL),
          type = "html", dep.var.labels   = "Employer-provided insurance, % of state pop.",
          omit = c("factor", "year"), out = "emp.html", covariate.labels = c("Medicaid expansion"),
          add.lines = list(c("State trends", "No", "Yes", "No", "Yes"), 
                           c("Pop. weights", "No", "No", "Yes", "Yes")), notes.align = "c", 
          keep = "exp",
          notes = "This table reports OLS regression DD estimates of expanded Medicaid insurance coverage on the percent of state population with employer-provided insurance coverage.  All models include state and year fixed effects.  Models (2) and (3) include state-specific linear time trends, and models (3) and (4) include weights by state population.  Standard errors clustered on states are reported in parenthesis.")

# graph medicaid
agg_medicaid <- aca_medicaid %>% group_by(exp_dummy, year) %>% summarize(avg_pct_pop = mean(as.numeric(total_pct)))

ggplot(agg_medicaid, aes(x = year, y = avg_pct_pop, group = factor(exp_dummy), color = factor(exp_dummy))) + 
        geom_line() + ggtitle("Medicaid insurance coverage, avg. % of pop.") + 
        ylab("Avg. % of pop.") + xlab("Year") + scale_x_continuous(breaks=seq(2008, 2014, 1)) +
        theme_hc() + 
        scale_colour_economist(name = "", labels = c("Non-expansion states", "Expansion states"))

# graph unins
agg_unins <- aca_unins %>% group_by(exp_dummy, year) %>% summarize(avg_pct_pop = mean(as.numeric(total_pct)))

ggplot(agg_unins, aes(x = year, y = avg_pct_pop, group = factor(exp_dummy), color = factor(exp_dummy))) + 
        geom_line() + ggtitle("Uninsured, avg. % of pop.") + 
        ylab("Avg. % of pop.") + xlab("Year") + scale_x_continuous(breaks=seq(2008, 2014, 1)) +
        theme_hc() + 
        scale_colour_economist(name = "", labels = c("Non-expansion states", "Expansion states"))

# graph priv
agg_priv <- aca_priv %>% group_by(exp_dummy, year) %>% summarize(avg_pct_pop = mean(as.numeric(total_pct)))

ggplot(agg_priv, aes(x = year, y = avg_pct_pop, group = factor(exp_dummy), color = factor(exp_dummy))) + 
        geom_line() + ggtitle("Private insurance coverage, avg. % of pop.") + 
        ylab("Avg. % of pop.") + xlab("Year") + scale_x_continuous(breaks=seq(2008, 2014, 1)) +
        theme_hc() + 
        scale_colour_economist(name = "", labels = c("Non-expansion states", "Expansion states"))

# graph indiv
agg_indiv <- aca_indiv %>% group_by(exp_dummy, year) %>% summarize(avg_pct_pop = mean(as.numeric(total_pct)))

ggplot(agg_indiv, aes(x = year, y = avg_pct_pop, group = factor(exp_dummy), color = factor(exp_dummy))) + 
        geom_line() + ggtitle("Individual-provided insurance coverage, avg. % of pop.") + 
        ylab("Avg. % of pop.") + xlab("Year") + scale_x_continuous(breaks=seq(2008, 2014, 1)) +
        theme_hc() + 
        scale_colour_economist(name = "", labels = c("Non-expansion states", "Expansion states"))

# graph emp
agg_emp <- aca_emp %>% group_by(exp_dummy, year) %>% summarize(avg_pct_pop = mean(as.numeric(total_pct)))

ggplot(agg_emp, aes(x = year, y = avg_pct_pop, group = factor(exp_dummy), color = factor(exp_dummy))) + 
        geom_line() + ggtitle("Employer-provided insurance coverage, avg. % of pop.") + 
        ylab("Avg. % of pop.") + xlab("Year") + scale_x_continuous(breaks=seq(2008, 2014, 1)) +
        theme_hc() + 
        scale_colour_economist(name = "", labels = c("Non-expansion states", "Expansion states"))



# table of crowd-out calculations
# broad calculation
# for every x % rightly covered by medicaid, y % crowd out from private
m2_co1 <- (coefficients(m2_medicaid)[2] + coefficients(m2_unins)[2]) / coefficients(m2_medicaid)[2]
m3_co1 <- (coefficients(m3_medicaid)[2] + coefficients(m3_unins)[2]) / coefficients(m3_medicaid)[2]
m4_co1 <- (coefficients(m4_medicaid)[2] + coefficients(m4_unins)[2]) / coefficients(m4_medicaid)[2]
m5_co1 <- (coefficients(m5_medicaid)[2] + coefficients(m5_unins)[2]) / coefficients(m5_medicaid)[2]
co1 <- mean(m2_co1, m3_co1, m4_co1, m5_co1)

m2_co1_row <- c(coefficients(m2_medicaid)[2], coefficients(m2_unins)[2], m2_co1)
m3_co1_row <- c(coefficients(m3_medicaid)[2], coefficients(m3_unins)[2], m3_co1)
m4_co1_row <- c(coefficients(m4_medicaid)[2], coefficients(m4_unins)[2], m4_co1)
m5_co1_row <- c(coefficients(m5_medicaid)[2], coefficients(m5_unins)[2], m5_co1)

co1_df <- rbind(m2_co1_row, m3_co1_row, m4_co1_row, m5_co1_row)
co1_df <- data.frame(co1_df)
names(co1_df) <- c("Medicaid", "Uninsured", "Crowd-out ratio 1")
row.names(co1_df) <- c("Model 2", "Model 3", "Model 4", "Model 5")

write_csv(co1_df, "co1_df.csv")

# narrow calculation
# for every x % rightly covered by medicaid, x % crowd out from private
m2_co2 <- coefficients(m2_priv)[2] / coefficients(m2_medicaid)[2]
m3_co2 <- coefficients(m3_priv)[2] / coefficients(m3_medicaid)[2]
m4_co2 <- coefficients(m4_priv)[2] / coefficients(m4_medicaid)[2]
m5_co2 <- coefficients(m5_priv)[2] / coefficients(m5_medicaid)[2]
co2 <- mean(m2_co2, m3_co2, m4_co2, m5_co2)

m2_co2_row <- c(coefficients(m2_priv)[2], coefficients(m2_medicaid)[2], m2_co2)
m3_co2_row <- c(coefficients(m3_priv)[2], coefficients(m3_medicaid)[2], m3_co2)
m4_co2_row <- c(coefficients(m4_priv)[2], coefficients(m4_medicaid)[2], m4_co2)
m5_co2_row <- c(coefficients(m5_priv)[2], coefficients(m5_medicaid)[2], m5_co2)

co2_df <- rbind(m2_co2_row, m3_co2_row, m4_co2_row, m5_co2_row)
co2_df <- data.frame(co2_df)
names(co2_df) <- c("Private", "Medicaid", "Crowd-out ratio 1")
row.names(co2_df) <- c("Model 2", "Model 3", "Model 4", "Model 5")

write_csv(co2_df, "co2_df.csv")

# estimate average indiv
avg_indiv <- mean(c(coefficients(m2_indiv)[2], coefficients(m3_indiv)[2], coefficients(m4_indiv)[2],
                  coefficients(m5_indiv)[2]))
indiv_col <- data.frame("Individual" = c(coefficients(m2_indiv)[2], coefficients(m3_indiv)[2], coefficients(m4_indiv)[2],
               coefficients(m5_indiv)[2], avg_indiv))

# estimate average emp
avg_emp <- mean(c(coefficients(m2_emp)[2], coefficients(m3_emp)[2], coefficients(m4_emp)[2],
                  coefficients(m5_emp)[2]))
emp_col <- data.frame("Employer" = c(coefficients(m2_emp)[2], coefficients(m3_emp)[2], coefficients(m4_emp)[2],
                                         coefficients(m5_emp)[2], avg_emp))
# combine average indiv and emp
indiv_emp <- cbind(indiv_col, emp_col)
rownames(indiv_emp) <- c("Model 2", "Model 3", "Model 4", "Model 5", "Average")

write_csv(indiv_emp, "indiv_emp.csv")

# create descriptive statistics chart
# chart 1: by state
state_descrip <- aca %>% group_by(location) %>% summarize(
        Expansion = mean(as.numeric(exp_dummy)), 
        "Medicaid mean" = mean(as.numeric(total_pct[coverage_type == "Medicaid/CHIP"])),
        "Uninsured mean" = mean(as.numeric(total_pct[coverage_type == "Uninsured"])),
        "Private mean" = mean(as.numeric(total_pct[coverage_type == "Private"])),
        "Invididual mean" = mean(as.numeric(total_pct[coverage_type == "Individual"])),
        "Employer mean" = mean(as.numeric(total_pct[coverage_type == "Employer"])),
        "Population mean" = mean(as.numeric(pop))
        )

write_csv(state_descrip, "state_descrip.csv")

exp_descrip <- data.frame(aca %>% group_by(exp_dummy) %>% summarize(
        "State count" = length(unique(location)),
        "Medicaid mean" = mean(as.numeric(total_pct[coverage_type == "Medicaid/CHIP"])),
        "Medicaid sd" = sd(as.numeric(total_pct[coverage_type == "Medicaid/CHIP"])),
        "Uninsured mean" = mean(as.numeric(total_pct[coverage_type == "Uninsured"])),
        "Uninsured sd" = sd(as.numeric(total_pct[coverage_type == "Uninsured"])),
        "Private mean" = mean(as.numeric(total_pct[coverage_type == "Private"])),
        "Private sd" = sd(as.numeric(total_pct[coverage_type == "Private"])),
        "Invididual mean" = mean(as.numeric(total_pct[coverage_type == "Individual"])),
        "Invididual sd" = sd(as.numeric(total_pct[coverage_type == "Individual"])),
        "Employer mean" = mean(as.numeric(total_pct[coverage_type == "Employer"])),
        "Employer sd" = sd(as.numeric(total_pct[coverage_type == "Employer"])),
        "Population mean" = mean(as.numeric(pop)),
        "Population sd" = sd(as.numeric(pop))
        ))

write_csv(exp_descrip, "exp_descrip.csv")

# graph of state gdp
gdp <- read_csv("bea_state_gdp_clean.csv")
aca_gdp <- aca %>% filter(year == 2010, coverage_type == "Uninsured")
aca_gdp <- left_join(aca_gdp, gdp, by = "fips")
aca_gdp <- data.frame(aca_gdp)
aca_gdp2 <- aca_gdp
aca_gdp2 <- select(aca_gdp2, location, exp_dummy, X2000:X2014)
aca_gdp2 <- melt(aca_gdp, id.vars = c("location", "exp_dummy"), 
        measure.vars = c("X2000", "X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007",
                         "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014"), 
        variable.name = "year", value.name = "gdp_growth")
aca_gdp2$year <- str_replace(aca_gdp2$year, "X", "")

agg_gdp <- data.frame(aca_gdp2 %>% group_by(exp_dummy, year) %>% 
        summarize(avg_gdp_growth = mean(as.numeric(gdp_growth))))

ggplot(agg_gdp, aes(x = as.numeric(year), y = avg_gdp_growth, group = exp_dummy, color = factor(exp_dummy))) + geom_line()
ggplot(agg_gdp, aes(x = as.numeric(year), y = avg_gdp_growth, group = factor(exp_dummy), color = factor(exp_dummy))) + 
        geom_line() + ggtitle("Avg. state GDP growth") + 
        ylab("Avg. GDP growth") + xlab("Year") + scale_x_continuous(breaks=seq(2000, 2014, 1)) +
        theme_hc() + 
        scale_colour_economist(name = "", labels = c("Non-expansion states", "Expansion states"))