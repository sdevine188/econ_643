* not sure why this gives different answer from r pnorm and book (pg 232)
display normalden(1.83) / 2

* set working directory
pwd
cd C:\Users\Stephen\desktop\stata\econ_643\problem_set_2
ls

* load insurance data
sysuse insurance1.dta

* inspect data
describe
summarize

* problem 3
* part a

* get number of obs
count

* get range of ages
summarize age

* range of household incomes
summarize hhincome

* how many obs have hhincome == 0?
count if hhincome == 0

* part b

* how many people have private insurance?
count if ins == 1

* what percentage of the sample have private insurance?
display 1241 / 3206

* part c

* median, mean, and sd of hhincome
tabstat hhincome, statistics(median, mean, sd) 

* w summarize/detail, calculate 10th and 95th pct, and skewness, for hhincome
summarize hhincome, detail

* part e

* create histogram of age
histogram age
histogram age, bin(34) start(50) title("Age of Individuals")



