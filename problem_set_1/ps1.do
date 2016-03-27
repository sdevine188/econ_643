* change directory 
cd c:\users\stephen\desktop\stata\econ_643\problem_set_1


* q2)
* read in data
clear
import excel Bishop.xls, firstrow
describe

* create scatter plot of P and Q
graph twoway scatter Price Quantity
graph twoway (scatter Price Quantity) (lfit Price Quantity)

* q3)
* bike shop with average 4 arrivals per hour, poisson dist
* a given customers has 70% probability of ordering repairs
* x = number of people arriving per hour

* a) what is prob that 6 people arrive in one hour?
display poissonp(4, 6)

* b) prob of at least 1 arrival in hour?
display poissontail(4, 1)

* c) prob that exactly two customers arrive in next hour
* and that exactly one of them orders a repair

* prob of exactly two arrivals in hour
display poissonp(4, 2)

* prob of exactly one ordering repair
* this is a binomial dist, n = 2, k = 1, p = .7,
display binomialp(2, 1, .7) 

















