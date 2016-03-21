* using binomial distribution, find probability that daughter successfully 
* brings food to mouth 3 times in 10 tries if probability of success in single
* trial is .25

* http://www.stat.ucla.edu/~rgould/labs/nicholas/lab5.pdf
* use help binomial
display binomialp(10, 3, 0.25)
display binomialp(5, 1, 0.4)


* this does not give specific prob of x, but instead cumulativeprob x >= and <=
* bitesti command takes arg (n = 10, x = 3, p = .25)
bitesti 10 3 .25
bitesti 5 1 .4
bitesti 5 1 .1

* using poisson distribution, find probabilty that daughter wakes up 4 times in
* 8 hour interval, given the average count of wake-ups is 2 per 8 hour interval
 * first install probcalc
 ssc install probcalc
 probcalc p 2 exactly 4
probcalc p 2 atmost 4
probcalc p 2 atleast 4
probcalc p 3 exactly 5
probcalc p 10 atleast 10
display poissontail(10, 10)

* can also use poissonp to get exact poisson probabilities
display poissonp(3, 5)

* poisson and binomial are similar 
display poissonp(50, 20)
display binomialp(1000, 20, .05)
display poissonp(50, 50)
display binomialp(1000, 50, .05)
* at higher counts of events they start to diverge more noticeably
display poissonp(50, 90)
display binomialp(1000, 90, .05)

display binomialp(6, 2, .17)


