# z score, pop mean = 12.2%, sample mean = 14.4%, n = 9, pop sd = 3.6%
# prob sample mean > 14.4%
z_score <- (14.4 - 12.2) / (3.6 / sqrt(9))
pnorm(1.83, lower.tail = FALSE)

# problem 1
# sd = 15, u = 45, n = 5
# prob x-bar > 60
z_score <- (60-45) / (15 / sqrt(5))
pnorm(z_score, lower.tail = FALSE)
pnorm(60, mean = 45, sd = (15 / sqrt(5)), lower.tail = FALSE)

# this is working version of book example pg 232
qnorm(.0336, lower.tail = FALSE)
# uses pop mean, but sample standard error - gives wrong answer if you use pop sd
# answer to above: it's because we're describing the sampling distribution of the population
# so we know the samp dist will have the same mean as pop (u)
# but the sample dist will not have same sd as pop, since samp dist sd depends on n
# since we are modeling the samp dist, we must give the function the samp dist mean and sd
qnorm(.0336, mean = 12.2, sd = 1.2, lower.tail = FALSE)

qnorm(.2, mean = 45, sd = (15 / sqrt(5)))
pnorm(39.35423, mean = 45, sd = (15 / sqrt(5)))

# problem 2
# u = 20, sd = .6, n = 4
# part a
pnorm(19.4, mean = 20, sd = (.6 / sqrt(4)))
pnorm(21.2, mean = 20, sd = (.6 / sqrt(4)), lower.tail = FALSE)

# part b
below <- pnorm(19.5, mean = 20, sd = (.6 / sqrt(4)))
above <- pnorm(20.5, mean = 20, sd = (.6 / sqrt(4)), lower.tail = FALSE)
1 - below - above
