36/120
dbinom(3, size=10, prob=0.25) 
dbinom(1, size = 5, prob = .4)

p(x) = (e^-lambda (lambda^x)) / x!
(((exp(1))^-2) * (2^4)) / factorial(4)
# to get probability of exactly 4
dpois(x = 4, lambda = 2)
# to get prob of 4 or less
ppois(q = 4, lambda = 2)
# to get prob of 4 or more
ppois(q = 3, lambda = 2, lower.tail = FALSE)


