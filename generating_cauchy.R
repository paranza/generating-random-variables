n = 1000
sigma = 2 
test = rcauchy(n,0,sigma^2)

u = runif(n,0,1)
x = sigma*tan(pi*(u-0.5))

hist(x, breaks = 100)
hist(test, breaks = 100)

