p = 0.6
n = 100000
sample = runif(n,0,1)
bern = c(1:n) * 0 

bern[sample>1-p] = 1 
sum(bern)
