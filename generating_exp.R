n = 100000
sample = runif(n,0,1)
exp = c(1:n) * 0 
lamda = 0.2

exps = c(-lamda^(-1)*log(1-sample))
xs = c(1:n)

boxplot(exps)
hist(exps)

mean(exps)
1/lamda
