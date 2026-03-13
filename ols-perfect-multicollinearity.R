#Testing that if a regressor can be described perfectly 
#with a linear function of another regressor, the ols 
#gives N/A for that first regressor

x = runif(1000,0,4)
b1 = 1 
b0 = 1
y_true = b0 + b1*x
eps = rnorm(1000,0,0.3)
y_noise = y_true + eps

reg1 = rbinom(1000,1,0.3)
reg2 = 1 - reg1 #dummy var

model = lm(y_noise ~ reg1 + reg2)
summary(model) #reg2 is N/A, linearly dependant from reg1 