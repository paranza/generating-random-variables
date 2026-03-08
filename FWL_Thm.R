#Traditional multilinear regression
model <- with(houses, lm(price ~ lot + bdrms + fb + sty + drive + rec +
                           ffinbmnt + ghw + ca + garage + pref))
summary(model) #beta1 estimate is 3.546

#Applying FWL Thm:

#Step 1: regress X1 on X2, ..., Xk
model1 <- with(houses, lm(lot ~ bdrms + fb + sty + drive + rec +
                            ffinbmnt + ghw + ca + garage + pref))
#summary(model1)
resx1 <- residuals(model1) #Save residuals 

#Step 2: regress Y on X2, ..., Xk
model2 <- with(houses, lm(price ~ bdrms + fb + sty + drive + rec +
                            ffinbmnt + ghw + ca + garage + pref))
#summary(model2)
resy <- residuals(model2) #Save residuals 

model3 <- with(houses, lm(resy ~ resx1))
summary(model3) #coeff estimate is 3.546, exactly beta1