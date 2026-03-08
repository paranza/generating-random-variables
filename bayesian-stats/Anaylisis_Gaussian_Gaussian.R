library(ggplot2)


#Suppose I want to learn theta from a sequence of iid from a Gaussian distribution N(true_theta, known_var)

#What is known:
known_var = 1 #This is not often the case
n = 10
observations = rnorm(n,true_theta,sqrt(known_var)) #data given from unknown distr parameter
obs_mean = mean(observations) #frequentist estimator

#What we want to learn:
true_theta = 9


#In order to proceed,
#We now find a non-informative prior N(v, tau**2)
#For example: N(0, 100)

#Or, we can set a more informative prior,
v = 4 #E(theta) which is our prior guess
tau = sqrt(20) #We set a high value for low info

#RMK: as seen from multiple tests, 
#The informativeness set by us in the prior distribution is not that important,
#Meaning that it won't affect the posterior that much


#We now evaluate the posterior
exp_theta_given_obs = (v)*((known_var) / (n*tau**2 + known_var**2) ) +
  (obs_mean)*((n*tau**2)/(n*tau**2+ known_var) ); exp_theta_given_obs 

var_theta_given_obs = (known_var*tau**2) / (n*tau**2+known_var); var_theta_given_obs
  
#Comments
#1) If we chose a great var, it would still have shrunk significantly
#2) Expectation got closer to true value of theta


#Let's write a function of the probability of the prediction 
#Given X_(n+1) ~ N(exp_theta_given_obs, var_theta_given_obs + known_var)

probability_of = function(xn1) {
  mean_pred = exp_theta_given_obs
  var_pred = known_var + var_theta_given_obs
  dens = dnorm(xn1, mean = mean_pred, sd = sqrt(var_pred))
  data.frame(xn1 = xn1, density = dens)
}
#Let's see where it is maximized (in this case)

tests = seq(0,20, by = 0.1) #; tests
probabilities = probability_of(tests) #; probabilities
max(probabilities[,2]) #0.3950038
probabilities[which.max(probabilities[, 2]),1] #9.1

#Comments
#1) The value that maximizes the probability of the next value 
#   is close to the expected value (mu) of the true probability
#3) Doubt: IS IT MAXIMIZED BY THE EXPECTED VALUE OF THE POSTERIOR? 
#   Not really, altough it converges to it if n--> inf


#Plotting
x = seq(5, 15, length=1000)
df = data.frame(
  x = seq(5, 15, length=1000),
  true = dnorm(x, mean=true_theta, sd=sqrt(known_var)),
  posterior = dnorm(x, mean=exp_theta_given_obs, sd=sqrt(var_theta_given_obs)),
  prior = dnorm(x, mean=v, sd=tau)
)

ggplot(df, aes(x)) +
  geom_line(aes(y = true, color = "true")) +
  geom_line(aes(y = prior, color = "prior")) +
  geom_line(aes(y = posterior, color = "posterior")) +
  labs(y = "y", x = "x", color = "Function") +
  theme_minimal()