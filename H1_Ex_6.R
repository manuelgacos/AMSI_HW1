# Create a data table that will store the information from all the simulations
beta1 = rep(0, times = 100)
beta_lower = rep(0, times = 100)
beta_upper = rep(0, times = 100)
sigma = rep(0, times = 100)
sigma_squared = rep(0, times = 100)
ci = rep(0, times = 100)
betas = data.frame(beta1,beta_lower,beta_upper,sigma,sigma_squared,ci)

# Run 100 simulations of the model, generating 100 xi's and 100 yi's
n = 1:100
x = 1/n
for (i in 1:100) {
  e = rnorm(100)
  y = 1+2*x+e
  data = data.frame(x,y)
  # Make the linear regression model
  linearMod = lm(formula = y~x, data = data)
  # Obtain the regression coefficients
  coefficients = coef(linearMod)
  beta1 = coefficients[2]
  betas[i,1] = beta1
  # Obtain the confidence interval of beta1
  beta1_lower = confint(linearMod)[2,1]
  betas[i,2] = beta1_lower
  beta1_upper = confint(linearMod)[2,2]
  betas[i,3] = beta1_upper
  # Obtain variance and standard deviation
  sigma = summary(linearMod)$coefficients[2,2]
  betas[i,4] = sigma
  sigma_squared = sigma^2
  betas[i,5] = sigma_squared
  # Check if beta1 is in the confidence interval
  if (beta1_lower<= 2 && 2 <= beta1_upper){
    betas[i,6] = 1
    }
}

# a)
emp_bias =abs(mean(betas$beta1) - 2)
print(paste('The empirical bias is ',emp_bias))
emp_variance = var(betas$beta1)
print(paste('The empirical variance is ',emp_variance))

# b)
ci_coverage = mean(betas$ci)
print(paste('The empirical coverage of the CI for Beta1 is ', ci_coverage, ' percent'))

# c)

# We have real_beta1 = 2 and real_beta0 = 1

sse = sum( (y - (1+2*x))^2 )
mse = sse/98
s_squared = mse/sum( (x-mean(x))^2 )
s = sqrt(s_squared)
print(paste('The theoretical variance is ', s_squared))
print('We can say that the empirical results agree with their theoretical values')

write.csv(betas,'parameters.csv')