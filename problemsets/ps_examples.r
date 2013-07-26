#### Problemset examples


library(RTutor)
# Add the path where you store this file
setwd("C:/libraries/RTutor/RTutor/problemsets")
set.student.file("C:/libraries/RTutor/RTutor/problemsets/ps_examples.r") # Path and name of this file
load.problem.set("examples")

###########################################
#### Exercise 1 (a simple calculation)
###########################################
  
# Compute 2*5 and store it in the variable x  

x = 2*5

#### end exercise 1 (a simple calculation)
check.exercise("1 (a simple calculation)")

###########################################
#### Exercise 2 (simulate demand function)
###########################################

# Simulate T outputs from a demand function
# q = beta0 + beta1 * p + eps


T = 1000 # Number of markets
p = runif(T, 0,100) # Prices
beta0 = 100
beta1 = -1


# Draw demand shocks eps from a normal distribution with mean 0 and variance 4
eps=rnorm(T,0,2)
# Compute realized demand q
q = beta0 + beta1 * p + eps

#### end exercise 2 (simulate demand function)
check.exercise("2 (simulate demand function)")
