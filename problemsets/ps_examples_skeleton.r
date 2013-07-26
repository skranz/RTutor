#### Problemset examples


library(RTutor)
# Add the path where you store this file
setwd("C:/libraries/RTutor/RTutor/problemsets")
set.student.file("C:/libraries/RTutor/RTutor/problemsets/ps_examples_skeleton.r") # Path and name of this file
load.problem.set("examples")

###########################################
#### Exercise 1 (a simple calculation)
###########################################
  
# Compute 2*5 and store it in the variable x  
x=10
#### end exercise 1 (a simple calculation)
check.exercise("1 (a simple calculation)")

###########################################
#### Exercise 2 (simulate demand function)
###########################################

# Draw 
# Simulate T outputs from a demand function
# q = beta0 + beta1 * p + eps
# 


T = 1000 # Number of markets
beta0 = 100
beta1 = -1

# i) Draw T demand shocks eps from a normal distribution with mean 0 and variance 4
eps=rnorm(T,0,2)
# ii) Generate a vector of endogenous prices
p=runif(T,0,10)
p = runif(T,0,20)+0.001*eps
# iii) Compute realized demand q from demand function q=beta0+beta1*p+eps
q = beta0+beta1*p+eps

###############
#### end exercise 2 (simulate demand function)
check.exercise("2 (simulate demand function)")
