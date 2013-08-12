#### Problemset examples


# To check your solutions in RStudio save (Ctrl-s) and then source (Ctrl-Shift-s)
# If you check "Source on Save" in RStudio you just have to save (Ctrl-s)

ps.dir =  "C:/libraries/RTutor/RTutor/problemsets" # your working directory
ps.file = "ps_examples.r" # this file

library(RTutor)
check.problem.set("examples", ps.dir, ps.file)

###########################################
#### Exercise 1 (a simple calculation)
###########################################

# Compute 2*5 and store it in the variable x  
x = 2*5

#hint.for("1 (a simple calculation)")

#### end exercise 1 (a simple calculation)


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

# ii) Generate a vector of endogenous prices

# iii) Compute realized demand q from demand function q=beta0+beta1*p+eps



#### end exercise 2 (simulate demand function)

