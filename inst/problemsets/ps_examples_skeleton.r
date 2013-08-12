#### Problemset examples


library(RTutor)
ps.dir = "C:/libraries/RTutor/RTutor/problemsets" # e.g. "C:/problemsets"
ps.file = "ps_examples.r"

check.problem.set("examples", ps.dir,ps.file, reset=FALSE)


#####################################################
Exercise("1 (a simple calculation)",{       
#####################################################

# Compute 2*5 and store it in the variable x  
2*5
   
  
    
}) # end exercise 1 (a simple calculation)


#####################################################
Exercise("2 (a simple calculation)",{       
#####################################################
  
  
# Draw 
# Simulate T outputs from a demand function
# q = beta0 + beta1 * p + eps
# 


T = 1000 # Number of markets
beta0 = 100
beta1 = -1

# i) Draw T demand shocks eps from a normal distribution with mean 0 and variance 4
eps = rnorm(T,0,2)

# ii) Generate a vector of endogenous prices

# iii) Compute realized demand q from demand function q=beta0+beta1*p+eps

  
  

}) # end exercise 1 (a simple calculation)



expr = quote({
  # Ho
  x = 5
  
})

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
eps = rnorm(T,0,2)

# ii) Generate a vector of endogenous prices

# iii) Compute realized demand q from demand function q=beta0+beta1*p+eps


###############
#### end exercise 2 (simulate demand function)
check.exercise("2 (simulate demand function)")
