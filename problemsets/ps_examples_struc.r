#$ problem_set examples

###############################################################

#$ exercise 1 (a simple calculation)
  
# Compute 2*5 and store it in the variable x  

#$ solution

x <- 2*5

#$ tests
check.var("x", exists=TRUE,length=TRUE, values=TRUE)

#$ end_exercise

###############################################################


#$ exercise 2 (simulate demand function)

# Simulate T outputs from a demand function
# q = beta0 + beta1 * p + eps


T = 1000 # Number of markets
p = runif(T, 0,100) # Prices
beta0 = 100
beta1 = -1


# Draw demand shocks eps from a normal distribution with mean 0 and variance 4

# Compute realized demand q


#$ solution

T = 1000 # Number of markets
p = runif(T, 0,100) # Prices
beta0 = 100
beta1 = -1


# Draw demand shocks eps from a normal distribution with mean 0 and variance 4
eps = rnorm(T,0,4)

# Compute realized demand q
q = beta0 + beta1 * p + eps


#$ tests

# Check given variables
check.var(c("beta0","beta1","T"),
          exists=TRUE, length=TRUE, class=TRUE, values=TRUE)
check.var(c("p"),
          exists=TRUE, length=TRUE, class=TRUE)

# Check construction of eps
check.var(c("eps"),
          exists=TRUE, length=TRUE, class=TRUE)

test.normality(eps)
test.mean(eps,0)
test.variance(eps,4)

# Check q
check.var(c("q"),
          exists=TRUE, length=TRUE, class=TRUE)

holds.true(q == beta0+beta1*p+eps, failure.message = 
"Sorry, you have not yet specified the right demand equation to compute q...")

#$ end_exercise

