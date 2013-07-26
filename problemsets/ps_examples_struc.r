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


###############
#$ solution
###############

T = 1000 # Number of markets
beta0 = 100
beta1 = -1


# Draw demand shocks eps from a normal distribution with mean 0 and variance 4
eps = rnorm(T,0,4)

# One solution to generate endogenous prices
p = eps/2

# Compute realized demand q
q = beta0 + beta1 * p + eps

################
#$ tests
################

# Check given variables
check.var(c("beta0","beta1","T"),
          exists=TRUE, length=TRUE, class=TRUE, values=TRUE)


# Check construction of eps
check.var(c("eps"),
          exists=TRUE, length=TRUE, class=TRUE)

test.normality(eps)
test.mean(eps,0)
test.variance(eps,4)

# Check p
check.var(c("p"),
          exists=TRUE, length=TRUE, class=TRUE)

check.H0.rejected(cor.test(p,eps),failure.message="
  I couldn't significantly reject the hypothesis that your prices are exogenous (H0: cor(p,eps)=0, p.value = {{p.value}}). Try harder to generate prices that are 'more endogenous'.")

# Check q
check.var(c("q"),
          exists=TRUE, length=TRUE, class=TRUE)

holds.true(q == beta0+beta1*p+eps, failure.message = 
"Sorry, you have not yet specified the right demand equation to compute q...")

#$ end_exercise

