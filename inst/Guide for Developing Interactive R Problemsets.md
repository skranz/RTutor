Tutorial for Developing Interactive R Problem Sets with RTutor
===============================================================

**Date: 2013-08-11**

**Author: Sebastian Kranz**

RTutor is an R package that allows to develop interactive R exercises. The interactive exercises, can directly test a student's solution and provide hints if not everything is correct. A problems et is just a collection of interactive exercises. This document gives you an overview how to generate an interactive problem set. I assume that you are using RStudio.

## A step by step guide

### 1. Install the newest version of RTutor
You first need to install some other packages. Simply run in R

```r
install.packages("devtools", "whisker", "stringr")

```

To install the newest version of restorepoint and RTutor from Github run then

```r
library(devtools)
install_github(repo = "restorepoint", username = "skranz")
install_github(repo = "RTutor", username = "skranz")
library(restorepoint)
library(RTutor)
```


### 2. Create template files for developing a new problem set

We first need to generate a folder in which some example files will be generated that help us to build a new problem set. First decide on a name for the problemset, e.g. "my ps" and generate a parent folder in which you want to save your problemsets, e.g. "C:/problemsets". (Henceforth I will always assume that you named your problem set "my ps". Just replace "my ps" everywhere by your own name) Then run the following code:


```r
develop.problem.set(name = "my ps", parent.path = "C:/libraries/RTutor/problemsets")
```


It generates a new folder below parent.path with the name of the problem set, here "C:/problemsets/my ps".

The generated folder contains 2 files:
* my ps_struc.r
* make_student_ps.r

Note: If these files do already exist, the existing files are by default **not** overwritten. You have to delete them manually if you want to generate new template files.

The file "my ps_struc.r" will contain all the **structure** of your problem set that specifies for several exercises:

* a task description shown to the student
* a solution for the exercise
* several tests that will check a student's solution and provide hints

Originally, there is an example structure with two simple exercies. We will explain later in detail, how you create a structure for your own problem set..

The file "make_student_ps.r" contains a few lines of code that will generate from your problem set structure an empty problem set for the student.

### 3. Generate an empty problem set for students

Open the file "make_student_ps.r" and run all its code. In your problem set folder, a new file, "my ps.r" should now have been generated. This is the file that will be given to students and that they shall try to solve.

### 4. Feel like a student: Try to solve the problem set

Open the generated file "my ps.r" in RStudio. The file has some commands in the beginning, which are used to initialize RTutor. You can ignore them for the moment. Then there is a description for two exercises that tell you where you can enter your own code. Try to solve the exercises (or just exercise 1). Try also to make some mistakes, to check how whether these mistakes are found out.

### 5. Feel like a student: Check your solution

When you have written some code:

1. First **save your file** (Ctrl-s or press the 'disk symbol')
2. Then source your file (Ctrl-Shift-s or press the symbol 'Source' on top of your code to the right).

RTutor will now check the solutions to your exercise and either give you a message that something is wrong, some warning messages or says everything is fine. Try out yourself...  

Hint: In RStudio above the code window there is a check box "Source on Save". I would recommend to check this box for "my ps.r". Now you only have to press Ctrl-s to automatically save and check your problem set.

### 6. Change the structure of your problem set

You can now change the structure of your problem set. This is explained in detail in the next section. Once, you have changed the structure, you can test it again by repeating steps 4 and 5 (and perhaps step 3).

## Specifying the structure of a problem set

If you open "my ps_struc.r", you see an example structure of a problem set. Here is the header and structure for the first exercise:


```r
#$ problem_set my ps


###############################################################
#$ exercise a)
###############################################################

#$ task #######################################################
# Compute 2*5 and store it in the variable x  


#$ solution ###################################################

x <- 2*5

#$ tests ######################################################
check.var("x", exists=TRUE,length=TRUE, values=TRUE)

#$ hints ######################################################
add.hint("Hint 1",{cat('
# The following code stores 2+3 in a variable y
y = 2+3"
')})

#$ end_exercise

```


Lines that start with #$ are special commands that are used by RTutor to parse different aspects of the problem sets. Other lines are all valid R commands. I explain the commands in detail:

### Specify name of a problem set
The command


```r
# $ problem_set my ps
```


simply sets the name of the problem set equal to the text after problem_set. Here the name is set to "my ps". As in this example, the name of a problem set can contain spaces.

### Specify an exercise
A problem set can have several exercises. Each exercises must have the following command lines in **exactly the order shown below**. After most command lines some R code can be entered.  


```r
#$ exercise name_of_exercise

  # No code will be entered here 

#$ task

  # Code that will be part of the empty problem set given to the student

#$ solution

  # Code that would correctly solve the exercise

#$ tests

  # Tests that will be performed on the student's code

#$ hints

  # Code that specifies hints that can be shown to the
  # students in specified situations

#$ end_exercise
```


I will now explain the different commands
#### #$ exercise
This command is at the start of an exercise. After the word exercise, a name of the exercise is specified, e.g.

```r
# $ exercise a)
```

generates an exercise with name "a)". So far, we don't use any R code directly after the command.

#### #$ task
Below this command, you enter the text will be shown in the empty problem set given to the user. The text should make clear, what the student shall do and where she should enter her code.  Unless, you have a good reason, this text should be valid R code, i.e. everything that is used for explanations should be commented with #.

Example:

```r
#$ task #######################################################

# Compute 2*5 and store it in the variable x  

```


#### #$ solution
Below this command, you enter R code that completely solves the exercise.
Example:

```r
#$ solution ###################################################

x <- 2*5
```


#### #$ tests
Below this command you specify different tests that will be performed on the student's solution to check whether some mistake can be found. If something wrong is encountered, the tests give feedback to the student.

The simple test in our example below, checks the following:

  * Has the user generated a variable x
  * Has x the same length as in the given solution, i.e. 1
  * Has x the same value as in the given solution, i.e. x=10


```r
#$ tests ###################################################

check.var("x", exists=TRUE,length=TRUE, values=TRUE)

```

If one test fails, e.g. if x does not exist. The user will be shown a standard failure message and no more code will be checked. We consider a student's solution "correct" if no test fails, i.e. we are not able to falsify the correctness.

Good and well designed tests are the key component of a good interactive problem set. There already exist several functions, like check.var, that can be used to perform different sorts of tests. They are explained in some detail in the next section.
If more functions are needed, e.g. a function that scans the code of the user for a particular command, send me an email or file an issue on GitHub.

#### #$ hints

In the code below the #$ hints command, you can specify several hints that can be shown to the student.  You can call here the function add.hint that allows to add a hint to the exercise.

The example generates a hint, that will give the user the solution to a similar exercise and that should be sufficient to solve the current exercise, even without any R knowledge.


```r
#$ hints ###################################################

add.hint("Hint 1",{cat('
# The following code stores 2+3 in a variable y
y = 2+3"
')})
```


I have not yet well worked out when and how hints shall be shown. One could think of the user asking for a hint, by calling a function hint(). Alternatively, one could offer the student a hint if he fails to often. Any ideas are very welcome.

#### #$ end exercise

This command simply indicates that the specification of the exercise is done. Afterwards, you can specify the structure of a new exercise, if you like.

## Specifying Tests

As said before, well specified tests that analyse the student's solution are crucial for a good problem set. Here I want to give an overview of some functions that can generate the test. A more detailed description of the test functions can be found in the R-help of the RTutor package. Consider the specification of exercise "b)" in the example file.


```r


############################################################### $ exercise
############################################################### b)

# $ task #######################################################

# Draw Simulate T outputs from a demand function q = beta0 + beta1 * p +
# eps


T = 1000  # Number of markets
beta0 = 100
beta1 = -1

# i) Draw T demand shocks eps from a normal distribution with mean 0 and
# variance 4

# ii) Generate a vector of endogenous prices

# iii) Compute realized demand q from demand function q=beta0+beta1*p+eps


# $ solution ###################################################
T = 1000  # Number of markets
beta0 = 100
beta1 = -1
# Draw demand shocks eps from a normal distribution with mean 0 and
# variance 4
eps = rnorm(T, 0, 2)
# One solution to generate endogenous prices
p = eps/2
# Compute realized demand q
q = beta0 + beta1 * p + eps
# $ tests ######################################################

# Check given variables
check.var(c("beta0", "beta1", "T"), exists = TRUE, length = TRUE, class = TRUE, 
    values = TRUE)

# Check construction of eps
check.var(c("eps"), exists = TRUE, length = TRUE, class = TRUE)

test.normality(eps, alpha.failure = 0.001, alpha.warning = 0.05)
test.mean(eps, 0)
test.variance(eps, 4)

# Check p
check.var(c("p"), exists = TRUE, length = TRUE, class = TRUE)

test.H0.rejected(cor.test(p, eps), check.warning = FALSE, alpha.failure = 0.05, 
    failure.message = " I couldn't significantly reject the hypothesis that your prices are exogenous (H0: cor(p,eps)=0, p.value = {{p_value}}). Try harder to generate prices that are 'significantly' endogenous.")

# Check q
check.var(c("q"), exists = TRUE, length = TRUE, class = TRUE)

holds.true({
    q.shall = beta0 + beta1 * p + eps
    q == q.shall
}, failure.message = "Sorry, you have not specified the correct demand equation q = ... (or perhaps you have changed q somewhere later in the code)")

# $ hints ######################################################

# $ end_exercise

```


Tests are performed in the following order:

1. Test that existing variables beta0, beta1, T are not changed

2. Test that eps are created correctly from the specified normal distribution

3. Test that prices p are correct and indeed endogenous

4. Test that the output q is correct given the student's eps and p

Let me explain the different used tests:

### check.var

```r
check.var(c("beta0", "beta1", "T"), exists = TRUE, length = TRUE, class = TRUE, 
    values = TRUE)
check.var(c("eps"), exists = TRUE, length = TRUE, class = TRUE)
```

The command check.var compares the users variables with the corresponding variables in the specified solution. We can check different aspects, by setting that flag TRUE

  * exists does the variable exist at all
  * length has the variable the same length as in the solution
  * class has the variable the same class as in the solution (e.g. numeric, character,data.frame,...)
  * values has the variable the same values as in the solution
  
Since eps shall be a vector of random variables, it does not make sense to check whether the values are identical to the randomly selected values in the solution. So we only test for existence, correct length and class.

### test.normality, test.mean test.variance


```r
test.normality(eps, alpha.failure = 0.001, alpha.warning = 0.05)
test.mean(eps, 0)
test.variance(eps, 4)
```


These three tests are used to determine in a statistical fashion whether eps satisfies the null hypothesises:
  
  * normally distributed
  * has mean 0
  * has a variance of 4
  
The tests perform statistical tests on the given vector and return a failure if the null hypothesis can be rejected at a significance level below alpha.failure. If the p-value is above alpha.failure but above alpha.warning just a warning will be shown. If not provided manually, the default values are alpha.failure = 0.1% and alpha.warning = 5%. You can look in the help pages for RTutor to get more details on these functions.

### test.H0.rejected and test.H0
For the prices p we first check existence, lengt and class with check.var. Then we implement a manual test whether p is indeed endogenous with the function test.H0.rejected.


```r
check.var(c("p"), exists = TRUE, length = TRUE, class = TRUE)

test.H0.rejected(cor.test(p, eps), check.warning = FALSE, alpha.failure = 0.05, 
    failure.message = " I couldn't significantly reject the hypothesis that your prices are exogenous (H0: cor(p,eps)=0, p.value = {{p_value}}). Try harder to generate prices that are 'significantly' endogenous.")

```


The first argument to test.H0.rejected is an expression that calls a statistical test, that returns a list that has an element p.value. Here we test the correlation between prices and eps by calling cor.test(p,eps). This expression will be evaluated with the student's solution. The student's solution passes our test , if we can **reject** the null hypothesis of this test,cor(p,eps)=0, at a level of alpha.failure = 0.05. (Recall from your econometrics classes that an explanatory variable p is endogenous if it is correlated with the error term eps.)

We have a similar function test.H0 which would be passed if H0 can **not be rejected**.

#### failure.message
If the test fails, we can specify a manual failure.message that will be shown to the user. Our failure message contains the following whiskers: {{p_value}}. This part of the message will be replaced by the actual p-value of the test. Another whisker you can use is  "{{test_name}}" that will be replaced by the name of the test.

### holds.true
Finally, we check the simulated output q.

```r
check.var(c("q"), exists = TRUE, length = TRUE, class = TRUE)
holds.true({
    q.shall = beta0 + beta1 * p + eps
    q == q.shall
}, failure.message = "Sorry, you have not specified the correct demand equation q = ...")
```

holds.true is a quite general testing function that checks whether a boolean condition (or all elements of a boolean vector) hold true given the student's solution. The first argument in {} is some code that will be evaluted given the student's solution. The last line of that code must be a boolean condition (or a vector of boolean condition). The test fails if one element of the boolean vector is false. Again, we specify a manual failure.message.
