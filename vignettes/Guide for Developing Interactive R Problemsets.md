Tutorial for Developing Interactive R Problem Sets with RTutor
===============================================================

**Date: 2014-06-12**

**Author: Sebastian Kranz, Ulm University** 

RTutor is an R package that allows to develop interactive R exercises. The interactive exercises directly test a student's solution and provide hints if not everything is correct. Unlike web-based approaches, as e.g. https://www.datacamp.com/, RTutor is developed for off-line use directly with RStudio.  This document gives you an overview how to generate an interactive problem set.

## I Brief overview

## 1. Install the newest version of RTutor

You first need to install some packages. Simply run in R
```s
# Install packages from CRAN
install.packages("devtools")
install.packages("whisker")
install.packages("stringr")
install.packages("JSONIO")

# Install packages from github
library(devtools)
install_github(repo="restorepoint", username="skranz")
install_github(repo="stringtools", username="skranz")
install_github(repo="RTutor", username="skranz")
library(restorepoint)
library(RTutor)
```

## 2. Create an R Markdown solution file

You create an interactive problem set is to write a solution file in R markdown format. In addition to the exercise description and a sample solution, the solution can contain manually adapted test, hints, or other commands like giving awards for a correct solution. Where no manual hints or tests are specified, RTutor will automatically generate tests and hints.

Here is the code from the example solution file "Example_sol.Rmd" that you find in the problemsets directory of the RTutor library. We will explain its structure in detail in the chapter on solution files.

```
    # Problemset Example
    
    Adapt the working directory below and press Ctrl-Alt-R (run all chunks). This creates the problem set files and the sample solution from this solution file.

    ```{r "Create problem set files"}
    setwd("C:/folder_of_this_solution_file")
    ps.name = "Example"; sol.file = paste0(ps.name,"_sol.Rmd")
    libs = NULL # character vector of all packages you load in the problem set
    create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs, name.sol.chunks=TRUE)
    ```
    
    ## Exercise 1 -- Summary statistics
    
    a) We often want to compute some summary statistic of a vector.
    For example:
    ```{r}
    #< task
    x = 10:20
    # Computing the sum of x
    sum(x)
    #> task
    ```
    Now compute the mean of x.
    ```{r}
    mean(x)
    #< hint
    display("There already exist R functions for many things. To find them use Google, e.g. search for 'R compute mean'.")
    #>
    #<
    give.award("mean means mean","Well, in some occasions one can just guess the name of an R function. The function to compute the mean of a vector, or matrix is called 'mean'. Usually, it is much quicker to goggle than to guess function names, however.")
    #>
    ```
```
The problemset folder of the RTutor library contains some example solution files. 

### 3. Generate a structure file and empty problem set for students

Copy the solution file in some directory, and set is as working directory by changing in your solution file the line

```s
    setwd("C:/folder_of_this_solution_file")
```
Open the solution file in RStudio and press Ctrl-Alt-R to run the first code chunk. 

The call to
```s
  create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs, name.sol.chunks=TRUE)
```
generates the files
  - Example.rps: a binary representation of the problem set structure
  - Example_struc.r: a human-readable representation of the problem set structure
  - Example.Rmd: empty problem set in Rmarkdown format
  - Example_sample_solution.Rmd: A sample solution of the problem set in Rmarkdown format

### 4. Solving and checking the problem set

A student that wants to interactively solve the problem set must copy the .rps file (the problem set structure) and an empty problem set as R-markdown  .Rmd file to a folder (same folder for both files) and open the .Rmd problem set file with RStudio.


The empty problem set files have some commands in the beginning, which are used to initialize RTutor and tell you, how you can check your solution. You first have to do 2 things:

1. Enter your user name at the specified place
2. Specify the folder in which you have copied this file

You can then start solving the problem set. To check your solution, you just have to

1. Save your .rmd problem set file (Ctrl-S) 
2. and then run all chuncks of your problem set (Ctrl-Alt-R)

How does it work? The header of the problem set file loads the package RTutor and calls the function check.problem.set, which runs automatic tests on the typed solution. If a test passes there is a success message. If a test fails, one gets an error message specifying in which part of the problem set the solution is not yet correct. Often one can type hint() in the R console to get a hint of how to solve the task that is not yet correct.

A student can type type `stats()` to get some information of how much of the problem set he has already solved. 

Before testing the empty problem set, open "Example_sample_solution.Rmd" and check if running all chunks (Ctrl-Alt-R) works correctly.

### 5. Iterative Development

I guess the most natural way to develop a problem set is to iteratively perform the following 3 steps:

  1. Write and adapt your solution file and generate problem set files with Ctrl-Alt-R
  2. Test whether the sample solution runs without problems
  3. Try to solve the empty problem set yourself and check whether you should change tests, hints or exercise tasks in your solution file (Step 1). 

**Advice:** Note that when you change your solution file and create new empty problem sets, all problem set files will be overwritten. When you solve the empty problem set during your iterative development, this can be annoying since you must replicate your solution steps again. Therefore it can sometimes be useful to save your solved problem set file, e.g. "Example.Rmd", under a different file name, like "Example_test.Rmd". You then must also adapt the line
```s
ps.file = 'Example.Rmd' # this file
```
in the beginning of your problem set file to that new file name. Of course, once you generate new exercises in your solution file you want to use the newly generated problem set file.

### 6. Distributing your problem set

Distributing the problem set is simple: just give your students the structure file as .rps and the empty problem set as .rmd file and tell them to install RStudio and all packages as described above. They can then go on solving the problem set. You can also ask them to upload the solutions and possible the log files that track their solution process.

It is planned to implement more functionality that facilitates grading. The goal is to put all solutions in a directory and quickly generate tables that show the total percentage solved for all students.

##  II  Writing Solution Files

Let us dig a bit deeper into writing solution files. Solution files are .Rmd files that specify the exercise text a solution as R code. Within the R code they can also use special comments to customize tests or hints. A solution file consists of one or more exercises. An exercise starts with a line like

    ## Exercise 1 -- Summary statistics

You can pick any label for the exercise (it can include spaces), but the line must have the structure start with 

    ## Exercise some_exercise_name

Recommend format for the exercise name is an exercise number, optionally followed by `--` and a short description, as in the example above.

Let us start by just writing an exercise with solution, without yet adding some manual hints or tests. Here, we also neglect the code chunk before Exercise 1 that generates the problem set file.

```
    # Problemset Example
    
    ```{r "Create problem set files"}
    setwd("C:/folder_of_this_solution_file")
    ps.name = "Example"; sol.file = paste0(ps.name,"_sol.Rmd")
    libs = NULL # character vector of all packages you load in the problem set
    create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs, name.sol.chunks=TRUE)
    ```
    
    ## Exercise 1 -- Summary statistics
    
    a) We often want to compute some summary statistic of a vector.
    For example:
    ```{r}
    x = 10:20
    # Computing the sum of x
    sum(x)
    ```
    Now compute the mean of x.
    ```{r}
    mean(x)
    ```
```
The description of the exercise is written as standard text, the code of your solution is put in R code chunks. RTutor recognizes lines in the exercise description that start with a) or ii) etc. as a "part" of an exercise and will use this information in the automatically generated messages from tests and hints. 

You can already generate an empty problem set from this simple solution file by running Ctrl-Alt-R (see step 3 of part I). In the empty problem set file, each chunk of R code will be replaced by a chunk of the form
```
    ```{r }
    # enter your code here ...
    ```
```
which asks the student to enter his solution.

### Showing code to students as part of exercise description
Actually, in our exercise the first code chuck shall be an example that will also be shown to the student. We can make code being shown to students by wrapping it in a block starting with the line
```{r }
#< task
```
and ending with a line
```{r }
#> task
```


This means we can modify our code as follows:
```
    # Problemset Example 
    
    ... chunk to generate problem sets is ommited ...
    
    ## Exercise 1 -- Summary statistics
    
    a) We often want to compute some summary statistic of a vector.
    For example:
    ```{r}
    #< task
    x = 10:20
    # Computing the sum of x
    sum(x)
    #> task
    ```
    Now compute the mean of x.
    ```{r}
    mean(x)
    ```
```
Generate again problem set files and look at the empty problem set. You see that the code of the first chunk is now shown in the empty problem set.

The interactive problem set can already be used. It automatically tests the solution and if the test fails since `mean(x)` is not entered correctly, it gives the option to type `hint()` to get a hint.

By default also tests will be generated for the code that is already given as a task (because future code may rely on this code not being accidently changed by the user.) If you don't want to generate tests for the code given in the task you can do so by adding the flag `notest` in your line that starts the task block: putting it into the following kind of block:

```{r eval=FALSE}
#< task notest
  #...
#> task
```


### Manual hints

RTutor tries to generate automatic hints that look at the students solution and try to give some advice while not telling too much. In particular, for very simple tasks the hint may reveal too much. For example, the automatic hint already tells the student the name of the function. (Often it is much harder to find the correct function arguments, though.)

    hint()

    You must call the function 'mean' in part a).

In our example, we want instead a manual hint that tells the student, how the internet is a very powerful source of information to find R commands for specific tasks. We include such a manual hint in our solution file as follows:
```
    # Problemset Example 

    ... chunk to generate problem sets is ommited ...

    ## Exercise 1 -- Summary statistics
    
    a) We often want to compute some summary statistics of a vector.
    For example:
    ```{r}
    #< task
    x = 10:20
    # Computing the sum of x
    sum(x)
    #> task
    ```
    Now compute the mean of x.
    ```{r}
    mean(x)
    #< hint
      display("There already exist R functions for many things. To find them use Google, e.g. search for 'R compute mean'.")
    #>
    ```
```
This means we simply add after the command of the solution that will be tested, a block of the form
```{r eval=FALSE, tidy=FALSE}
#< hint

#>
```
Inside the block, you enter some code that will be shown if the student types hint() after the test for the command failed. Often this will simply be a message, but you could also do other things, e.g. showing a plot as hint. Note that the hint code will be evaluated in an environment that contains all variables the student has defined earlier. 

Sometimes you want to show the advice from the automatically generated hint but also want to add some additional advice. You can do this by putting code inside the following block:
```{r eval=FALSE, tidy=FALSE}
#< add to hint

#>
```

### Using variables from earlier exercises

By default variables that have been generated by the Student in earlier exercises are not known in the current exercise. Sometimes, we want to use variables from earlier exercises, however. The actual Example_sol.Rmd file contains a second exercise that uses the variable `x` from exercise 1:

```
    ## Exercise 2 -- Computing with vectors #####################################  
    ```{r}
    #< settings
    import.var = list("1"="x")
    #>
    ```
    a) Let y be a vector that contains the squared elements of x. Show y
    ```{r}
    y = x^2
    y
    ```
```
As you see, in order to use variables from earlier exercises, you must add at the beginning of an exercise a settings block that defines a variable `import.var`, like
```
    ```{r}
    #< settings
    import.var = list("1"="x")
    #>
    ```
```
The variable `import.var` is a list whose element names correspond to the short exercise names (part of the name before --) from which you want to import variables. The list elements are character vectors of the variable names you want to import from the corresponding exercise.

### Giving awards

Isn't it amazing when video game players play for hours and hours, doing sometimes quite repetitive and boring tasks, just in order to earn some virtual points or virtual trophies? Well it seems that many people can be motivated, at least to some degree, by such stuff. Since a main motivation for RTutor is to make learning R, as well as statistical and economic concepts, more interesting and more entertaining, it seems natural to borrow some ideas from computer games.

So far there is only a small thing: students can get **awards** if they have solved a problem. The received awards can be shown by typing `awards()` in the R console. Information on the awards is stored in a file with name user_username.ruser in your problem set folder. Here is an example, how you can add an award to your solution file:


```{r eval=FALSE, tidy=FALSE}
#<
give.award("mean means mean","Well, in some occasions one can just guess the name of an R function. The function to compute the mean of a vector, or matrix is called 'mean'. Usually, it is much quicker to goggle than to guess function names, however.")
#>
```
Inside a #< #> block, you need to call give.award and have to specify a name of the award and can add some description text that may also be a bit informative. The award is granted once all tests for the solution above the award are passed.

There may be more elements from computer games that may motivate some students (experience points, high scores, levels, duells, an underlying story) and perhaps some of them will be included at some point of time. Yet, for the moment it's just awards.


### Manual tests

I tried my best to automatically test whether the student entered a correct solution or not. Yet sometimes we need to manually include specifice variants of tests in the solution file. Here is an example for such a manual test:

```{r eval=FALSE, tidy=FALSE}
#' b) Save in the variable u a vector of 4 different numbers
u = c(3,6,7,99)  
#< test
check.var("u",c(3,6,7,99),exists=TRUE, length=TRUE, class=TRUE)
#>
```

The automatic test would check whether one of the following two conditions is meet:

  - u has the same value than in the solution. This means u=c(2,5,6,98)+1 would also pass as correct solution
  - u is generated by an equivalent call as in the solution (equivalent means the function name should be the same and the arguments should have the same value). This is useful if the solution is a call that generates a random variable like x = runif(1).
  
Yet in this example, the automatic test is too restrictive. The student shall just generate some arbitrary vector consisting of 4 numbers. The block

```{r eval=FALSE, tidy=FALSE}
#< test
check.var("u",c(3,6,7,99),exists=TRUE, length=TRUE, class=TRUE)
#>
```

replaces the automatic test with a test that just checks whether a variable u exists, and has the same length and class (numeric or integer) as an example solution c(3,6,7,99).

RTutor has a series of helper functions for such manual tests. Take a look at the package help for a documentation.

### Specifying parameters of default tests

By default test are generated that call either `check.call` (a statement that does not assign a varible), `check.assign` (if a value is assigned to a variable), or `check.function` (if a function is generated). You can take a look at the generated _struc.r file to see which default and manual tests are generated. All test function have a number of arguments, that allow to customize the tests. A block starting with the line  `#< test.arg` allows you to change the arguments of a default test. Consider the following example:
```{r}
plot(x=p,y=q,main="A plot", xlab="Prices")
#< test.arg
  ignore.arg = c("main","xlab"), allow.extra.arg=TRUE
#>
```
The `#< test.arg` block customizes the parameters `ignore.arg` and `allow.extra.arg` of the check.call function. The parameter `ignore.arg = c("main","xlab")` means that the student does not have to add these two arguments to the plot function or can use different values. The parameter `allow.extra.arg=TRUE` allows the student to specify additional arguments when calling plot, e.g. specifying a `ylab`. So essentially, it will now only be tested whether the x and y arguments of the plot are currect and any customization of the plot will still be considered a correct solution.

The next section provides more examples for using tests and hints

## 4. Examples for tests and hints

### Testing a function written by the student

Sometimes you may ask to write a function in the problem set. Here is an example, how you could construct a solution file. In the exercise the student is asked to complete a manual function `ols` that shall compute an ols estimate:

```{r }
#< task notest
ols = function(y,X) {
  
  # enter code to compute beta.hat here ...
  
  return(as.numeric(beta.hat))
}
#> task

ols <- function(y,X) {
  beta.hat = solve(t(X) %*% X) %*% t(X) %*% y
  return(as.numeric(beta.hat))
}
#< test.arg
  ols(c(100,50,20,60),cbind(1,c(20,30,15,20))), hint.name="ols"
#>
#< hint ols
display("Just insert inside the function ols the code to compute beta.hat from y and X. You have developed this code in Exercise 1.")
#>
```
First, we have a `#< task notest` block that specifies an unfinished function that will be given to the student. Afterward, we have an example of a correct function `ols`.
Then the `#< test.arg` block specifies parameters for the automatic test `check.function`. The unnamed parameter 
```{r}
ols(c(100,50,20,60),cbind(1,c(20,30,15,20)))
```
Specifies a test call. `check.function` will run this test call for the official solution and the student's solution. The test will only pass if both versions return the same value.
The parameter hint.name specifies a manual name for a hint, since currently no automatic hints are generated for function creation (hopefully that will improve in later versions of RTutor.) Finally, the `#< hint ols` specifies what the hint `ols` shows.

### Testing a function that generates random variables

In the moment there is relatively little support to check user written functions that generate random variables, and I hope this will improve with newer versions of RTutor. What currently can be done to compare the results of the student's function and the official solution using the same random seed.

Here is an example:
```
    a) Write a function `runif.square` with parameters n, min and max that generates n random variables that are the square of variables that are uniform distibuted on the interval from min to max.
    
    ```{r}
    runif.square = function(n,min,max) {
      runif(n,min,max)^2
    }
    #< test.arg
      with.random.seed(runif.square(n=20,min=4,max=9), seed=12345)
    #>
    ```
```
Our test call is now embeded in the function with.random.seed that calls the function with a fixed random seed. The test check.function only passes if the students function and official solution return the same value when called with the same random seed.

If a function requires simulation of more than one random number, this testing procedure only works if the student draws the random numbers in the same order than the official solution. This means your task should specify already a lot of structure for the function and tell the student not to draw any additional random variables inside the function.

### Test if variables that satisfy specific conditions are generated

Consider the following example:
```
    a) Specify a number of observations T>=5
    
    ```{r }
    T = 100
    #< test
      check.var("T",100,exists=TRUE, length=TRUE,class=TRUE)
    #>
    #<
      holds.true(T>=5, failure.message="You must set T>=5.")
    #>
    ```
```
The default test `check.assign` would only pass if the student would set `T=100`, but any T>=5 shall be considered correct. So we first replace the default test by a `check.var` test that checks whether a numeric variable T of length 1 exists. Then we add another manual test `holds.true` that checks whether T>=5 holds. Note that using the holds.true test without first checking that a numeric, variable T of length 1 exists can cause the holds.true test to crash.

TO DO:

Add more examples