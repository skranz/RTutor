Tutorial for Developing Interactive R Problem Sets with RTutor
===============================================================

**Date: 2014-04-16**

**Author: Sebastian Kranz, Ulm University** 

RTutor is an R package that allows to develop interactive R exercises. The interactive exercises directly test a student's solution and provide hints if not everything is correct. Unlike web-based approaches, as e.g. https://www.datacamp.com/, RTutor is developed for off-line use directly with RStudio.  This document gives you an overview how to generate an interactive problem set.

## I Brief overview

## 1. Install the newest version of RTutor

You first need to install some packages. Simply run in R

```r
# Install packages from CRAN
install.packages("devtools")
install.packages("whisker")
install.packages("stringr")
install.packages("JSONIO")

# Install packages from github
library(devtools)
install_github(repo = "restorepoint", username = "skranz")
install_github(repo = "stringtools", username = "skranz")
install_github(repo = "RTutor", username = "skranz")
library(restorepoint)
library(RTutor)
```


## 2. Create a solution file

The recommend way to create an interactive problem set is to write a solution file. It is an R file that contains as comments a description of the tasks as the student will see it, together with a solution to the problem set. In addition the solution can contain manual hints, tests or other commands like giving awards for a correct solution. Where no manual hints or tests are specified, RTutor will automatically generate tests and hints.

Here is a small solution file with only one exercise. We will explain its structure in detail in the chapter on solution files.


```r
#$ exercise 1 Summary statistics #####################################

#' a) We often want to compute some summary statistic of a vector.
#' For example:
#s
x = 10:20
# Computing the sum of x
sum(x)
#e
#' Now compute the mean of x.
mean(x)
#< hint
display("There already exist R functions for many things. To find them use Google, e.g. search for 'R compute mean'.")
#>
#<
give.award("mean means mean")
#>
```


The problemset folder of the RTutor library contains several example solution files. 

### 3. Generate a structure file and empty problem set for students

Assume your solution is in a file "example_ps_sol.r". The following code generates the necessary files for students to use the interactive problem set.


```r
  #setwd("C:/ ... folder with your solution file")
  create.struc("example_ps_sol.r",ps.name="example_ps")
  create.empty.ps("example_ps")
```


The create.struc command generates the files
  - example_ps.rps: a binary representation of the problem set structure
  - examples_ps_struc.r: a human-readable representation of the problem set structure

The create.empty.ps command then generates the files
  - example_ps.rmd: empty problem set in R-markdown format
  - example_ps.r: empty problem set in R format

### 4. Solving and checking the problem set as student

A student that wants to use the problem set needs to copy the .rps file (the problem set structure) and an empty problem set either as R-markdown  .rmd file **or** as .r file to the same folder and open the problem set file with RStudio. (The advantage of the .rmd file is that it looks much nicer. I think the main advantage of .r files is that in RStudio one can save and check the problem set with a single short cut, while you need to separately save and check .rmd files)

The empty problem set files have some commands in the beginning, which are used to initialize RTutor and tell you, how you can check your solution. You first have to do 2 things:

1. Enter your user name at the specified place
2. Specify the folder in which you have copied this file

You can then start solving the problem set. To check your solution, you just have to

1. Save your problem set file (Ctrl-S) 
2. and then
  - if your have an .r file: source your file (Ctrl-Shift-S) 
  - if you have an .rmd file: run all chuncks (Ctrl-Alt-R)

Hint: If you use a problem set as .r file, you can check the box "Source on Save" in RStudio. Then you only have to press Ctrl-S to automatically save and check your problem set.

How does it work? The header of the problem set file loads the package RTutor and calls the function check.problem.set, which runs automatic tests on the student's solution. If a test passes there is a success message. If a test fails, one gets an error message specifying in which part of the problem set the solution is not yet correct. Often one can type hint() in the R console to get a hint of how to solve the task that is not yet correct.

### 5. Iterate
You probably will iterate between writing a solution file, solving the problem set yourself and extending and improving the problem set until you have a nice version that you want to send out to the public.

Distributing the problem set is simple: just give your students the structure file as .rps and the empty problem set as .r or .rmd file and tell them to install RStudio and RTutor as described above. 

##  II  Writing Solution Files

Solution files are .r files that use comments for some special syntax in comments. A solution file consists of one or more exercises. An exercise starts with a line

```r
#$ exercise name_of_exercise #####################
```

You can pick any name of the exercise (it can include spaces), but the line must start with 

```r
#$ exercise
```

The trailing ####### are optional. They just help to navigate your solution file but will not be part of the name.

Let us start by just writing an exercise with solution, without yet adding some manual hints or tests. For example:


```r
#$ exercise 1 Summary Statistics #####################################

#' a) We often want to compute some summary statistic of a vector. For example:
x = 10:20
# Computing the sum of x
sum(x)
#' Now compute the mean of x.
mean(x)
```


Comments starting with #' will be transformed into non-code markdown text (as is default if you use the knitr to generate and .rmd or .html files from an .r file). Your description of the task should be put into such comments.

Note that lines starting with #' a) or (#' b) etc.) define a "part" of an exercise. Parts will be used in messages shown from tests and hints to help the student see where a test has failed.

You can already generate an empty problem set from this solution file (see step 3 of part I). Each chunk of code between #' lines will be removed and replaced by a single line prompting the user to enter his solution:

```r
# enter your code here ...
```


### Showing code to students as part of exercise description
Actually, in our exercise the first code chuck is an example that shall be shown to the student and only the last line of code is a solution that will not be shown. We can make code being shown to students by wrapping it in a block starting with the line

```r
# s
```

and ending with a line

```r
# e
```

This means we can modify our code as follows:

```r
#$ exercise 1 Summary Statistics #####################################

#' a) We often want to compute some summary statistic of a vector.
#' For example:
#s
x = 10:20
# Computing the sum of x
sum(x)
#e
#' Now compute the mean of x.
mean(x)
```


Generate again the problem set structure and empty problem set. You see that the first code chunk is now shown in the empty problem set. The interactive problem set can already be used. It automatically tests the solution and if the test fails since mean(x) is not entered correctly, it gives the option to type hint() to get a hint.

### Manual hints

RTutor tries to generate automatic hints that look at the students solution and try to give some advice while not telling too much. In particular, for very simple task the hint may reveal too much. For example, the automatic hint already tells the student the name of the function. (Often it is much harder to find the correct function arguments, though.)

    hint()

    You must call the function 'mean' in part a).

In our example, we want instead a manual hint that tells the student, how Google is a very powerful tool to find R commands for specific tasks. We include such a manual hint in our solution file as follows:

```r

#$ exercise 1 Summary statistics #####################################

#' a) We often want to compute some summary statistic of a vector.
#' For example:
#s
x = 10:20
# Computing the sum of x
sum(x)
#e
#' Now compute the mean of x.
mean(x)
#< hint
display("There already exist R functions for many things. To find them use Google, e.g. search for 'R compute mean'.")
#>
```


This means we simply add after the command of the solution that will be tested, a block of the form

```r
#< hint

#>
```

Inside the block, you enter some code that will be shown if the student types hint() after the test for the command failed. Often this will simply be a message, but you could also do other things, e.g. showing a plot, as hint. Note that the hint code will be evaluated in an environment that contains all variables the student has defined earlier. 

Sometimes you want to show the automatically generated hint but add some additional advice. You can do this by putting code inside the following block:

```r
#< add to hint

#>
```


### Giving awards

Isn't it amazing when video game players play for hours and hours, doing sometimes quite repetitive and boring tasks, just in order to earn some virtual points or virtual trophies? Well it seems that many people can be motivated, at least to some degree, by such stuff. Since a main motivation for RTutor is to make learning R, as well as statistical and economic concepts, more interesting and entertaining, it seems natural to borrow some ideas from computer games.

So far there is only a small thing: students can get **awards** if they have solved a problem. The received awards can be shown by typing awards() in the R console. Information on the awards is stored in a file with name user_username.ruser in your problem set folder. Here is an example, how you can add an award to your solution file:



```r
#<
give.award("mean means mean","Well, in some occasions one can just guess the name of an R function. The function to compute the mean of a vector, or matrix is called 'mean'. Usually, it is much quicker to goggle than to guess function names, however.")
#>
```

Inside a #< #> block, you need to call give.award and have to specify a name of the award and can add some description text that may also be a bit informative. The award is granted once all tests for the solution above the award are passed.

There may be more elements from computer games that may motivate some students (experience points, high scores, levels, duells, an underlying story) and perhaps some of them will be included at some point of time. Yet, for the moment it's just awards.


### Manual tests

I tried my best to automatically test whether the student entered a correct solution or not. Yet sometimes we need to manually include specifice variants of tests in the solution file. Here is an example for such a manual test:


```r
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


```r
#< test
check.var("u",c(3,6,7,99),exists=TRUE, length=TRUE, class=TRUE)
#>
```


replaces the automatic test with a test that just checks whether a variable u exists, and has the same length and class (numeric or integer) as an example solution c(3,6,7,99).

RTutor has a series of helper functions for such manual tests.

TO DO:
1. Document functions for manual tests
2. How to add hints for manual test functions (not yet implemented)
2. Talk about _struc.r files for a deeper understanding
3. Tips & Tricks for developing interactive problem sets 

