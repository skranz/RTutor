---
output:
  html_document:
    theme: readable
    toc: yes
---

Design Tipps for Interactive Problemsets based on an article
===============================================================

**Author: Sebastian Kranz, Ulm University** 

This is a list of suggestions when you create as part of your Bachelor or Master thesis an interactive problem set that reproduce the analysis of a published economic article. Also take a look at the general RTutor Guide for making problem sets.

## Main Philosophy

```
Send the user on an interesting, data-driven journey into the economic contents of the article
```
The problem set shall resemble an interactive article. It shall *not* be a tough problem set from a course that is designed to make students learn by figuring out much stuff for themselves. The user shall enter some lines of code, but she shall not forced to work too long on solving it. So don't worry that the problem set is too simple or too little work for the user. 

## Target user

Design most parts of your problem set with the following typical user in mind:

  1. The user is somebody who finds your problem set on the internet, is interested in the topic and wants to try it out.
  2. The user knows already a little bit of R, but does not necessarily know how to use packages like `dplyr`.
  3. The user is primarily interested in the economic analysis of the article.
  4. Whenever the user has computed a summary statistic, has run a regression or has shown a plot, she wants to read a short economic interpretation of the results. Try to always add text with such interpretations: mostly a good place is directly below the code chunk where the user shall perform the computation.
  5. While the user is also interested in learning some R tricks and seeing and adapting examples of well written R code, she does not like spending time searching through the internet or help files in order to find R commands or their precise syntax. She rather wants you to first  give an example, that she can adapt. She also strongly prefers short, elegant R code & tricks from existing packages. She does not like unelegant, complicated code.
  6. While the user likes solving and reading a well designed problem set, she also can quickly become bored. If solving the task becomes too complicated or she must do too much repeated work, she will just stop solving the problem set. E.g. if many similar computations have to be performed, just let the user solve one or two and provide the remaining computations in the task. 
  7. If the user has good knowledge of R and economics, she should be able to solve the problem set quite quickly by adapting the examples you provide in the task description.

## Tips for designing yor problem set

### Start quickly with an interesting data set.

Give only a brief background of the content and try to start quickly with loading an interesting data set. Let the user generate summary statistics and figures of interesting aspects. 

### Tell the story with the data

Typically, show first the data and then based on the data, develop step by step the story, hypothesis and results of the paper. Nevertheless, you still should give a very short summary, the length of a an abstract, at the beginning of the problem set.

### Pick good variable names and good variable descriptions

An important part of your initial work will be the generation of a variable description text file. It has the following format like in the following example (Header & 2 entries):

```
orgvar | var | descr

minority_dummy | minority | Dummy variable 1 if the account holder belongs to the minority community

ins_adj | above_insured | Dummy variable; 1 if the account holders total balances are above the deposit insurance coverage limit of 100,000 Rs.
```

The column `orgvar` is the name of the variable as given in the original data. The column `var` can be an alternative name that is easier to read. Separate words with `_`. Finally, the column `descr` provides a description of the variable, typically one or two sentences.

When calling `create.ps`, provide the file name of this file as argument `var.txt.file`. The variable description will then be shown in the data explorer of the HTML problem sets.

You can translate the original variable names to your new variables in a code chunk of your problem set by using the function `translate.var.names` which is part of RTutor. Example:
```
  # Read original Stata data
  term = read.dta("term_deposit_data_file1.dta")
  # Automatically translate the variable names
  term = translate.var.names(term)
``` 

### Design the problem set for HTML

The new RTutor allows two sort of problem sets: HTML-Shiny-based or Markdown-RStudio-based. Even though for longer courses the Markdown-RStudio-based approach has some advantages, with our target user in mind, you should definitely design the problem set for the HTML-Shiny-based format. 


### Use info blocks for background information and variable descriptions

When designing problem sets, you often face the following conflict. On the one hand, you may want to give detailed background information. On the other hand, you want your tasks sufficiently short, as you don't want to force the reader to read a lot before she can start analysing the data. To resolve this conflict, you can put a lot of information into info blocks. The user can view info blocks whenever she likes. You should generally put a description of the relevant variables of the current data set in info blocks.

### Create short code chunks and frequently add explanations and discussions afterwards

Try to avoid too many commands in one code chunk, but rather create more chunks. It is totally ok if the user only has to enter one or two commands in each code chunk. Whenever the user performed some analysis, provide a short explanation or discussion afterwards that helps the user to interpret the results.

### Interpret significant coefficients and effect sizes

After showing regression resulst, try to verbally interpret some economically interesting significant coefficients. What do they mean (one unit increase of x changes y by ??? units)? Do they seem economically substantial or rather not so? How big is the uncertainty mirrored in the standard errors? You can also ask the user for the quantitative interpretation via a quizz (see below).

Also consider using the function `effectplot` in my package `regtools`. It helps to compare the effects that changes in different explanatory variables have on the dependent variable in a standardized fashion.

If coefficients are not significant, you may be more careful in interpreting them. Many researchers have the opinion that you should not interpret insignificant coefficients quantitatively.

### Only load the data you currently need

Don't load and prepare all data sets at once, but try to start small and only load the data set you need for the current steps of the analysis.

### Start with a descriptive or graphical analysis before you prepare the data

It is ok if at some later point you want to teach the user how to modify and prepare data for the analysis (such skills are definitely useful since much time is usually spent with data preparation). Yet, try to start the problem set with loading data and showing some description or plot. Relegate data preparation to later parts. This means you may provide the user with a data set that you have prepared already yourself. In a later exercise you can explain how you have prepared that data.

### Don't let the user repeat too often boring tasks

While you can ask the user some time to prepare or modify data, don't let him do all data preparation. Let him learn interesting and useful things, but don't let him do boring work.

### Write elegant R code and use the recommended packages

There are many ways to perform some analysis in R, but one goal of the problem sets is to teach rather elegant solutions. "Elegant" typically means just a few lines of code that can be easily read. Of course, it is not so easy to write elegant code when you are yourself not yet an R expert. One thing you definitely should do is to read the section *"Recommended Packages"* and use those packages for typical tasks like data manipulation, plotting or showing regression outputs in a nice format.

### include auxialliary functions in an `extra.code.file`

You may want to write some own functions that simplify some steps of the analysis and make the user code shorter. You can put them into an extra .r file whose file name you pass as argument `extra.code.file` to `create.ps`. Your functions can then be called everywhere in the problem set. 

### Use quizzes
You can also ask the user a question about the data or regression results in form of a quiz. Here is an example, how you can implement a quiz chunk:

```
    c) Look at the different plots to answer the following questions (replace '???' with the right answer):
    ```{r "2.1 c)"}
    #< task_notest
    # Do you see a particular effect in open-end fund starts and net flows during Regime 2 compared to the other periods??
    # sol31 = "???" Assign "yes" or "no" to sol31 and remove the comment
    #>
    sol31 = "no"
    ```
```

### Make life easy for the user: give code examples

If the user shall write some code that is a bit more complicated, try to give an example in a task plot first that he can adapt. Remember that the target user does not want to figure out complicated commands on his own but wants to see quick results.

### Make life easy for the user: give part of the code

If there is a complicated command or task, you can also give part of the code to the user in the problem set and make him adapt the code. Here is an example, how you can construct that in your solution file:

```
    ```{r "2.2.1 c)"}
    #< task_notest
    # Replace the ??? in the code below and uncomment the command.
    # plot(???, ??? , ylim = c(0,6.5))
    #>
    plot(x = open$nfo_open_date, y = open$entry_load_nfo, ylim = c(0,6.5)) 
    ```
```
Of course, once the user has seen the code. You could ask him to write the full plot command next time.

### Use compute blocks if you ask for computations in several steps
### Not yet implemented

Maybe at some point you want to ask a more complex computation from the user instead of just a single line of code. In this case consider using a compute block that is designed to give sensible hints for students.


### If you have a hard task put it in an own exercise and / or be generous with your hints

Even if overal the goal is that the problem set is not too hard to solve. You may want to add one or two more tricky exercises. Then make sure that you put them in an extra exercise, which can be solved optionally. Otherwise be very generous with the hint, e.g. provide the solution in your hint.

### Except you have a very good reason, don't let the user write unelegant, complicated code if you know a nicer way.

Sometimes you think it could be a good idea that the user first uses a complicated way to solve problem and you only later tell how she can solve it more elegantly. Typically this not such a good idea and it is better to skip the unelegant solution and immediatly start with the elegant solution.


## Recommended packages and functions

This sections gives recommendations for packages and functions you should use for common tasks in your problem sets. 


### Regressions

#### present regression results: showreg (in regtools)

The package `regtools` contins the function `showreg` to show regression results. It shows results from one or several regression objects in a typical format used in journal articles. The function is basically a wrapper to functions in the package `texreg`. I think that the output of the usual `summary` function typically does not look as nice in an interactive problem set. 

#### basic regression functions

The base R functions `lm` and `glm` allow linear regressions and generalized linear models, including tobit and probit regressions. But there are many specialized functions and packages for other sorts of regressions, e.g.

  - `plm`: linear panel data models
  - `mlogit`: discrete choice data
  
For other models search the internet.

### Robust standard errors: showreg (or sandwich)

Dealing with robust standard errors in R is traditionally a bit more complicated than in Stata, but can be done. Much functionality is included in  the package `sandwich`. For simplicity, the function `showreg` contains the arguments `robust` and `robust.type` that allow showing robust standard errors in a simple manner.

Support for clustered standard errors is implemented in showreg but not yet tested. Please compare your results with Stata.

### Marginal effects for probit and logit models:


To show marginal effects for probit and logit models use the argument `coef.transform="mfx"` in `showreg`. It based on functions from the package `mfx` (not yet fully tested, though)

### Effect sizes: effectplot (in regtools)

Since explanatory variables are usually measured in different units, it is often not easy to compare the sizes of the effects that 'typical' changes of different explanatory variables have on the dependent variable in a regression model. The function `effectplot` shall help for such comparison. The key idea is to compare the effects the explanatory variables change from e.g. their 10% quantile to their 90% quantile. For non-linear specifications also the package `effects` is quite helpful, to see how effects change over the range of values of an explanatory variable.

### Data preparation: dplyr, tidyr and dplyrExtras

The package `dplyr` and `tidyr` provide a very well thought through framework for many common data manipulation tasks and the functions run fast also for big data sets. Try to use them. Unfortunatley, `dplyr` is still very young and I was missing some functionality. Therefore I wrote the package `dplyrExtras` provides that can be installed from github:
```s
library(devtools);
install_github(repo="dplyrExtras", username="skranz")
```

### Summarising and aggregating data: dplyr (group_by and summarise)

For summarising data, the `summarise` function in `dplyr` is very useful. Combine it with the `group_by` function to perform summaries by groups. Also the `summarise_each` function, as well as the `xsummarise_each` function in `dplyrExtras`, can be helpful. 


### Graphics: ggplot2 and more

A very nice and powerful graphics package is `gpplot2`. Even though the syntax may look a bit complicated at the beginning, I recommend to use it for most of your figures.  

The ggplot2 package is nicle complemented by the `ggthemes` package that contains several additional themes for the appearance of your plots. I particularly like `theme_wsj()`, which renders your graphics in a 'Wall Street Journal' theme. There is also a Stata theme: `theme_stata()`, which may make your graphs look more similar to those in the original article.

For simple plots you may also sometimes use the standard R functions like `plot` or `hist`. For interactive plots take a look at the packages `googleVis`. In particular the motion plots in `googleVis` can be quite nice. To use it, set in the chunk header the option `results="asis"` to make the output be displayed as HTML. I have not yet figured out how the promising `ggvis` package can be best integrated with RTutor.


