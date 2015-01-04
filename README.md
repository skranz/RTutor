# Tutorial for Developing Interactive R Problem Sets with RTutor

**Date: 2014-12-01

**Author: Sebastian Kranz, Ulm University** 

RTutor is an R package that allows to develop interactive R exercises. The interactive exercises directly test a student's solution and provide hints if not everything is correct. Unlike web-based approaches, as e.g. https://www.datacamp.com/, RTutor is developed for off-line use. Problem sets can either be solved as Markdown  .rmd file directly in RStudio or in a browser based interface powered by Shiny.  This document gives you an overview how to generate an interactive problem set.

## Installing the newest version of RTutor

To install RTutor with required packages run in R
```s
if (!require(devtools))
  install.packages("devtools")
source_gist("gist.github.com/skranz/fad6062e5462c9d0efe4")
install.rtutor(update.github=TRUE)
library(RTutor)
```

## Trying out some problem sets

Before developing your own problem sets, you may want to try out some existing problem sets. Check out those very nice problem sets by students of mine, which replicate the main insights of interesting economic articles:

### On the Optimal Taxation of Top Incomes
https://github.com/skranz/RTutorTopIncomeTaxation

### An interesting case study of a bank run
https://github.com/skranz/RTutorBankRuns

## Create your own problem sets

Take a look at the .md files in the vignette folder for documentation of how to create own problem sets.

## Feedback suggestions

If you have suggestions or find bugs, please don't hesitate to open an issue on this github page. RTutor is still in a preliminary version and feedback is very appreciated.