# Tutorial for Developing Interactive R Problem Sets with RTutor

**Date: 2015-01-23

**Author: Sebastian Kranz, Ulm University** 

RTutor is an R package that allows to develop interactive R exercises. Problem sets can be solved off-line or can hosted in the web via shiny server. Problem sets can be designed as a Markdown  .rmd file (to be solved directly in RStudio) or use a browser-based interface powered by RStudio's Shiny.

## Trying out some problem sets

Before developing your own problem sets, you may want to try out some existing problem sets. Check out those very nice problem sets by students of mine, which replicate the main insights of interesting economic articles:

### On the Optimal Taxation of Top Incomes
   - Hosted on shiny.apps.io:
   https://skranz.shinyapps.io/RTutorTopIncomeTaxation/
   
  - Github: https://github.com/skranz/RTutorTopIncomeTaxation

### An interesting case study of a bank run
  - Github: https://github.com/skranz/RTutorBankRuns

## Installing the newest version of RTutor

To install RTutor without any bigger contributed problem set, just run in R
```s
if (!require(devtools))
  install.packages("devtools")
source_gist("gist.github.com/skranz/fad6062e5462c9d0efe4")
install.rtutor(update.github=TRUE)
library(RTutor)
```

## Create your own problem sets

Take a look at the .md files in the vignette folder for documentation of how to create own problem sets.

## Suggestions & Feedback

If you have suggestions or find bugs, please don't hesitate to open an issue on this github page. RTutor is still in a preliminary version and feedback is very appreciated.