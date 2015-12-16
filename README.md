# Tutorial for Developing Interactive R Problem Sets with RTutor

**Date: 2015-01-23

**Author: Sebastian Kranz, Ulm University** 

RTutor is an R package that allows to develop interactive R exercises. Problem sets can be solved off-line or can be hosted in the web with shinyapps.io. Problem sets can be designed as a Markdown  .rmd file (to be solved directly in RStudio) or use a browser-based interface powered by RStudio's Shiny. While the web interface looks nicer, I personally use problem sets in the Markdown format when teaching advanced economic classes.

## Trying out some problem sets

For the web-based interface, several students at Ulm University have created very nice problem sets that allow to interactively replicate the main insights of interesting economic articles and to learn a bit about R and econometrics. Before developing your own problem sets, you may want to try out some of these examples:

### Public Procurement Auctions: Design, Outcomes and Adaption Costs (by Frederik Collin)
  - Github: https://github.com/Fcolli/RTutorProcurementAuction
  - shinyapps.io: https://fcolli.shinyapps.io/RTutorProcurementAuction

### How soap operas reduced fertility in Brazil (by Clara Ulmer)
  - Github: https://github.com/ClaraUlmer/RTutorSoapOperas
  - shinyapps.io: https://claraulmer.shinyapps.io/RTutorSoapOperas

### On the optimal taxation of top incomes (by Jonas Send)
  - Github: https://github.com/JonasSend/RTutorTopIncomeTaxation
  - shinyapps.io: https://jonassend.shinyapps.io/RTutorTopIncomeTaxation/

### A macroeconomic study of credit booms and busts (by Thomas Clausing)
  - Github: https://github.com/tcl89/creditboomsgonebust
  - shinyapps.io: https://tcl89.shinyapps.io/creditboomsgonebust 

### An interesting case study of a bank run (by Joachim Plath)
  - Github: https://github.com/skranz/RTutorBankRuns
  

## Installing the newest version of RTutor

To install RTutor with all required additional packages, just run in R
```s
if (!require(devtools)) 
  install.packages("devtools")

devtools::source_gist("gist.github.com/skranz/fad6062e5462c9d0efe4")
install.rtutor(update.github=TRUE)
library(RTutor)
```

If you only want to update the RTutor package (and have the other packages already installed). You can just type:

```s
devtools::install_github("skranz/RTutor")
```
(You may have to restart your R session / RStudio for the update to work.)

## Create your own problem sets

Take a look at the files in the vignette folder for documentation of how to create own problem sets.

## Suggestions & Feedback

If you have suggestions or find bugs, please don't hesitate to open an issue on this github page. RTutor is still in a preliminary version and feedback is very appreciated.