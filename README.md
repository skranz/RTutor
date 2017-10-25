# Tutorial for Developing Interactive R Problem Sets with RTutor

**Date: 2015-01-23

**Author: Sebastian Kranz, Ulm University** 

RTutor is an R package that allows to develop interactive R exercises. Problem sets can be solved off-line or can be hosted in the web with shinyapps.io. Problem sets can be designed as a Markdown  .rmd file (to be solved directly in RStudio) or use a browser-based interface powered by RStudio's Shiny. While the web interface looks nicer, I personally use problem sets in the Markdown format when teaching advanced economic classes.

## Trying out some problem sets

For the web-based interface, several students at Ulm University have created very nice problem sets that allow to interactively replicate the main insights of interesting economic articles and to learn a bit about R and econometrics. Before developing your own problem sets, you may want to try out some of these examples:

### Public Procurement Auctions: Design, Outcomes and Adaption Costs (by Frederik Collin)
  - Github: https://github.com/Fcolli/RTutorProcurementAuction
  - shinyapps.io: https://fcolli.shinyapps.io/RTutorProcurementAuction

### Poverty Reduction and Deforestation (by Katharina Kaufmann)
  - Github: https://github.com/KathKaufmann/RTutorEcologicalFootprintOfPovertyAlleviation
  - shinyapps.io: https://kathkaufmann.shinyapps.io/RTutorEcologicalFootprintOfPovertyAlleviation/

### The Effect of Water Pollution on Cancer (by Brigitte Peter)
  - Github: https://github.com/brigittepeter/RTutorWaterPollutionChina
  - shinyapps.io: https://brigittepeter.shinyapps.io/RTutorWaterPollutionChina/

### Assessing Free Trade Agreements (by Tobias Fischer)
  - Github: https://github.com/fischeruu/RTutorNAFTAfreetrade
  - shinyapps.io: https://fischeruu.shinyapps.io/RTutorNAFTAfreetrade/

### How soap operas reduced fertility in Brazil (by Clara Ulmer)
  - Github: https://github.com/ClaraUlmer/RTutorSoapOperas
  - shinyapps.io: https://claraulmer.shinyapps.io/RTutorSoapOperas

### CO2 Trading and Risk of Firm Relocation (by Benjamin Lux)
  - Github: https://github.com/b-lux/RTutorCarbonLeakage
  - shinyapps.io: https://b-lux.shinyapps.io/RTutorCarbonLeakage/

### On the optimal taxation of top incomes (by Jonas Send)
  - Github: https://github.com/JonasSend/RTutorTopIncomeTaxation
  - shinyapps.io: https://jonassend.shinyapps.io/RTutorTopIncomeTaxation/

### The effect of the TseTse fly on African Development (by Vanessa Schöller)
  - Github: https://github.com/vanessaschoeller/RTutorTseTse
  - shinyapps.io: https://vanessaschoeller.shinyapps.io/RTutorTseTse/

### Pollution Reduction by Wind Energy (by Anna Sophie Barann)
  - Github: https://github.com/asbara/RTutorPollutionReductions
  - shinyapps.io: https://asbara.shinyapps.io/RTutorPollutionReductions/

### Water Pollution and Digestive Cancer (by Brigitte Peter)
  - Github: https://github.com/brigittepeter/RTutorWaterPollutionChina
  - shinyapps.io: https://brigittepeter.shinyapps.io/RTutorWaterPollutionChina/

### Wall Street and the Housing Bubble (by Marius Wentz)
  - Github: https://github.com/mwentz93/RTutorWallStreet
  - shinyapps.io: https://mwentz93.shinyapps.io/RTutorWallStreet/
  
### Air pollution and house prices (by Moritz Sporer)
  - Github: [https://github.com/msporer/RTutorEnvironmentalRegulation](https://github.com/msporer/RTutorEnvironmentalRegulation)
  - shinyapps.io: [https://msporer.shinyapps.io/RTutorEnvironmentalRegulations/](https://msporer.shinyapps.io/RTutorEnvironmentalRegulations/)

### A macroeconomic study of credit booms and busts (by Thomas Clausing)
  - Github: https://github.com/tcl89/creditboomsgonebust
  - shinyapps.io: https://tcl89.shinyapps.io/creditboomsgonebust 

### The impact of emmission trading on green innovation (by Arthur Schäfer)
  - Github: https://github.com/ArthurS90/RTutorEmissionTrading
  - shinyapps.io: https://arthurs90.shinyapps.io/RTutorEmissionTrading/

### Building Codes and Energy Efficiency (2 versions, by Simon Hertle and Lisa Eilts)
  - Github (Simon Hertle): https://github.com/simonhertle/RTutorBuildingCodes
  - shinyapps.io (Simon Hertle): https://shertle.shinyapps.io/RTutorBuildingCodes/
  - Github (Lisa Eilts): https://github.com/LEilts/RTutorBuildingCodes

### Technological Progress and Fuel Economy of Cars (by Marius Breitmayer)
  - Github: https://github.com/MariusBreitmayer/RTutorAttributeTradeOffs
  - shinyapps.io: https://mariusbreitmayer.shinyapps.io/RTutorAttributeTradeOffs

### How can Scandinavians tax so much? (by David Hertle)
  - Github: https://github.com/dhertle/RTutorTaxationScandinavia
  
### An interesting case study of a bank run (by Joachim Plath)
  - Github: https://github.com/skranz/RTutorBankRuns

## Installing and Running RTutor with Docker 

If you already use [Docker](https://www.docker.com/), you can quickly use RTutor with the docker container `skranz/rtutor`. The container allows you to work with RTutor via RStudio server in your webbrowser. It already contains some example problem sets, but you can install other problem sets or create your own problem sets. Details are here:

https://hub.docker.com/r/skranz/rtutor/

Since the image contains R, shiny, rstudio and a lot of packages, it has quite some size, however.

## Installing the newest version of the RTutor package

Since RTutor uses a lot of packages from Github that are not on CRAN, I have written a small install function, that can be found in this gist:


[https://gist.github.com/skranz/fad6062e5462c9d0efe4](https://gist.github.com/skranz/fad6062e5462c9d0efe4)

Copy the code in the link into your R console and then run:
```r
install.rtutor(update.github=TRUE)
```

Depending on your devtools version, also the following code may work directly (yet source_gist is buggy in some devtools versions):
```s
if (!require(devtools)) 
  install.packages("devtools")

devtools::source_gist("gist.github.com/skranz/fad6062e5462c9d0efe4", filename="install_rtutor.r")
install.rtutor(update.github=TRUE)
library(RTutor)
```

If you only want to update the RTutor package (and have the other packages already installed). You can just type:

```s
devtools::install_github("skranz/RTutor", upgrade_dependencies=FALSE)
```
(You may have to restart your R session / RStudio for the update to work.)



## Create your own problem sets

Take a look at the files in the vignette folder for documentation of how to create own problem sets.

## Suggestions & Feedback

If you have suggestions or find bugs, please don't hesitate to open an issue on this github page. RTutor is still in a preliminary version and feedback is very appreciated.