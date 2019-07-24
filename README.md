# RTutor: Interactive R Problem Sets

**Author: Sebastian Kranz, Ulm University** 

RTutor is an R package that allows to develop interactive R exercises. Problem sets can be solved off-line or can be hosted in the web with shinyapps.io. Problem sets can be designed as a Markdown  .rmd file (to be solved directly in RStudio) or use a browser-based interface powered by RStudio's Shiny. While the web interface looks nicer, I personally use problem sets in the Markdown format when teaching advanced economic classes.

## Trying out some problem sets

You can try out the Rmarkdown version of RTutor via RStudio Cloud:

[https://rstudio.cloud/project/39040](https://rstudio.cloud/project/39040)

For the web-based interface, several students at Ulm University have created very nice problem sets that allow to interactively replicate the main insights of interesting economic articles and to learn a bit about R and econometrics. Before developing your own problem sets, you may want to try out some of these examples:

### The Effect of Water Pollution on Cancer (by Brigitte Peter)
  - Github: https://github.com/brigittepeter/RTutorWaterPollutionChina
  - shinyapps.io: https://brigittepeter.shinyapps.io/RTutorWaterPollutionChina/

### Comparing the Environmental Damages of Driving Electric or Gasoline Cars (by Felix Stickel)
  - Github: https://github.com/felsti/RTutorECars
  - shinyapps.io: https://felsti.shinyapps.io/RTutorECars
  - rstudio.cloud: https://rstudio.cloud/project/139129

### Public Procurement Auctions: Design, Outcomes and Adaption Costs (by Frederik Collin)
  - Github: https://github.com/Fcolli/RTutorProcurementAuction
  - shinyapps.io: https://fcolli.shinyapps.io/RTutorProcurementAuction

### Poverty Reduction and Deforestation (by Katharina Kaufmann)
  - Github: https://github.com/KathKaufmann/RTutorEcologicalFootprintOfPovertyAlleviation
  - shinyapps.io: https://kathkaufmann.shinyapps.io/RTutorEcologicalFootprintOfPovertyAlleviation/

### How soap operas reduced fertility in Brazil (by Clara Ulmer)
  - Github: https://github.com/ClaraUlmer/RTutorSoapOperas
  - shinyapps.io: https://claraulmer.shinyapps.io/RTutorSoapOperas

### Excessive Traffic Jams? Improving Incentive Contracts for Road Construction Projects (by Claudius Schmid)
  - Github: https://github.com/ClaMaSch/RTutorIncentiveContracts
  - shinyapps.io: https://clamasch.shinyapps.io/RTutorIncentiveContracts
  - rstudio cloud: https://rstudio.cloud/project/137023

### Assessing Free Trade Agreements (by Tobias Fischer)
  - Github: https://github.com/fischeruu/RTutorNAFTAfreetrade
  - shinyapps.io: https://fischeruu.shinyapps.io/RTutorNAFTAfreetrade/

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

# Installation

RTutor and some required packages are not hosted on CRAN (while CRAN is great it takes a lot of time to maintain several packages there). I have created an own Github based R repository, from which you can install RTutor by using the following code:

```r
install.packages("RTutor",repos = c("https://skranz-repo.github.io/drat/",getOption("repos")))
```
**Note: If you want to create your own web-based RTutor problem sets and upload them on shinyapps.io, you need to install RTutor and required packages directly from Github and CRAN as explained below. That is because shinyapps.io only works with R packages directly installed from Github or CRAN.**

## Installing RTutor directly from Github

To install RTutor and required packages directly from Github and CRAN, you can use the small function in the following gist:

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

If you have suggestions or find bugs, please don't hesitate to open an [issue on the Github page](https://github.com/skranz/RTutor/issues).