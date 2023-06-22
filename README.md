# RTutor: Interactive R Problem Sets

**Author: Sebastian Kranz, Ulm University** 

RTutor is an R package that allows to develop interactive R exercises. Problem sets can be solved off-line or can be hosted in the web with shinyapps.io. Problem sets can be designed as a Markdown  .rmd file (to be solved directly in RStudio) or use a browser-based interface powered by RStudio's Shiny. While the web interface looks nicer, I personally use problem sets in the Markdown format when teaching advanced economic classes.

# Installation

RTutor and some required packages are not hosted on CRAN (while CRAN is great it takes a lot of time to maintain several packages there). You can install it from my [r-universe repository](https://skranz.r-universe.dev/ui#builds) by using the following code:

```r
options(repos = c(skranz = 'https://skranz.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
install.packages("RTutor")
```


## Create your own problem sets

Take a look at the [manuals](https://skranz.github.io/RTutor/articles) for documentation of how to create own problem sets. Also look at the examples below.


## Trying out some problem sets

You can try out the Rmarkdown version of RTutor via RStudio Cloud:

[https://rstudio.cloud/project/39040](https://rstudio.cloud/project/39040)

For the web-based interface, several students at Ulm University have created very nice problem sets that allow to interactively replicate the main insights of interesting economic articles and to learn a bit about R and econometrics. Before developing your own problem sets, you may want to try out some of these examples:

### The Effect of Water Pollution on Cancer (by Brigitte Peter)
  - Github: https://github.com/brigittepeter/RTutorWaterPollutionChina
  - shinyapps.io: https://brigittepeter.shinyapps.io/RTutorWaterPollutionChina/

### Political Incentives and Water Pollution in China (by Simon Maier)
  - Github: https://github.com/SimonMaier1995/Chinas-Political-Promotion-Incentives
  - shinyapps.io: https://simonmaier.shinyapps.io/Chinas-Political-Promotion-Incentives/

### Economic Impact of Covid-19 (by Alexandra Aehle)
  - Github: https://github.com/alexaehle/RTutorEconomicImpactsofCOVID19
  - shinyapps.io: https://alexaehle.shinyapps.io/RTutorEconomicImpactsOfCOVID19

### What explains the employment drop in the great recession 2007-2009? (by Birgit Schroff)
  - Github: https://github.com/bschroff/RTutorDropInEmployment
  - shinyapps.io: https://bschroff.shinyapps.io/RTutorDropInEmployment

### Comparing the Environmental Damages of Driving Electric or Gasoline Cars (by Felix Stickel)
  - Github: https://github.com/felsti/RTutorECars
  - shinyapps.io: https://felsti.shinyapps.io/RTutorECars

### The causal effects of Sweden's CO2 Tax (by Theresa Graefe)

  - Github: [https://github.com/TheresaGraefe/RTutorCarbonTax](https://github.com/TheresaGraefe/RTutorCarbonTax)
  - shinyapps.io: [https://theresagraefe.shinyapps.io/RTutorCarbonTaxesAndCO2Emissions/](https://theresagraefe.shinyapps.io/RTutorCarbonTaxesAndCO2Emissions/)


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

### The impact of competition policy and industrial policy on economic development (by Julian Latzko)
  - Github: https://github.com/julianlatzko/RTutorCompetitionPolicy
  - shinyapps.io: https://julianlatzko.shinyapps.io/RTutorCompetitionPolicy

### Analyzing Mozart's letters: How emotions determine creativity (by Daniel Klinke)
  - Github:  https://github.com/DKlinke/RTutorCreativity
  - shinyapps.io: https://dklinke.shinyapps.io/RTutorCreativity/

### Assessing Free Trade Agreements (by Tobias Fischer)
  - Github: https://github.com/fischeruu/RTutorNAFTAfreetrade
  - shinyapps.io: https://fischeruu.shinyapps.io/RTutorNAFTAfreetrade/

### CO2 Trading and Risk of Firm Relocation (by Benjamin Lux)
  - Github: https://github.com/b-lux/RTutorCarbonLeakage
  - shinyapps.io: https://b-lux.shinyapps.io/RTutorCarbonLeakage/

### The Effects of Defaults on Donations (by Stefanie Buda)
  - Github: https://github.com/StefanieBuda/RTutorDefaultsAndDonations
  - shinyapps.io: https://stefaniebuda.shinyapps.io/RTutorDefaultsAndDonations/  

### Insurance and the Church (by Adidti Malani)
  - Github: https://github.com/aditi-malani/RTutorGodInsuresThoseWhoPay 
  - shinyapps.io: https://aditi-malani.shinyapps.io/RTutorGodInsuresThoseWhoPay/ 

### Public Infrastructure Spending and Voting Behaviour (by Philipp Klotz)
  - Github: https://github.com/PhilippKlotz/RTutorPublicSpendingVotingBehavior
  - shinyapps.io: http://philippklotz.shinyapps.io/RTutorPublicSpendingVotingBehavior

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

### How much less effective are publicly assigned lawyers? (by Artemij Cadov)
  - Github: [https://github.com/KendamaQQ/LawyersLemon](https://github.com/KendamaQQ/LawyersLemon)
  - shinyapps.io: [https://kendamaqq.shinyapps.io/RTutorLawyers/](https://kendamaqq.shinyapps.io/RTutorLawyers/)
  
### Air pollution and house prices (by Moritz Sporer)
  - Github: [https://github.com/msporer/RTutorEnvironmentalRegulation](https://github.com/msporer/RTutorEnvironmentalRegulation)
  - shinyapps.io: [https://msporer.shinyapps.io/RTutorEnvironmentalRegulations/](https://msporer.shinyapps.io/RTutorEnvironmentalRegulations/)

### Female Role Models for Potential Economics Students (by Stefanie Buda)

  - GitHub: https://github.com/StefanieBuda/RTutorInfluenceOfFemaleRoleModels
  - shinyapps.io: http://stefaniebuda.shinyapps.io/RTutorInfluenceOfFemaleRoleModels
  
### Predicting Effects of Carbon Pricing on US Electricity Production (by Daniel Dreyer)

  - Github: [(https://github.com/danieldreyer/RTutorCO2ReductionCosts]((https://github.com/danieldreyer/RTutorCO2ReductionCosts)
  - shinyapps.io: [https://danieldreyer.shinyapps.io/RTutorCO2ReductionCosts/](https://danieldreyer.shinyapps.io/RTutorCO2ReductionCosts/)

### A macroeconomic study of credit booms and busts (by Thomas Clausing)
  - Github: https://github.com/tcl89/creditboomsgonebust
  - shinyapps.io: https://tcl89.shinyapps.io/creditboomsgonebust 

### The impact of emmission trading on green innovation (by Arthur Schäfer)
  - Github: https://github.com/ArthurS90/RTutorEmissionTrading
  - shinyapps.io: https://arthurs90.shinyapps.io/RTutorEmissionTrading/

### Social Spillovers in Movie Consumption (by Lara Santak)
  - Github: https://github.com/larasantak/RTutorSomethingToTalkAbout
  - shinyapps.io: https://lara-santak.shinyapps.io/RTutorSomethingToTalkAbout/

### Building Codes and Energy Efficiency (2 versions, by Simon Hertle and Lisa Eilts)
  - Github (Simon Hertle): https://github.com/simonhertle/RTutorBuildingCodes
  - shinyapps.io (Simon Hertle): https://shertle.shinyapps.io/RTutorBuildingCodes/
  - Github (Lisa Eilts): https://github.com/LEilts/RTutorBuildingCodes

### Gasoline Prices and Consumer Behavior (by Melina Klenk)

  - Github: https://github.com/melinakle/RTutorGasolineTaxes
  - shinyapps.io: https://melina-kle.shinyapps.io/RTutorGasolineTaxes/

### Technological Progress and Fuel Economy of Cars (by Marius Breitmayer)
  - Github: https://github.com/MariusBreitmayer/RTutorAttributeTradeOffs
  - shinyapps.io: https://mariusbreitmayer.shinyapps.io/RTutorAttributeTradeOffs

### Experiment zu sozialen Netzwerken und Vertragserfuellung (by Fabian Zeiher)
  - Github: https://github.com/zeiherfabian/RTutorSozialeNetzwerke	
  - shinyapps.io: https://daprtp-fabian-zeiher.shinyapps.io/RTutorSozialeNetzwerke/
  
### Long-Term Effects of Communism in Eastern Europe (by Benjamin Markert)
  - Github: https://github.com/BenjaminMarkert/RTutorLongTermEffectsCommunism	
  - shinyapps.io: https://benjaminmarkert.shinyapps.io/RTutorLongTermEffectsCommunism
  
### How can Scandinavians tax so much? (by David Hertle)
  - Github: https://github.com/dhertle/RTutorTaxationScandinavia
  
### An interesting case study of a bank run (by Joachim Plath)
  - Github: https://github.com/skranz/RTutorBankRuns


## Courses that use RTutor problem sets

If you a have course that uses RTutor that you want to share, just send me an email and I add your course to the list! 

- Two courses from me: [Empirical Economics with R](https://github.com/skranz/empecon) and [Market Analysis with Econometrics and Machine Learning](https://github.com/skranz/MarketAnalysis). Both consists of online shiny apps with videos and quizzes and many RTutor problem sets.

- [Jade Benjamin-Chung](https://www.ocf.berkeley.edu/~jadebc/) from UC Berkeley School of Public Health has created with RTutor online tutorials for an [introductory R course for epidemiologists](https://ucb-epi-r.github.io/). If you click on a tutorial the corresponding RTutor problem set can be directly solved on shinyapps.io. There is no need to log in.

- RTutor is also used in a compulsory [data science project course](https://github.com/AlexRieber/datascience-teaching) taught by [Alex Rieber](https://github.com/AlexRieber/) for business and economics students at Ulm University. The problem sets teach basic skills in R, including tidyverse data wrangling, as well as econometric and machine learning basic with economic applications. Alex published the problem sets and other course material [here on Github](https://github.com/AlexRieber/datascience-teaching). You find on the Github pages also links that allow you to test the problem sets on the rstudio cloud. The course is in German but Alex already started to make an English version of the problem sets, which will be added once finished.


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
devtools::install_github("skranz/RTutor", upgrade="ask")
```
(You may have to restart your R session / RStudio for the update to work.)


## Suggestions & Feedback

If you have suggestions or find bugs, please don't hesitate to open an [issue on the Github page](https://github.com/skranz/RTutor/issues).
