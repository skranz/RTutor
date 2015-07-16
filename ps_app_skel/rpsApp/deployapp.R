# Deploying an interactive RTutor problem set on shinyapps.io

# 1. Make sure you have generated a package for your problem set and 
#    have it hosted on github

# 2. Install in the subfolder ./app/work all the data sets that are used by
#    your problem set.

# 3. All packages that are used must have be installed in your
#    RStudio from CRAN or GITHUB (a local build of a package does not work)

if (FALSE) {
  # Run this code if you need to install some packages	
  library(devtools)
  install_github("skranz/shinyEvents",ref = "master")  
  install_github("skranz/regtools",ref = "master")
  install_github("skranz/dplyrExtras",ref = "master")
  install_github("skranz/stringtools",ref = "master")

  # Pick master or development version of RTutor
  install_github("skranz/RTutor",ref = "master")  
  #install_github("skranz/RTutor",ref = "develop")

 }

# 4. You must now install the shinyapps package and open a free account
#    on shinyapps.io. See the description here:
#    http://shiny.rstudio.com/articles/shinyapps.html

#    Then adapt and run the lines below
if (FALSE) {


  # You must adapt the following lines to your account as explained in
  #  http://shiny.rstudio.com/articles/shinyapps.html 

  shinyapps::setAccountInfo(
  	      name='<SHINYAPPS_USERNAME>',
  		  token='<TOKEN>',
		  secret='<SECRET>')

  library(shinyapps)
  
  # Set app directory as working directory
  setwd("APP_PATH/app")

  # This command deploys your app on shinyapps.io
  deployApp(appName="APP_NAME")

  # You can now log in on shinyapps.io to get usage statistics about your app
}