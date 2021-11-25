# Deploying an interactive RTutor problem set on shinyapps.io

# 1. Install in the subfolder ./app all the data sets that are used by
#    your problem set.

# 2. You must now install the rsconnect package and open a free account
#    on shinyapps.io. See the description here:
#    http://shiny.rstudio.com/articles/shinyapps.html

# install.packages('rsconnect')

#  3. Then adapt and run the lines below
if (DIRECT_EXECUTION) {


  # You must adapt the following lines to your account as explained in
  #  http://shiny.rstudio.com/articles/shinyapps.html 

  rsconnect::setAccountInfo(
    name='SHINYAPPS_USERNAME',
    token='SHINYAPPS_TOKEN',
    secret='SHINYAPPS_SECRET')

  library(rsconnect)
  
  # Set app directory as working directory
  setwd("APP_PATH/app")
  
  # Set skranz.r-universe.dev repo.
  # Needed so that shinyapps.io finds all required packages
  options(repos = c(skranz = 'https://skranz.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))  

  # This command deploys your app on shinyapps.io
  deployApp(appName="APP_NAME")

  # You can now log in on shinyapps.io to get usage statistics about your app
}