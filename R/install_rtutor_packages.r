# Install required packages for RTutor
# put this code into a gist

install.rtutor = function(update.cran=FALSE, update.github=TRUE) {
  
  cat("\nInstall required packages from CRAN...")
  
  pkgs = c("devtools","whisker","stringr","data.table",
    "dplyr","shiny","shinyBS","xtable","lmtest","texreg")
  for (pkg in pkgs) {
    if (!update.cran) {
      if (!require(pkg, character.only=TRUE) | update.cran)
        install.packages(pkg)
    } else {
      try(detach(paste0("package:",pkg), character.only=TRUE, force=TRUE), silent=TRUE)
      install.packages(pkg)    
    }
  }

  cat("\nInstall required packages from Github...")

  # Install packages from Github
  repos = c("skranz/restorepoint",
    "skranz/stringtools",
    "skranz/shinyAce",
    "skranz/dplyrExtras",
    "skranz/regtools",
    "skranz/RTutor"
  )
  for (repo in repos) {
    pkg = strsplit(repo,"/", fixed=TRUE)[[1]][2]
    if (!update.github) {
      if (!require(pkg, character.only=TRUE) | overwrite.github)
        install_github(repo=repo)
    } else {
      try(detach(paste0("package:",pkg), character.only=TRUE,force=TRUE), silent=TRUE)
      install_github(repo=repo)
    }
  }
}

# install.rtutor()