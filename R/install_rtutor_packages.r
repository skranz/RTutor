# Install required packages for RTutor
# put this code into a gist
 
install.rtutor = function(update.cran=FALSE, update.github=TRUE, lib=.libPaths()[1]) {
  cat("\nInstall required packages from CRAN...")
  pkgs = c("devtools","whisker","stringr","JSONIO","data.table",
  "dplyr","shiny","shinyBS","hwriter","lmtest","texreg","RCurl")
  for (pkg in pkgs) {
    if (!update.cran) {
      if (!require(pkg, character.only=TRUE) | update.cran)
        install.packages(pkg,lib=lib)
      } else {
        try(detach(paste0("package:",pkg), character.only=TRUE, force=TRUE), silent=TRUE)
        install.packages(pkg,lib=lib)
    }
  }
   
  cat("\nInstall required packages from Github...")
   
  # Install packages from Github
  repos = c("skranz/restorepoint",
  "skranz/stringtools",
  "skranz/shinyAce",
  "skranz/shinyEvents",
  "skranz/dplyrExtras",
  "skranz/regtools",
  "skranz/RTutor"
  )
  for (repo in repos) {
    pkg = strsplit(repo,"/", fixed=TRUE)[[1]][2]
    if (!update.github) {
      if (!require(pkg, character.only=TRUE) | overwrite.github)
        with_libpaths(new = lib, install_github(repo=repo))
      } else {
        try(detach(paste0("package:",pkg), character.only=TRUE,force=TRUE), silent=TRUE)
        with_libpaths(new = lib, install_github(repo=repo))
    }
  }
}
 
# install.rtutor() 