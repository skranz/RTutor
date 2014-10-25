
examples.deploy.ps = function() {
  deploy.ps()
}

# Copies data sets and other required files to solve problem set
# into the directory specified with dir
deploy.ps = function(ps.name=pkg$ps[1], dir=getwd(), material.dir=NULL, pkg = ps.pkg.info(), ask.user=TRUE, overwrite=FALSE) {
  restore.point("deploy.ps")
    
  if (is.null(material.dir)) {
    pkg.dir = path.package(info$package)
    material.dir = paste0(pkg.dir,"/material/",ps.name)
  }
  
  if (!overwrite) {
    if (is.ps.deployed(dir=dir, material.dir=material.dir)) {
      return(TRUE)
    }
  }

  if (!file.exists(dir)){
    warning(paste0("The directory '",dir,"' does not yet exist. Please create the directory before you proceed."))
    return(FALSE)
  }

  
  if (ask.user) {
    cat(paste0("\nDo you want to deploy the problem set to '", dir,"'?"))
    answer <- readline("Type y if this is ok: ")
    if (!identical(tolower(answer),"y")) {
      cat("\nCancelled deployment of problem set. If you want to deploy and run the problem set in a different directory, set the argument 'dir' in run.ps or deploy.ps.")
      return(FALSE)
    }
  }
  
  cat("\nCopy files to '", dir,"'...",sep="")

  # Copy files into working directory
  files= list.files(material.dir,pattern=".*",full.names = TRUE)  
  file.copy(from=files, to=dir, overwrite = overwrite, recursive = TRUE,copy.mode = !TRUE)
  
  cat(" done!")
  return(TRUE)
}

is.ps.deployed = function(dir,material.dir) {
  need.files= list.files(material.dir,full.names = FALSE)
  has.files = list.files(dir,full.names = FALSE)
  
  length(setdiff(need.files, has.files)) == 0
}

examples.run.ps = function() {
  setwd("D:/libraries/RTutor/work")
  library(RTutorShroudedFees)
  run.ps(user.name="Seb")
  detach("package:RTutorShroudedFees", unload=TRUE)  
  
}


#' Run a problem set from a package in the browser
#' 
#' Only works if a package with problem sets is loaded.
#' For problem sets stored in a local .rps file use show.ps() instead
run.ps = function(user.name, ps.name=info$ps[1], dir=getwd(), info=ps.pkg.info(), ...) {
  restore.point("run.ps")
  
  setwd(dir)
  
  pkg.dir = path.package(info$package)
  rps.dir = paste0(pkg.dir,"/ps")
  material.dir = paste0(pkg.dir,"/material/",ps.name)
  
  ret = deploy.ps(ps.name=ps.name, dir=dir, material.dir=material.dir)
  if (!ret) {
    return()
  }
  show.ps(user.name=user.name, ps.name=ps.name, dir=dir, rps.dir=rps.dir,...)
}
