
examples.deploy.ps = function() {
  deploy.ps()
}

# Copies data sets and other required files to solve problem set
# into the directory specified with dir
deploy.ps = function(ps.name=pkg$ps[1], dir=getwd(), info = ps.pkg.info(),
    ask.user=TRUE, overwrite=FALSE,
    pkg.dir = path.package(info$package),
    rps.dir = find.pkg.rps.dir(ps.name, pkg.dir),
    material.dir = find.pkg.material.dir(ps.name, pkg.dir) ) 
{
  restore.point("deploy.ps")
      
  if (!file.exists(dir)){
    warning(paste0("The directory '",dir,"' does not yet exist. Please create the directory before you proceed."))
    return(FALSE)
  }

  if (is.null(material.dir)) {
    cat("\nThe problem set has no additional materials. No files need to be copied to the working directory '",dir,"'.")
    return(TRUE)
  }
  
  if (!overwrite) {
    if (is.ps.deployed(dir=dir, material.dir=material.dir)) {
      return(TRUE)
    }
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
  library(RTutorTopIncomeTaxation)
  run.ps(user.name="default_user")
  detach("package:RTutorShroudedFees", unload=TRUE)  
  
}


get.package.info = function(package=NULL) {
  restore.point("get.package.info")
  if (is.null(package))
    return(ps.pkg.info())
  
  library(package,character.only = TRUE)
  
  call = paste0(package,"::ps.pkg.info()")
  eval(base::parse(text=call))
}


# Try old and new folder structure
find.pkg.rps.dir = function(ps.name, pkg.dir) {
  restore.point("find.pkg.rps.dir")
  # New folder format /ps/ps.name
  dir = paste0(pkg.dir,"/ps/",ps.name)      
  if (file.exists(dir)) return(dir)
  
  # Old folder format /ps
  dir = paste0(pkg.dir,"/ps")      
  if (file.exists(dir)) return(dir)
  
  err = paste0("Could not find ps directory for ", ps.name, " in your package folder ", pkg.dir)
  stop(err)
}

# Try old and new folder structure
find.pkg.material.dir = function(ps.name, pkg.dir) {
  restore.point("find.pkg.material.dir")
  
  # New folder format /ps/ps.name/material
  dir = paste0(pkg.dir,"/ps/",ps.name,"/material")      
  if (file.exists(dir)) return(dir)
  
  # Old folder format /material/ps.name
  material.dir = paste0(pkg.dir,"/material/",ps.name)    
  if (file.exists(dir)) return(dir)

  # No material directory found
  #warning("Your problem set has no material directory.")
  return(NULL)
}



#' Run a problem set from a package in the browser
#' 
#' Only works if a package for the problem set is loaded.
#' For problem sets stored in a local .rps file, use show.ps() instead
#' 
#' @param user.name Your user name
#' @param ps.name Name of the problem set. By default the first problem set name of the loaded RTutor problem set package.
#' @param package name of the package that contains your problem set. Is automatically chosen a (single) package with an RTutor problem set is loaded.
#' @param auto.save.code If TRUE all entered code will be automatically saved for the user. If FALSE (default) no entered code is saved. The user statistics (how many chunks are solved) are still saved, however.
#' @param run.solved If TRUE and also sample.soulution=TRUE all previously solved chunks will be run when the problem set is newly shown. This can be very time consuming. I would suggest in most cases to keep the default run.solved=FALSE. 
#' @param clear.user If TRUE all previously saved data for the user is removed if the problem set starts. Can be useful for developlmen or for resetting things.
#' @param sample.solution If TRUE the sample solution is shown in all chunks. Can be useful when developing a problem set. Note that one can create a problem set such that the sample solution is not available, e.g. if one wants to avoid that students just look at the sample solution.
#' @param show.solution.btn If TRUE add a button to each chunk to show the sample solution. Note that one can create a problem set such that the sample solution is never available.
#' @param launch.browser if TRUE (default) show the problem set in the browser. Otherwise it is shown in the RStudio viewer pane
#' @param dir your working directory for the problem set
#' @param pkg.dir the package directory under which problem set files are searched under pkg.dir/ps/ps.name/. Will be set by default to currently loaded RTutorProblemSet package
#' @param rps.dir directory of rps.files. Will be set to default for current package
#' @param material.dir directory of additional problem set files. Will be set to default for current package
#' @param ... additional arguments of show.ps
#' 
#' 
run.ps = function(user.name, ps.name=info$ps[1],dir=getwd(), package=NULL,
 auto.save.code = FALSE,clear.user=FALSE,run.solved=FALSE,sample.solution=FALSE,show.solution.btn=NA,launch.browser=TRUE,info=get.package.info(package),deploy.local = !make.web.app, make.web.app=FALSE, pkg.dir = path.package(info$package),rps.dir = find.pkg.rps.dir(ps.name, pkg.dir), material.dir = find.pkg.material.dir(ps.name, pkg.dir), ...) {
  
  #browser()
  restore.point("run.ps")
  if (deploy.local) {
    setwd(dir)
    ret = deploy.ps(ps.name=ps.name, dir=dir, material.dir=material.dir, 
                    info=info, rps.dir=rps.dir, pkg.dir=pkg.dir)
    if (!ret) {
      return()
    }
  }
  show.ps(user.name=user.name, ps.name=ps.name,
    auto.save.code = auto.save.code,clear.user=clear.user,run.solved=run.solved,sample.solution=sample.solution,show.solution.btn=show.solution.btn,launch.browser=launch.browser,
    dir=dir, rps.dir=rps.dir, ...)
}

examples.rtutor.package.skel = function() {
    #setwd("C:/Users/Joachim/Documents/BC/Atombombe")
  setwd("D:/libraries/RTutor/examples")
  
  set.restore.point.options(display.restore.point = TRUE)
  
  library(RTutor)
  ps.name = "understanding bank runs" 
  sol.file = paste0(ps.name,"_sol.Rmd") 
  libs = NULL
  libs = c("foreign","reshape2","plyr","dplyr","mfx", "ggplot2","knitr","regtools","ggthemes","dplyrExtras","grid","gridExtra","prettyR") # character vector of all packages you load in the problem set
  
  name.rmd.chunks(sol.file,only.empty.chunks=FALSE)
  
  # Create problemset
  create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs, extra.code.file = "extracode.r", var.txt.file = "variables.txt")

  rtutor.package.skel(sol.file=sol.file, ps.name=ps.name, pkg.name="RTutorBankRuns", pkg.parent.dir = "D:/libraries/RTutorBankRuns", libs=libs, author="Joachim Plath", github.user="skranz", extra.code.file = "extracode.r", var.txt.file = "variables.txt", overwrite=TRUE)
  
  
  ##### Example 
  setwd("D:/libraries/RTutor/examples")
  ps.name = "Example"; sol.file = paste0(ps.name,"_sol.Rmd")
  libs = c() # character vector of all packages you load in the problem set
  #name.rmd.chunks(sol.file) # set auto chunk names in this file

  create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs, stop.when.finished=FALSE)

   rtutor.package.skel(sol.file=sol.file, ps.name=ps.name, pkg.name="RTutorExample", pkg.parent.dir = "D:/libraries/RTutorExample", libs=libs, author="Sebastian Kranz", github.user="skranz", overwrite=TRUE)

}



#' Generate a package skeleton for a shiny based RTutor problem set that shall be deployed as a package
#'  
rtutor.package.skel = function(sol.file,ps.name,  pkg.name, pkg.parent.dir,author="AUTHOR_NAME", github.user = "GITHUB_USERNAME", date=format(Sys.time(),"%Y-%d-%m"),  source.dir = getwd(),rps.file = paste0(ps.name,".rps"), libs=NULL, extra.code.file=NULL, var.txt.file=NULL, ps.file = paste0(ps.name,".Rmd"), overwrite=FALSE, overwrite.ps=TRUE,...) {
  #create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs, extra.code.file = "extracode.r", var.txt.file = "variables.txt")
  restore.point("rtutor.package.skel")

  dest.dir = paste0(pkg.parent.dir,"/", pkg.name)
  skel.dir = paste0(path.package("RTutor", quiet = FALSE),"/ps_pkg_skel")
  
  if (!file.exists(dest.dir))
    dir.create(dest.dir, recursive = TRUE)
  
  # Copy package skeleton
  long.skel.files = list.files(skel.dir,full.names = TRUE)
  file.copy(from = long.skel.files,to = dest.dir, overwrite=overwrite, recursive = TRUE)

  
  rps.dir =paste0(dest.dir,"/inst/ps/",ps.name)
  if (!file.exists(rps.dir))
    dir.create(rps.dir, recursive = TRUE)
  
  mat.dir = paste0(dest.dir,"/inst/ps/",ps.name,"/material")
  if (!file.exists(mat.dir))
    dir.create(mat.dir, recursive = TRUE)

  
  # Replace placeholder strings
  
  
  dest.files = c("R/package_info.r","DESCRIPTION","NAMESPACE","README.md")
  dest.files = paste0(dest.dir,"/",dest.files)
  file = dest.files[1]
  if (length(libs)>0) {
    lib.txt = paste0("RTutor, ", paste0(libs, collapse=", "))
  } else {
    lib.txt = "RTutor"
  }
  descr.txt = paste0("RTutor problem set ", ps.name)
  for (file in dest.files) {
    txt = readLines(file, warn=FALSE)
    txt = gsub("PACKAGE_NAME",pkg.name,txt, fixed=TRUE)
    txt = gsub("PROBLEM_SET_NAME",ps.name,txt, fixed=TRUE)
    txt = gsub("AUTHOR_NAME",author,txt, fixed=TRUE)
    txt = gsub("CURRENT_DATE",date,txt, fixed=TRUE)
    txt = gsub("DEPENDS_LIBRARIES",lib.txt,txt, fixed=TRUE)
    txt = gsub("DESCRIPTION_TITLE",descr.txt,txt, fixed=TRUE)
    txt = gsub("GITHUB_USERNAME",github.user,txt, fixed=TRUE)
    
    writeLines(txt,file)
  }
  
  
  # Copy files into ps
  file.copy(from = c(sol.file, rps.file, extra.code.file, var.txt.file, ps.file), to=rps.dir, overwrite=overwrite.ps)
  
  cat(paste0("Package skeleton created in ", paste0(dest.dir,"/",pkg.name), ". ",
             "\nRead 'TO DO.txt' for the remaining steps."))
  
}

example.rtutor.app.skel = function() {
  setwd("D:/libraries/RTutor/examples")
  ps.name = "Example"; sol.file = paste0(ps.name,"_sol.Rmd")
  libs = c() # character vector of all packages you load in the problem set
  #name.rmd.chunks(sol.file) # set auto chunk names in this file

  create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs, stop.when.finished=FALSE)

  app.dir = "D:/libraries/RTutor/examples/ExampleApp"
  
  # Create app based on .rps
  
  ps.name = "Example" # problem set name
  # Folder that contains your .rps file
  rps.dir = "D:/libraries/RTutor/examples"
  # Folder in which app shall be created
  app.dir = "D:/libraries/RTutor/examples/ExampleApp"
  rtutor.app.skel(ps.name=ps.name, app.name="RTutorExample",app.dir=app.dir, 
                  rps.app = TRUE, rps.dir = rps.dir, overwrite=TRUE)
 

  # Create app based on a problem set package
  ps.name = "Example"
  app.dir = "D:/libraries/RTutor/examples/ExampleApp"
  rtutor.app.skel(ps.name=ps.name, app.name="RTutorExample",app.dir=app.dir, 
                  pkg.name = "RTutorExample", rps.app = FALSE,
                  github.user = "skranz", overwrite=TRUE)
  
}

#' Generate a skeleton for a shinyapps.io app of a problem set
#' 
#' @param ps.name Name of the problem set
#' @param app.name Name of your app. Should have no white spaces or special characters
#' @param app.dir Your local directory to which you want to deploy your app files
#' @param rps.app logical. If `TRUE` create an app based on an .rps file. Otherwise create the app based on a problem set package that is hosted on Github.
#' @param pkg.name If you create the app from a package this is the name of your package.
#' @param github.user If you create the app from a package this is the name of your Github user name.
#' @param rtutor.fork Note that shinyapps.io only works with R packages directly installed from Github or CRAN. It is therefore not possible to locally change RTutor and use the adapted version for your own problemsets. This option however allows you to refer to your fork on github. Default is the main package under skranz. 
#' @param rps.file The name of your rps file without directory if you create the app from a .rps file
#' @param rps.dir the folder of your rps.file 
#' @param ps.show.opts ps.show() arguments which are added to the generated ps.show. Has to be given as a named list, e.g. `ps.show.opts=list(show.solution.btn=FALSE)` if one wants to create an app which does not show the solution button. By default only the necessary options are set. If those are provided, they are overwritten. This way, one can for example set the user.name to something different than Guest.
rtutor.app.skel = function(ps.name, app.name=ps.name, app.dir,rps.app=!is.null(rps.dir), pkg.name=NULL, rps.file = paste0(ps.name,".rps"), rps.dir=NULL, overwrite=FALSE, github.user = "GITHUB_USERNAME", rtutor.fork="skranz", libs=NULL, ps.show.opts=NULL, ...) {
  #create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs, extra.code.file = "extracode.r", var.txt.file = "variables.txt")
  restore.point("rtutor.app.skel")

  
  if (!file.exists(app.dir))
    dir.create(app.dir)
  
  app.app.dir = paste0(app.dir,"/app")
  if (!file.exists(app.app.dir))
    dir.create(app.app.dir)

  work.dir = paste0(app.dir,"/app/work")
  if (!file.exists(work.dir))
    dir.create(work.dir)

  
  if (!rps.app) {
    base.dir = path.package("RTutor", quiet = FALSE)
    skel.dir = paste0(base.dir,"/ps_app_skel/packageApp")
    ps.show.opts.string = rtutor.skel.show.opts.string(mandatory=list(user.name = "Guest",
                                                                      deploy.local = FALSE,
                                                                      make.web.app = TRUE, 
                                                                      save.nothing=FALSE,
                                                                      offline=FALSE),
                                                       optional=ps.show.opts)
  } else {
    base.dir = path.package("RTutor", quiet = FALSE)
    skel.dir = paste0(base.dir,"/ps_app_skel/rpsApp")
    ps.show.opts.string = rtutor.skel.show.opts.string(mandatory=list(user.name = "Guest",
                                                                      ps.name = ps.name,
                                                                      make.web.app = TRUE, 
                                                                      save.nothing=FALSE,
                                                                      offline=FALSE),
                                                       optional=ps.show.opts)
    
    file.copy(from = paste0(rps.dir,"/",rps.file),to = work.dir,
              overwrite=overwrite, recursive = TRUE)
  }
  

  # Copy app skeleton
  long.skel.files = list.files(skel.dir,full.names = TRUE)
  file.copy(from = long.skel.files,to = app.dir, overwrite=overwrite, recursive = TRUE)
  
  
  # Replace placeholder strings
  dest.files = c("deployapp.R","app/global.R")
  dest.files = paste0(app.dir,"/",dest.files)
  file = dest.files[1]
  descr.txt = paste0("RTutor problem set ", ps.name)
  
  if (length(libs)==0) {
    lib.txt = ""
  } else {
    lib.txt = paste0("library(",libs,")", collapse="\n")
  }

  
  for (file in dest.files) {
    txt = readLines(file, warn=FALSE)
    if (!is.null(pkg.name))
      txt = gsub("PACKAGE_NAME",pkg.name,txt, fixed=TRUE)
    txt = gsub("DEPENDS_LIBRARIES",lib.txt,txt, fixed=TRUE)
    txt = gsub("APP_NAME",app.name,txt, fixed=TRUE)
    txt = gsub("APP_PATH",app.dir,txt, fixed=TRUE)
    txt = gsub("GITHUB_USERNAME",github.user,txt, fixed=TRUE)
    txt = gsub("PS_OPTIONS",ps.show.opts.string,txt, fixed=TRUE)
    txt = gsub("FORK_DEFAULT",rtutor.fork,txt,fixed=TRUE)
    writeLines(txt,file)
  }
  
  cat(paste0("App skeleton created in ", app.dir, ". ",
             "\nAdapt the file 'deployapp.R' and run the commands to deploy your app on shinyapps.io."))
  
}
  
#' Intermediary Function helping to build the ps.show() Options string
#' 
#' Expects two lists with arguments. 
#' 
#' @param mandatory Are always set but may be overwritten by optional
#' @param optional Are intended to be set by the user. May overwrite mandatory ones if set explicitely. 
rtutor.skel.show.opts.string = function(mandatory, optional){
  restore.point("rtutor.skel.show.opts.string")
  
  if(!is.null(optional)){
    #Testing
    if(length(names(optional))!=length(optional)){
      stop("It is necessary to provide a list where each element is named to generate a ps.show() option list!")
    }
    
    mandatory.remain = mandatory[!(names(mandatory) %in% names(optional))]
    all.list = c(mandatory.remain,optional)
  } else {
    all.list = mandatory
  }
  
  chars = sapply(all.list,FUN=is.character)
  
  if(any(chars)) all.list[chars] = paste("\"",all.list[chars],"\"", sep="")
  
  arg.string = paste(names(all.list),all.list,sep="=",collapse=", ")
  
  return(arg.string)
}

