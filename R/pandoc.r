find.pandoc.path = function() {
  rmarkdown:::find_pandoc()
  rmarkdown:::.pandoc$dir
}

rmd.to.latex = function(input, output=NULL, pandoc.path = find.pandoc.path(), pandoc.args="--table-of-contents --toc-depth 2") {
  library(knitr)
  base.name = substring(input,1,nchar(input)-4)
  
  md.file = paste0(base.name, ".md")
  if (is.null(output))
    output = paste0(base.name, ".tex")
  knit(input=input, output = md.file)
  md.to.latex(md.file, output, pandoc.path = pandoc.path, pandoc.args=pandoc.args)
}

md.to.latex = function(input, output=NULL, pandoc.path = find.pandoc.path(), pandoc.args="--table-of-contents --toc-depth 2") {
  restore.point("md.to.latex")
  
  library(rmarkdown)
  if (is.null(output)) {
    base.name = substring(input,1,nchar(input)-3)
    output = paste0(base.name, ".tex")
  }
  com = paste0(pandoc.path,"/pandoc.exe ",input," -f markdown -t latex -s -o ",output," ",pandoc.args)
  cat(paste0("Converting .md to .tex via pandoc:\n",com,"\n\n"))
  system(com,wait = TRUE)
  
}