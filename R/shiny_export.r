export.ui = function(ps=get.ps()) {
  restore.point("export.ui")
  make.export.handlers()
  if (file.exists("downloads.zip")) {
    zip.ui = list(
      downloadButton("downloadZipBtn","Download additional material"),
      helpText("The ZIP file contains additional material, e.g. data or R scipts that is needed to replicate the analysis and run the RMarkdown file on your computer. You need to extract the zip file into the folder of your RMarkdown file.")
    )
  } else {
    zip.ui = NULL
  } 
  
  list(
    br(),
    downloadButton("downloadRmdBtn","Download as RMarkdown"),
    helpText("You can download the problem set with your current solution as an RMarkdown text file. Save it in a directory and open it with RStudio, to run freely the code on your computer and to create HTML, Word or PDF files from your solution. (Note: If you directly open the file with RStudio without saving it first, the file is probably stored in a directory to which you have no write access and RStudio gives the error 'Access denied' if you want to knit the file.)"),
    br(),
    zip.ui
  )
}


make.export.handlers = function(session=ps$session,ps=get.ps(), app=getApp()) {
  setDownloadHandler("downloadRmdBtn", 
    filename = paste0(ps$name, ".Rmd"),
    content = function(file) {
      txt = shiny.to.rmd.txt()
      writeLines(txt, file)
    },
    contentType = "text/Rmd"
  )
  
  setDownloadHandler("downloadZipBtn", 
      filename <- paste0(ps$name, "_extra_material.zip"),
      content <- function(file) {
        file.copy("downloads.zip", file)
      },
      contentType = "application/zip"
    )
}

shiny.to.rmd.txt = function(ps=get.ps(), user.name=get.user.name()) {
  restore.point("shiny.to.rmd.txt")
  cdt = ps$cdt
  rps = ps$rps
  
  txt = rps$empty.rmd.txt
  cl = rps$empty.rmd.chunk.lines
  rownames(cl) = cl$chunk.name
  chunks = intersect(cdt$chunk.name, cl$chunk.name)
  
  cl.ind = match(chunks, cl$chunk.name)
  cdt.ind = match(chunks, cdt$chunk.name)
  stud.code = cdt$stud.code[cdt.ind]
  start.lines = cl$start.line[cl.ind]
  clear.lines = do.call("c",lapply(cl.ind, function(i) int.seq(cl$start.line[i]+1,cl$end.line[i]-1)))
  
  txt[start.lines] = paste0(txt[start.lines],"\n",stud.code)
  
  addon.lines = which(str.starts.with(txt,"#! addon__"))
  txt[addon.lines] = sapply(txt[addon.lines],make.rmd.addon.txt,ps=ps)
  
  if (length(clear.lines)>0)
    txt = txt[-clear.lines]
  
  txt

}

make.rmd.addon.txt = function(str, ps=get.ps(), ups=get.ups()) {
  restore.point("make.rmd.addon.txt")
  id = str.right.of(str, "#! ")
  str = str.right.of(str,"#! addon__")
  type = str.left.of(str,"__")
  name = str.right.of(str,"__")
  
  Addon = ps$rps$Addons[[type]]
  ao = ps$rps$addons[[id]]
  rta = ao$rta

  res = Addon$out.txt.fun(ao,solved=isTRUE(rta$was.solved))
  paste0(res, collapse="\n")
}
