examples.import.logs = function() {
  dir = "D:/lehre/empIOUlm/rtutor/"
  df = import.logs(dir)

  ret$ex.df
  head(ret$fail.df)
  ret$exus.df
}

import.logs = function(dir, check.sub.dir = TRUE) {
  restore.point("import.logs")

  library(dplyr)
  library(stringtools)
  library(data.table)
  #adapt.ps.dir(dir)
  
  files = list.files(path=dir,pattern=glob2rx("*.log"), recursive=check.sub.dir, full.names=TRUE)
  
  file = files[1]
  
  li = lapply(files,function(file) {
    restore.point("fhehf")
    
     
    txt = readLines(file)
    last.row = max(which(str.trim(txt)==","))
    txt = c("[",txt[1:(last.row-1)],"]")
    txt = paste0(txt,collapse="")
    
    li = fromJSON(txt, simplifyDataFrame = FALSE)
  
    df = as.tbl(as.data.frame(rbindlist(li, fill=TRUE)))
    df = mutate(df,logFile=file)
    df
  })
  df = bind_rows(li)
  df = mutate(df,
    ce = paste0(chunk,".",e.ind),
    row = 1:NROW(df)
  )
}

log.summary = function(df, num.chunks = max(df$chunk)) {

  num.users = length(unique(df$user))
  
  d = df
  d = filter(d, !is.na(chunk))
  d = group_by(d,chunk,user)
  d = mutate(d, was.solved = cumsum(type=="check_chunk" & ok)>0)
  
  # Summarise by user and chunk
  dcu = suppressWarnings(summarise(d,
    solved =  any(type=="check_chunk" & ok),
    errors =  sum(type=="check_chunk" & !ok & !was.solved),
    hints  =  sum(type=="hint" & !was.solved),
    start.time = min(time),
    solved.time = min(time[type=="check_chunk" & ok])
  ))
  
  # Summarise by chunk
  dc = summarise(group_by(dcu,chunk),
    attempted = NROW(solved),             
    solved =  sum(solved),
    not.solved = sum(!solved),
    av.errors = mean(errors),
    av.hints = mean(hints),
    used.hint = sum(hints>0),
    no.hint = sum(hints==0)
  )
  
  list(dcu=dcu,dc=dc)
}

# Changes ps.dir to the current folder for all problem set files
adapt.ps.dir = function(dir) {
  
  files = c(list.files(path=dir,pattern=glob2rx("*.r"), recursive=check.sub.dir, full.names=TRUE),
            list.files(path=dir,pattern=glob2rx("*.R"), recursive=check.sub.dir, full.names=TRUE))
  
  li = lapply(files,function(file) {
    restore.point("adapt")
        
    txt = readLines(file)
    row = which(str.starts.with(str.trim(txt),"ps.dir"))[1]
    txt[row] = paste0("ps.dir = '",dirname(file),"'")
    writeLines(txt,file)
  })  
}