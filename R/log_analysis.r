examples.import.logs = function() {
  dir = "D:/lehre/energieoekonomik/rinteractive"
  ret = import.logs(dir)

  ret$ex.df
  head(ret$fail.df)
  ret$exus.df
}

import.logs = function(dir, check.sub.dir = TRUE) {
  restore.point("import.logs")

  library(dplyr)
  library(stringtools)  
  adapt.ps.dir(dir)
  
  files = list.files(path=dir,pattern=glob2rx("*.log"), recursive=check.sub.dir, full.names=TRUE)
  
  li = lapply(files,function(file) {
    restore.point("fhehf")
    
     
    txt = readLines(file)
    close.rows = which(txt == "}")
    close.rows = close.rows[-length(close.rows)]
    txt[close.rows] = paste0(txt[close.rows],",")
    txt[length(txt)] = "}]"
    txt[1]='[ {'
    txt = paste0(txt,collapse="")
    
    li = fromJSON(txt)
    li = lapply(li, function(obj) do.call("data.table",obj[-length(obj)]))
    dt = rbindlist(li)
    dt$logId = round(runif(1,0,1e11))
    dt$logFile = file
    
    ex.file = paste0(dirname(file),"/",str.left.of(basename(file),".log"),".r")
    txt = readLines(ex.file)
    user.row = txt[str.starts.with(str.trim(txt),"user.name")][1]
    user.name = str.trim(str.left.of(str.right.of(user.row, "="),"#"))
    dt$user.name = user.name
    
    dt
  })
  dt = rbindlist(li)
  
  setcolorder(dt, c("user.name",colnames(dt)[-NCOL(dt)]))

  dt = mutate(dt,
              ok  = failure.short == "No failure message recorded",
              not.exist = str.ends.with(failure.short," does not exist"),
              error = (!(ok | not.exist)) & code.changed
  )
  
  
  df = as.data.frame(dt)
  
  # Show list of all errors
  all.df = df %.%
    filter(error==TRUE) %.%
    dplyr::select(-stud.code)
  
  # Summarize errors by exercise and users
  exus.df = df %.%
    group_by(ex.name,ps.name,user.name) %.%
    summarise(sum_error=sum(error), once_ok=any(ok), log_entries=length(ok)) %.%
    arrange(user.name, ex.name)
  exus.df
  
  # Count failures of specific type
  fail.df = df %.%
    filter(error==TRUE) %.%
    group_by(ps.name,ex.name,failure.short) %.%
    summarise(times.failure=length(failure.short), num.users=length(unique(user.name))) %.%
    arrange(desc(num.users),desc(times.failure), ex.name) %.%
    mutate(failure.short = str.left(failure.short,50)) %.%
    dplyr::select(num.users,times.failure, ex.name, failure.short, ps.name)
 
  fail.df = as.data.frame(fail.df)
  fail.df
  
  colnames(df)
   
  # Summarize errors by exercise
  ex.df = exus.df %.%
    group_by(ex.name,ps.name) %.%
    summarise(mean_errors=mean(sum_error), share_users_correct=mean(once_ok)) %.%
    arrange(share_users_correct,desc(mean_errors), ex.name)     
  ex.df  
  
  
  list(all.df=all.df,exus.df=exus.df, ex.df=ex.df, fail.df=fail.df)
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