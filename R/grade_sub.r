# Grade submitted files in .sub format

grade.log = as.environment(list(current=NULL, all=NULL, file="grade_log.txt"))

examples.grade.sol = function() {
  base.dir = "D:/lehre/energieoekonomik/rtutor/sub2015"
  base.dir = "D:/lehre/empIOUlm/ps2016"
  setwd(base.dir)
  grade.sol()
}


grade.sol = function(base.dir=getwd(), prefix="",postfix=".zip", grade.dir = paste0(base.dir,"/grades")) {
  restore.point("grade.sol")
  
  library(RTutor)
  
  setwd(base.dir)
  
  res = unpack.big.zips(base.dir, prefix = prefix, postfix=postfix)
  ass = res$ass  
  as = ass[1]
  
  sub.li = load.subs(ass, base.dir = base.dir)
  grade.total.points(sub.li)
  
  log.df = import.logs(sub.li)
  
  df = filter(log.df, stud.name == log.df$stud.name[1], ps.name ==log.df$ps.name[1], type != "check_chunk")
  wt.df = log.df %>% group_by(ps.name, stud.name) %>%
    summarise(mins = estimate.working.time(times=time, break.min=20)) %>% ungroup()

  write.csv(wt.df, file = paste0(grade.dir,"/estimated work time.csv"), row.names = FALSE)
  
  cat("Files have been written to ", grade.dir)
  
  #library(ggplot2)
  #ggplot(wt.df, aes(mins, group=ps.name, fill=ps.name)) +
  #geom_histogram() + facet_wrap(~ps.name)
  
}

estimate.working.time = function(times=df$time, break.min = 10) {
  restore.point("estimate.working.time")
  times = as.numeric(as.POSIXct(times))
  times = sort(times)
  secs = diff(times)
  mins = secs / 60
  mins
  mins[mins >= break.min] = 0
  sum(mins)
}

import.logs = function(sub.li) {
  li = lapply(sub.li, function(sub) {
    sub$log.df
  })
  bind_rows(li)
}

write.grade.log = function(msg, console="cat") {
  grade.log$current = c(grade.log$current,msg)
  grade.log$all = c(grade.log$all,msg)
  if (console=="cat") {
    cat("\n",msg)
  }  else if (console=="warning") {
    warning(msg)
  }
  writeLines(grade.log$all, grade.log$file)
}

grade.total.points = function(sub.li, grade.dir = paste0(getwd(),"/grades")) {
  restore.point("grade.total.points")
  
  if (!dir.exists(grade.dir))
    dir.create(grade.dir)
  
  # Grade all rows of sub.df separately
  li = lapply(sub.li, function(sub) {
    res = sub$total
    res$user.name = sub$stud.name
    res
  })
  sub.df = bind_rows(li)
  sub.df$user.name = correct.stud.names(sub.df$user.name)
  
  
  sub.df = mutate(group_by(sub.df, ps.name),
    time.rank = rank(finished.time, ties.method="min"),
    perc.time.rank = 100*time.rank / length(time.rank),
    points.rank = rank(-points, ties.method="min"),
    hints.rank = rank(num.hints, ties.method="min")
  )
  
  sub.df = arrange(sub.df,user.name)
  write.csv(sub.df,paste0(grade.dir,"/ps_points.csv"),row.names = FALSE)
  
  max.points.df = sub.df %>% group_by(ps.name) %>% summarise(max.points = first(max.points))
  total.points = sum(max.points.df$max.points)
  
  tot.df = sub.df %>% group_by(user.name) %>% summarise(
    max.points = total.points,
    points = sum(points),
    share.solved = points / max.points,
    num.hints = sum(num.hints),
    num.ps = n()
  ) 

  write.csv(tot.df,paste0(grade.dir,"/total_points.csv"),row.names = FALSE)
    
  invisible(list(sub.df=sub.df, tot.df=tot.df))
}


load.sub.with.name = function(file) {
  restore.point("load.sub.with.name")
  
  load(file)
  
  #stud.name = str.left.of(basename(file),"_")
  stud.name = str.left.of(str.right.of(basename(file),"----"),"--")
  
  Encoding(stud.name) <- "UTF-8"
  stud.name = mark_utf8(stud.name)
  sub$stud.name = correct.stud.names(stud.name)
  sub$log.df$stud.name = sub$stud.name
  sub$log.df$ps.name = sub$ps.name
  
  sub
}

load.subs = function(as=NULL, base.dir=NULL, files=NULL) {
  restore.point("load.subs")
  
  if (is.null(files)) {
    ass = as
    li = lapply(ass, function(as) {
      dir = paste0(base.dir,"/sub/",as)
      list.files(dir,pattern = glob2rx("*.sub"),full.names = TRUE)
    })
    files = do.call("c",li)
  }
  sub.li = lapply(files, load.sub.with.name)  
  sub.li
}
  

correct.stud.names = function(txt) {
  restore.point("correct.stud.names")
  
  txt = gsub("Ã¤","ä",txt,fixed=TRUE)
  txt = gsub("Ã¶","ö",txt,fixed=TRUE)
  
  txt
} 

#' Takes assignment ZIPs with all students' solutions 
#' and unpacks them into separate folders for each assignment
unpack.big.zips = function(base.dir, prefix="", postfix=".zip") {
  library(stringtools)
  
  
  # Extract big ZIP files
  # We have one big zip file per assignment
  # it contains all the zip files submitted 
  # by students for this assignment
  
  bzip.dir = paste0(base.dir,"/bigzip")
  bzips = list.files(bzip.dir)
  # Example file name: "WiWi39-SS15-Assignment ps_1a-33751.zip"
  
  # Extract names of assignments
  ass = str.between(bzips,prefix,postfix)
  
  as.base.dir = paste0(base.dir,"/sub")
  i = 1
  for (i in seq_along(ass)) {
    as = ass[i]
    as.dir = paste0(as.base.dir,"/",as)
    if (!dir.exists(as.dir))
      dir.create(as.dir, recursive=TRUE)
    
    bzip = paste0(bzip.dir,"/",bzips[i])
    unzip(bzip, exdir=as.dir)
    
    files = list.files(as.dir)
    library(tools)
    exts = file_ext(files)
    rows = exts != "sub"
    if (any(rows)) {
      str = paste0("\nThe following submissions have not the extension .sub and will be ignored:\n\n ", paste0(files[rows], collapse=",\n "))
      write.grade.log(str,"warning")
    }
  }
  return(list(ass=ass))
}

# not yet working
check.submitted.rmd = function(sub.df, run.dir=paste0(base.dir,"/run"), res.dir = paste0(base.dir,"/runzip"),base.dir = getwd(),copy.zip = TRUE,ask = TRUE, verbose=TRUE) {

  restore.point("check.submitted.rmd")
    
  if (NROW(sub.df) == 0) {
    cat("\nsub.df is empty")
    return(NULL)
  }

  if (ask) {
    cat("\nAre you sure that you want to run and check ", NROW(sub.df), "  submitted rmd files on your own computer? Be sure that the rmd files from your students don't contain malicious code. You can take a look the source files. Furthermore, I would strongly recommend to run the files  as a system user that has limited priviliges on your computer (not with a user that has administrator rights). The system user needs, however, read write access to the run.dir =",run.dir, " and the res.dir = ", res.dir,"\n")
    res = readline(prompt="Type y and press Enter if you want to continue: ")
    if (!tolower(res)=="y") {
      cat("\nCancelled solving .")
      return(invisible(NULL))
    }
  }

  i = 1  
  for (i in seq_along(sub.df$rmd.file)) {
    su = sub.df[i,]
    
    # copy files to run.dir
    file.copy(from = su$rmd.file,to = paste0(run.dir,"/",su$rmd.org.file),overwrite = TRUE)
    file.copy(from = su$log.file,to = paste0(run.dir,"/",su$log.org.file),overwrite = TRUE)
    file.copy(from = su$ups.file,to = paste0(run.dir,"/",su$ups.org.file),overwrite = TRUE)
    
    # check problem set
    setwd(run.dir)
    cat("\nCheck problemset ", su$ps.name, " of ", su$stud.name,"\n")

    new.sub = try(grade.ps(ps.name=su$ps.name, stud.path=run.dir, stud.short.file=su$rmd.org.file,reset = TRUE,user.name = su$user.name))
    
    if (!identical(oug$num.success, nug$num.success)) {
      write.grade.log(paste0("Recheck ", su$ps.name, " of ", su$stud.name,"\n",
        "num.success has changed from ", oug$num.success, " to ", nug$num.success))
    } else {
      cat("\nno change in num.success for ", su$ps.name, " of ", su$stud.name,"\n")
    }
    
    # Zip solution and copy to zip.dir
    zip.name = zip.solution(dir = run.dir, ask=FALSE)
    
    if (copy.zip) {
      new.zip = paste0(su$zip.dir,"/",basename(su$zip))
      file.copy(from=zip.name, to = new.zip, overwrite=TRUE)
      cat("\nZIP saved in ",new.zip)
    }
  }
 
}