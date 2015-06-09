# Grade submitted files in .sub format

grade.log = as.environment(list(current=NULL, all=NULL))

grade.sol = function() {
  library(RTutor)
  
  base.dir = "D:/lehre/empIOUlm/ps2015"

  res = unpack.big.zips(base.dir)
  ass = res$ass  
  as = ass[1]
  
  sub.df = unpack.sub.zips(ass, base.dir=base.dir, do.unpack=TRUE)
  
  
  res = grade.using.ups(sub.df=sub.df)
  
  sum.df = res$sum.df
  sub.df = res$sub.df
  
  late.ups = filter(sub.df, ups.seconds.lag > 1)
  table(sub.df$ups.seconds.lag)
  
  unique(sub.df$as)
  df = filter(sub.df,share.solved <= 90)
  #check.submitted.rmd(df, base.dir=base.dir)
  
  sdf = group_by(sub.df, as) %>%
    mutate(
      num.test = sum(num.test),
      num.success = sum(num.success),
    num.hints = sum(num.hints),
    num.ps = length(as),
    min.ps.share.solved = min(share.solved),
    mean.ps.share.solved = mean(share.solved)
  )

    
  # Generate plots
  library(ggplot2)
  colnames(sub.df)
  ggplot(data=sub.df, aes(x=as, y=share.solved, group=stud.name)) + geom_line() + facet_wrap(~stud.name)
  ggplot(data=sub.df, aes(x=as, y=time.rank, group=stud.name)) + geom_line() + facet_wrap(~stud.name)
 
  
  ggplot(data=sub.df, aes(x=as, y=share.solved, group=stud.name)) + geom_line() + facet_wrap(~stud.name) + geom_line(mapping =  aes(x=as, y=perc.time.rank, group=stud.name),color="blue")
  
  log.df = import.logs(sub.df)
}

import.logs = function(sub.df) {
  li = lapply(seq_along(sub.df$log.file), function(i) {
    df = import.log(sub.df$log.file[[i]])
    df$as = sub.df$as[[i]]
    df$stud.name = sub.df$stud.name[[i]]
    df$stud.id = sub.df$stud.id[[i]]
    df
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
}

grade.sub.df = function(sub.df) {
  restore.point("grade.all.using.ups")
  
  # Grade all rows of sub.df separately
  li = lapply(1:NROW(sub.df), function(i) {
    grd = load.grd(sub.df$grd.file)
    grd$total
  })
  df = bind_rows(li)
  res = cbind(sub.df, df)

  res = mutate(group_by(res, as),
    time.rank = rank(finished.time, ties.method="min"),
    perc.time.rank = 100*time.rank / length(time.rank),
    
    success.rank = rank(num.success, ties.method="min"),
    hints.rank = rank(num.success, ties.method="min")
  )
  
  sdf = summarise(group_by(res, stud.name),
    num.test = sum(num.test),
    num.success = sum(num.success),
    num.hints = sum(num.hints),
    num.ps = length(as),
    median.time.rank = median(as.numeric(time.rank)),
    min.ps.share.solved = min(share.solved),
    mean.ps.share.solved = mean(share.solved)
  )
  sdf = arrange(ungroup(sdf), -num.success, num.hints)
  sdf$share.solved = sdf$num.success / max(sdf$num.test)
  
  list(sub.df=ungroup(res), sum.df=ungroup(sdf))
}


load.grd.with.names = function(file) {
  grd = load.grd(file)
  stud.name = str.left.of(file,"_")
  Encoding(stud.names) <- "UTF-8"
  grd$stud.name = stud.name
  
  grd
}

load.grds = function(as=NULL, base.dir=NULL, files=NULL) {
  if (is.null(files) & length(as)>1) {
    ass = as
    li = lapply(ass, function(as) {
      dir = paste0(base.dir,"/sub/",as)
      list.files(dir,pattern = glob2rx("*.sub"),full.names = TRUE)
    })
    files = do.call("c",li)
  }
  grd.li = lapply(files, load.grd.with.name)  
  grd.li
}
  

import.subs = function(as=NULL, base.dir=NULL, files=NULL, grd.li = NULL) {
  restore.point("import.subs")
  
  if (is.null(grd.li)) {
    grd.li = load.grds(as=as, base.dir=base.dir, files=files)
  }

  # Grade all rows of sub.df separately
  li = lapply(grd.li, function(grd) {
    res = grd$total
    res$stud.name = grd$stud.name
    res
  })
  sub.df = bind_rows(li)

  sub.df = mutate(group_by(sub.df, ps.name),
    time.rank = rank(finished.time, ties.method="min"),
    perc.time.rank = 100*time.rank / length(time.rank),
    
    success.rank = rank(num.success, ties.method="min"),
    hints.rank = rank(num.success, ties.method="min")
  )
  
  sdf = summarise(group_by(res, stud.name),
    num.test = sum(num.test),
    num.success = sum(num.success),
    num.hints = sum(num.hints),
    num.ps = length(as),
    median.time.rank = median(as.numeric(time.rank)),
    min.ps.share.solved = min(share.solved),
    mean.ps.share.solved = mean(share.solved)
  )
  sdf = arrange(ungroup(sdf), -num.success, num.hints)
  sdf$share.solved = sdf$num.success / max(sdf$num.test)
  
  sum.df = ungroup(sdf)
  
  
  # Grade all rows of sub.df separately
  li = lapply(grd.li, function(grd) {
    res = grd$by.chunk
    res$stud.name = grd$stud.name
    res
  })
  by.chunk = bind_rows(li)

  
  # Grade all rows of sub.df separately
  li = lapply(grd.li, function(grd) {
    res = grd$by.ex
    res$stud.name = grd$stud.name
    res
  })
  by.ex = bind_rows(li)
  
  
  as.environment(list(grd.li = grd.li, sub.df=sub.df, sum.df=sum.df, by.chunk=by.chunk, by.ex=by.ex))

  
}

#' Takes assignment ZIPs with all students' solutions 
#' and unpacks them into separate folders for each assignment
unpack.big.zips = function(base.dir) {
  library(stringtools)
  
  
  # Extract big ZIP files
  # We have one big zip file per assignment
  # it contains all the zip files submitted 
  # by students for this assignment
  
  bzip.dir = paste0(base.dir,"/bigzip")
  bzips = list.files(bzip.dir)
  # Example file name: "WiWi39-SS15-Assignment ps_1a-33751.zip"
  
  # Extract names of assignments
  ass = str.between(bzips,"-Assignment ","-")
  
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
      str = paste0("The following submissions have not the extension .sub and will be ignored:\n ", paste0(files[rows], collapse=",\n "))
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

    new.grd = try(grade.ps(ps.name=su$ps.name, stud.path=run.dir, stud.short.file=su$rmd.org.file,reset = TRUE,user.name = su$user.name))
    
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