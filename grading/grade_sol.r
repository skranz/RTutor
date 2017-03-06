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


grade.using.ups = function(sub.df) {
  restore.point("grade.all.using.ups")
  
  # Grade all rows of sub.df separately
  li = lapply(1:NROW(sub.df), function(i) {
    grade.ups(stud.id=sub.df$stud.id[[i]],ups.file = sub.df$ups.file[[i]])
  })
  df = bind_rows(li)
  res = left_join(sub.df, df, by="stud.id")

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

grade.ups = function(stud.id=NULL, ups=NULL, ups.file = NULL) {
  restore.point("grade.ups")
  if (is.null(ups)) {
    load(file=ups.file)
  }
  df = summarise(as.data.frame(ups$tdt),
    num.test = length(test.e.ind),
    num.success = sum(success),
    share.solved=round(sum(success)/num.test*100),
    num.hints = sum(num.hint),
    finished.time = max(success.date)
  )
  df$stud.id = stud.id
  df
}


summarise.ups = function(ups = NULL, ups.file=NULL) {
  if (is.null(ups))
    load(file=ups.file)

  ex.df = summarise(group_by(as.data.frame(ups$tdt),ex.ind),
    num.test = length(test.e.ind),
    num.success = sum(success),
    percentage.solved=round(sum(success)/num.test*100),
    hints = -sum(num.hint),
    end.time = max(success.date)
  )
  
  ps.df = summarise(as.data.frame(ups$tdt),
    num.test = length(test.e.ind),
    num.success = sum(success),
    percentage.solved=round(sum(success)/num.test*100),
    hints = -sum(num.hint),
    end.time = max(success.date)
  )
  
}

#' Unpack the zips of the individual submissions 
#' store meta data in a sub.csv and return the data.frame 
unpack.sub.zips = function(as, as.dir = paste0(base.dir,"/stud/",as), base.dir, do.unpack=TRUE) {
 
  if (length(as)>1) {
    restore.point("unpack.sub.zips.outer")
    ass = as
    li = lapply(ass, function(as) {
      unpack.sub.zips(as=as, base.dir=base.dir)
    })
    sub.df = bind_rows(li)
    return(sub.df)
  }
  restore.point("unpack.sub.zips.inner")
  
  zips.dir = paste0(as.dir,"/zips")


  zips = c(
    list.files(path = zips.dir, pattern=glob2rx("*.zip")),
    list.files(path = zips.dir, pattern=glob2rx("*.ZIP")),
    list.files(path = zips.dir, pattern=glob2rx("*.Zip"))
  )
  
  stud.names = str.left.of(zips,"_")
  Encoding(stud.names) <- "UTF-8"
  
  stud.id = str.between(zips,"_","_assignsubmission") 
  
  sub.df = data.frame(as=as, stud.name=stud.names, stud.id=stud.id, zip=zips, zip.dir = zips.dir)
  

  zip = zips[1]
  li = lapply(seq_along(zips), function(i) {
    restore.point("inner.unzip")
    zip = zips[i]

    rdf = unzip(paste0(zips.dir,"/",zip), list=TRUE)
  
    fdf = dplyr::data_frame(as=as,stud.id=sub.df$stud.id[i], stud.name=sub.df$stud.name[i],ext =tolower(file_ext(rdf$Name)), file=rdf$Name, size=rdf$Length, date = rdf$Date)
    fdf
    
    
  })
  fdf = bind_rows(li)
  fdf$base = str.left.of(fdf$file,paste0(".", fdf$ext))
  
  sdf = summarise(group_by(fdf,stud.id),
    has.ups = sum(ext=="ups"),
    has.rmd = sum(ext=="rmd"),
    has.log = sum(ext=="log"),
    ups.org.file = first(file[ext=="ups"]),
    rmd.org.file = first(file[ext=="rmd"]),
    log.org.file = first(file[ext=="log"]),
    ups.date = first(date[ext=="ups"]),
    rmd.date = first(date[ext=="rmd"]),
    ups.seconds.lag  = rmd.date- ups.date, 
    ps.name = first(str.left.of(file[ext=="log"],".log")),
    user.name = first(str.trim(str.left.of(base[ext=="ups"],"_")))
  )
  sub.df = left_join(sub.df, sdf, by="stud.id")

  # Do the unzipping
  temp.dir = paste0(as.dir,"/temp") 
  types = c("ups","rmd","log")
  for (type in types) {
    type.dir = paste0(as.dir,"/",type)
    if (!dir.exists(type.dir))
      dir.create(type.dir)
    
    sub.df[[paste0(type,".file")]] = 
      paste0(type.dir,"/",sub.df$stud.name,"___", sub.df[[paste0(type,".org.file")]])
  }
    
  i = 1
  if (do.unpack) {
    for (i in seq_along(zips)) {
      zip = zips[i]
      # unzip to temporal folder
      unzip(paste0(zips.dir,"/",zip), exdir = temp.dir)
      for (type in types) {
        org.file = paste0(sub.df[i,paste0(type,".org.file")])
        new.file =  paste0(sub.df[i,paste0(type,".file")])
      
        file.copy(from =  paste0(temp.dir,"/",org.file),
                    to =  paste0(new.file),
                  overwrite=TRUE)
        #file.remove(paste0(temp.dir,"/",file))
      }
    }
  }
  try(write.csv(sub.df,file = paste0(as.dir,"/sub.csv"),row.names = FALSE))
  
  return(sub.df)
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
  
  as.base.dir = paste0(base.dir,"/stud")
  i = 1
  for (i in seq_along(ass)) {
    as = ass[i]
    as.dir = paste0(as.base.dir,"/",as,"/zips")
    if (!dir.exists(as.dir))
      dir.create(as.dir, recursive=TRUE)
    
    bzip = paste0(bzip.dir,"/",bzips[i])
    unzip(bzip, exdir=as.dir)
    
    files = list.files(as.dir)
    library(tools)
    exts = file_ext(files)
    rows = exts != "zip"
    if (any(rows)) {
      str = paste0("The following submissions have not the extension .zip and will be ignored:\n ", paste0(files[rows], collapse=",\n "))
      write.grade.log(str,"warning")
    }
  }
  return(list(ass=ass))
}

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

    res = try(check.problem.set(ps.name=su$ps.name, stud.path=run.dir, stud.short.file=su$rmd.org.file,reset = TRUE,user.name = su$user.name))
    
    new.ups = get.ups()
    
    oug = grade.ups(stud.id = su$stud.id, ups.file = su$ups.file)
    nug = grade.ups(stud.id = su$stud.id, ups=new.ups)
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