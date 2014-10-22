# Store user specific information about the problem set solution

# Store in a data.frame for each test:

# i) When first run
# ii) How often failed before successful
# iii) How often hint() called before successful
# iv) When first successful

# clear.user()

init.ups = function() {
  ps = get.ps()
  user=get.user()
  tdt = mutate(as.data.frame(ps$tdt), first.call.date=as.POSIXct(NA), num.failed=0, num.hint=0, success=FALSE, success.date=as.POSIXct(NA))
  ups = as.environment(list(ps.name=ps$name, user.name=user$name, tdt=tdt))
  set.ups(ups)
  save.ups()
  ups
}

get.ups = function() {
  ps =get.ps()
  if (is.null(ps))
    return(NULL)
  user=get.user()
  
  ups <- NULL
  try(ups<-get(".__rtutor_ups",.GlobalEnv), silent=TRUE)
  if (!is.null(ups)) {
    if (ups$ps.name != ps$name | ups$user.name != user$name)
      ups = NULL
  }
  if (is.null(ups))
    ups = load.ups()
  ups
}

load.ups = function() {
  ps = get.ps()
  user = get.user()
  
  dir = get.ps()$stud.path
  file = paste0(dir,"/",user$name,"_",ps$name,".ups")
  
  if (!file.exists(file)) {
    ups = init.ups()
    save.ups()
  } else {
    load(file=file)
    assign(".__rtutor_ups",ups,.GlobalEnv)  
  }
  return(invisible(ups))
}

set.ups = function(ups) {
  assign(".__rtutor_ups",ups,.GlobalEnv)    
}

save.ups = function() {
  ps = get.ps()
  user = get.user()
  dir = get.ps()$stud.path
  file = paste0(dir,"/",user$name,"_",ps$name,".ups")
  ups = get.ups()
  suppressWarnings(save(ups,file=file))
  assign(".__rtutor_ups",ups,.GlobalEnv)  
  return(invisible(ups))
}

#' Shows your progress
#' @export
stats = function() {
  ps = get.ps()
  if (is.null(ps)) {
    display("No problem set specified. You must check a problem before you can see your stats.")
    return(invisible())
  }
    
  user = get.user()
  ups = get.ups()
  
  res = summarise(group_by(as.data.frame(ups$tdt),ex.ind),
    num.test = length(test.e.ind),
    percentage.solved=round(sum(success)/num.test*100),
    hints = -sum(num.hint),
    end.time = max(success.date)
  )
  res$completed = ifelse(is.na(res$end.time), "",strftime(res$end.time, format="%H:%M %d.%m."))
  res$ex.name = ps$edt$ex.name
  sr = dplyr::select(res,ex.name,percentage.solved,hints, completed)
  colnames(sr) = c("Ex","solved (%)","hints","completed")
  rownames(sr) = NULL
  display(user$name, "'s stats for problem set ",ps$name,":\n")
  print(as.data.frame(sr))  
}


# remove old ups files when new problem set structure is generated 
remove.ups = function(ps.name = get.ps()$name) {
  set.ups(NULL)

  files = list.files()
  files = files[str.ends.with(files,paste0("_",ps.name,".ups"))]
  if (length(files)>0) {
    file.remove(files)
  }
  set.ups(NULL)
}

