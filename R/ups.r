# Store user specific information about the problem set solution

# Store in a data.frame for each test:

# i) When first run
# ii) How often failed before successful
# iii) How often hint() called before successful
# iv) When first successful

# clear.user()


# clear.user()
get.user = function(user.name = NULL, dir = get.ps()$user.dir) {
  restore.point("get.user")
  if (!exists(".__rtutor_user",.GlobalEnv)) {
    file = paste0(dir,"/current_user.Ruser")
    if (file.exists(file)) {
      user = load.user(dir)
      if (is.null(user.name) | identical(user$name, user.name))
        return(user)
    }
    if (is.null(user.name))
      user.name = "GUEST"
    init.user(user.name)
    save.user()
  }
  user = get(".__rtutor_user",.GlobalEnv)
  if (!identical(user$name, user.name) & !is.null(user.name)) {
    user = init.user(user.name)
    save.user()    
  }
  return(user)
}


init.user = function(user.name="GUEST") {
  user = as.environment(list(name=user.name, awards = list()))
  assign(".__rtutor_user.name",user.name,.GlobalEnv)  
  assign(".__rtutor_user",user,.GlobalEnv)
  user
}

update.user = function(user=get.user()) {
  save.user(user)
}

load.user = function(dir = get.ps()$user.dir) {
  file = paste0(dir,"/current_user.Ruser")
  load(file=file)
  assign(".__rtutor_user.name",user$name,.GlobalEnv)  
  assign(".__rtutor_user",user,.GlobalEnv)  
  return(invisible(user))
}

save.user = function(user=get.user(user.name), user.name = get.user.name(), dir = get.ps()$user.dir, ps = get.ps()) {
  if (isTRUE(ps$save.nothing))
    return()
  
  file = paste0(dir,"/current_user.Ruser")
  save(user, file=file)
  # Backup
  file = paste0(dir,"/user_",user.name,".Ruser")
  save(user, file=file)
}


init.ups = function() {
  ps = get.ps()
  user=get.user()
  
  
  ups.tdt = !is.false(ps$ups.tdt)
  
  cdt = ps$cdt
  
  # Store chunk results
  cu = data_frame(solved=rep(FALSE, NROW(cdt)), first.check.date=as.POSIXct(NA),  num.failed=0, num.hint=0, solved.date=as.POSIXct(NA))
  
  # Store add-on results
  ao.dt = ps$rps$ao.dt
  if (NROW(ao.dt)>0) {
    aou = data_frame(solved=rep(FALSE,NROW(ao.dt)) , first.check.date=as.POSIXct(NA),  num.failed=0, num.hint=0, solved.date=as.POSIXct(NA), score=0)
  } else {
    aou = NULL
  }
  
  got.award = rep(FALSE, length(ps$rps$awards))

  
  if (ups.tdt)  {
    tdt = mutate(as.data.frame(ps$tdt), first.call.date=as.POSIXct(NA), num.failed=0, num.hint=0, success=FALSE, success.date=as.POSIXct(NA))
  } else {
    tdt = NULL
  }
  ups = as.environment(list(ps.name=ps$name, user.name=user$name, cu=cu, aou=aou, got.award = got.award, tdt=tdt))

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
  if (is.null(ups)) {
    ups = load.ups()
    
    # old version of ups
    if (is.null(ups$cu)) {
      new.ups = init.ups()
      new.ups$tdt = ups$tdt
      ups = new.ups
      set.ups(ups)
    }
    
  }
  ups
}

load.ups = function() {
  ps = get.ps()
  user = get.user()
  
  dir = get.ps()$ups.dir
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


update.ups = function(ups = get.ups(), ps = get.ps()) {
  save.ups(ups=ups, ps=ps)
}

save.ups = function(ups = get.ups(), ps=get.ps()) {
  if (isTRUE(ps$save.nothing)) return()
  
  ups$chunk.ind = ps$chunk.ind
  
  #cat("\nups saved with chunk.ind = ", ups$chunk.ind)
  
  user = get.user()
  dir = get.ps()$ups.dir
  file = paste0(dir,"/",user$name,"_",ps$name,".ups")

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
  res$ex.name = ps$edt$ex.name[res$ex.ind]
  sr = dplyr::select(res,ex.name,percentage.solved,hints, completed)
  colnames(sr) = c("Ex","solved (%)","hints","completed")
  rownames(sr) = NULL
  display(user$name, "'s stats for problem set ",ps$name,":\n")
  print(as.data.frame(sr))  
}


# remove old ups files when new problem set structure is generated 
remove.ups = function(ps.name = get.ps()$name, dir = get.ps()$ups.dir) {
  set.ups(NULL)

  if (is.null(dir)) dir =getwd()
  
  files = list.files(path = dir,full.names = TRUE)
  files = files[str.ends.with(files,paste0("_",ps.name,".ups"))]
  if (length(files)>0) {
    file.remove(files)
  }
  set.ups(NULL)
}

