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
  i = 1
  li = lapply(seq_along(ps$ex), function(i) {
    ex = ps$ex[[i]]
    n = length(ex$tests)
    if (n ==0) {
      return(NULL)
    }
    #test.name = sapply(ex$tests,name.of.call)
    #test.arg1 = sapply(ex$tests, function(e) {
    #  args = args.of.call(e)
    #  if (length(args)>0)
    #    return(deparse1(args[[1]]))
    #  return("")
    #})
    data.frame(ex.ind = i, test.ind=1:n,  first.call.date=as.POSIXct(NA), num.failed=0, num.hint=0, num.awards=0, success=FALSE, success.date=as.POSIXct(NA))
  })
  names(li) = names(ps$ex)
  ups = as.environment(list(ps.name=ps$name, user.name=user$name, li=li))
  assign(".__rtutor_ups",ups,.GlobalEnv)
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
  save(ups,file=file)
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
  
  res = lapply(seq_along(ups$li), function(i) {
    d = ups$li[[i]]
    num.test = NROW(d)
    percentage.solved=round(sum(d$success)/num.test*100)
    hints = -sum(d$num.hint)
    awards = sum(d$num.awards)
    start.time = end.time = NA
    completed = ""
    if (all(!is.na(d$success.date))) {
      end.time = max(d$success.date)
      completed = strftime(end.time, format="%H:%M %d.%m.")
    }
    
    data.frame(exercise = names(ps$ex)[i], solved=paste0(percentage.solved," %"), hints=hints, awards=awards, completed = completed)
  })
  d = do.call("rbind",res)
  display(user$name, "'s stats for problem set ",ps$name,":\n\n")
  print(as.data.frame(d))  
}
