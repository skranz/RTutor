give.award = function(award, user = get.user()) {
  if (has.award(award, user))
    return(TRUE)
  
  message(paste0('
*************************************************
* Congrats, you earned the award "', award, '"
*************************************************
'))
  user$awards = c(user$awards,award)
  update.user()
  return(TRUE)
}

awards = function(user = get.user()) {
  cat(paste0("Hi ",user$name,", you have earned the following awards:\n"))
  print(user$awards)
}

has.award = function(award,user=get.user()) {
  award %in% user$awards
}


get.user.name = function() {
  if (!exists(".__rtutor_user.name",.GlobalEnv)) {
    return("GUEST")
  }
  get(".__rtutor_user.name",.GlobalEnv)  
}
clear.user = function(dir = get.ps()$stud.path) {
  suppressWarnings(rm(".__rtutor_user.name",envir=.GlobalEnv))
  suppressWarnings(rm(".__rtutor_user",envir=.GlobalEnv))
  file = paste0(dir,"/rtutor_user.RData")
  if (file.exists(file))
    file.remove(file)
  
}
# clear.user()
get.user = function(user.name = NULL, dir = get.ps()$stud.path) {
  restore.point("get.user")
  if (!exists(".__rtutor_user",.GlobalEnv)) {
    file = paste0(dir,"/rtutor_user.RData")
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
  user = as.environment(list(name=user.name, awards = NULL))
  assign(".__rtutor_user.name",user.name,.GlobalEnv)  
  assign(".__rtutor_user",user,.GlobalEnv)
  user
}

update.user = function(user=get.user()) {
  save.user(user)
}

load.user = function(dir = get.ps()$stud.path) {
  file = paste0(dir,"/rtutor_user.Rdata")
  load(file=file)
  assign(".__rtutor_user.name",user$name,.GlobalEnv)  
  assign(".__rtutor_user",user,.GlobalEnv)  
  return(invisible(user))
}

save.user = function(user=get.user(user.name), user.name = get.user.name(), dir = get.ps()$stud.path) {
  file = paste0(dir,"/rtutor_user.RData")
  save(user, file=file)
  # Backup
  file = paste0(dir,"/rtutor_user_",user.name,".RData")
  save(user, file=file)
}