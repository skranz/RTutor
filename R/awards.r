

tell.story = function(text) {
  
}

give.prize = function(prize.text=NULL, prize.code=NULL,...) {
  
}

give.award = function(award, prize.text = NULL, prize.code = NULL, user = get.user()) {
  if (has.award(award, user))
    return(TRUE)
  
  message(paste0('
**********************************************************
* Congrats, you earned the award "', award, '"
**********************************************************

PS: awards() shows all your awards
'))
  user$awards = c(user$awards,award)
  user$awards.granted.in[[award]] = paste0(get.ps()$name, " ", get.ex()$name)
  update.user()
  return(TRUE)
}

awards = function(user = get.user(), details=TRUE) {
  cat(paste0("Hi ",user$name,", you have earned ", length(user$awards)," awards:\n"))
  if (!details) {
    print(user$awards)
  } else {
    ad = awards.details()[user$awards]
    for (i in seq_along(ad)) {
      cat(paste0("\n*** ",user$awards[i], " ***\n", ad[[i]]," (Awarded in ", user$awards.granted.in[user$awards[[i]]], ")\n"))
    }
  }
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
  user = as.environment(list(name=user.name, awards = NULL, awards.granted.in = list()))
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

awards.details = function() {
  list(
"American Economic Review Data Grabber" = "You downloaded data from an AER article from the AER website and imported (at least some of) the data into R. There are a lot of really good articles with available data on the AER website. This helps you to reproduce and understand the research and to test your own ideas!",   
    
"Column Identificator" = "You correctly identified the meanings of columns from a real world data set.",

"Random Shocker" = "You have simulated a vector of random shocks for a regression model.",

"Price Peaker" = "You have simulated prices that are correlated with peak- off-peak hours.",


"Regression Runner" = "You have run a linear regression in R with the command lm.",

"Confidence Shaker" = "You have computed a confidence interval for an OLS regression with an endogenous variable and saw that the confidence interval is rubbish if you have an endogeniety problem.",

"Equilibrium Calculator" = "You have derived the correct formula for equilibrium prices of a competitive market and simulated these prices in R.",

"Creator of Endogeniety" = "You have simulated an endogeonus explanatory variable in a regression model.",

"Omitted Variable Bias" = "You have run an OLS regression where you omitted (left out) an explanatory variable that is correlated with another explanatory variable. Such a story cannot end well... The omitted variable becomes part of the error term, the error term is then correlated with an explanatory variable, you get an endogeniety problem and your OLS estimate is inconsistent and biased!",

"Playing the Wind" = "Musicians may play a piano, violine or trumpet, but you used the wind as instrument!",

"Two stages for Consistency" = "You consistently estimated a demand equation with an endogenous price via a manual implementation of the 2SLS (two stage least squares) procedure!",

"Consistent IV" = "You consistently estimated a demand function with an endogenous price with an instrumental variable estimator using the ivreg function in the package AER. Better use ivreg instead of manual 2SLS (two stage least squares) since it will have correct standard errors.",

"An Inconsistent Demand" = "You have estimated via OLS a demand function with an endogenous price, which was simulated with a simple market model. And guess what? You got an inconsistent estimate!"
    )
}