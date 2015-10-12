
#' Used in a solution file. Give an award to a student who made it so far
#' @export
give.award = function(award.name, award = ps$rps$awards[[award.name]] , user = get.user(), ps=get.ps()) {
  restore.point("give.award")

  if (has.award(award.name, user))
    return(TRUE)
  
  message(paste0('
**********************************************************
* Congrats, you earned the award "', award.name, '"
**********************************************************

PS: awards() shows all your awards
'))
  award$granted.in = ps$name
  user$awards[[award.name]] = award 
  update.user()
  show.award(award)
  return(TRUE)
}

show.award = function(award, award.name = award$award.name, html=award$html, txt=award$txt, ps=get.ps()) {
  if (isTRUE(ps$is.shiny)) return()
  if (!is.null(html)) {
    htmlFile <- tempfile(fileext=".html")
    writeLines(html,htmlFile )
    if (require(rstudioapi)) {
      rstudioapi::viewer(htmlFile)
    } else {
      cat(paste0("\n*** ",award.name, " ***\n", txt," (awarded in ", award$granted.in, ")\n"))
    }
  } else {
    cat(paste0("\n*** ",award.name, " ***\n", txt," (awarded in ", award$granted.in, ")\n"))
  }
}

get.award.ui.id = function(award.name,ps=get.ps()) {
  award.ind = which(names(ps$rps$awards) == award.name)
  paste0("awardUI__",award.ind)
}

show.shiny.awards = function(ps=get.ps(), user=get.user()) {
  awards = intersect(names(ps$rps$awards), names(user$awards))
  for (award.name in awards) {
    show.shiny.award(award.name)
  }
}

show.shiny.award = function(award.name) {
  html = shiny.award.ui(award.name=award.name)
  id = get.award.ui.id(award.name)
  setUI(id, html)
}

shiny.award.ui = function(award.name, ps=get.ps(), user = get.user()) {
  restore.point("shiny.award.ui")
  
  html = user$awards[[award.name]]$html
  if (is.null(html)) return(NULL)
  restore.point("shiny.award.ui")
  
  award.ind = which(names(user$awards) == award.name)[1] 
  
  collapseId = paste0("collapse_award_",award.ind)
  collapsePanelId = paste0("collapse_panel_award_",award.ind) 
  ahtml = bsCollapse(open = NULL, id = collapseId,
    bsCollapsePanel(paste0("Award: ",award.name),value=collapsePanelId, HTML(html) )
  )
  # GOLD: #DFC463
  txt = gsub(
    '<div class="panel-heading"',
    '<div class="panel-heading" style="background-color: #DFC463;box-shadow: 2px 2px 2px #888888;"',
    as.character(ahtml), fixed=TRUE
  )
  return(HTML(txt))  
  ahtml
}


#' Show all your awards
#' @export
awards = function(user = get.user(), as.html=FALSE, details=TRUE) {
  
  if (!as.html) {
    cat(paste0("Hi ",user$name,", you have earned ", length(user$awards)," awards:\n"))
    if (!details) {
      print(names(user$awards))
    } else {
      for (ad in user$awards) {
        cat(paste0("\n*** ",ad$award.name, " ***\n", ad$txt," (awarded in ", ad$granted.in, ")\n"))
      }
    }
  } else {
    if (!details) {
      txt = paste0("<h4>",names(user$awards),"...</h4>")
      
    } else {
#       li = lapply(user$awards, function(ad) {
#         bsCollapsePanel(title=ad$award.name,HTML(ad$html))
#       })
#       names(li) = NULL
#       pa = do.call(bsCollapse,li)
#       txt = as.character(pa)  
      
      txt = sapply(user$awards, function(ad) {
        paste0(ad$html)
      })
    }
    txt = c(paste0("<h3>You have earned ", length(user$awards)," awards</h3>"),txt)

    txt = HTML(paste0(txt, collapse="\n"))
    txt
  }
}

has.award = function(award.name,user=get.user()) {
  award.name %in% names(user$awards)
}

#' @export
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

awards.details = function() {S
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