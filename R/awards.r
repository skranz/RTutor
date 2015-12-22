
#' Used in a solution file. Give an award to a student who made it so far
#' @export
give.award = function(award.name, award = ps$rps$awards[[award.name]] , ups = get.ups(), ps=get.ps()) {
  restore.point("give.award")

  if (has.award(award.name, ups))
    return(TRUE)
  
  message(paste0('
**********************************************************
* Congrats, you earned the award "', award.name, '"
**********************************************************

PS: awards() shows all your awards
'))
  ups$awards[[award.name]] = list(award.name=award.name) 
  update.ups()
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
      cat(paste0("\n*** ",award.name, " ***\n", txt,"\n"))
    }
  } else {
    cat(paste0("\n*** ",award.name, " ***\n", txt,"\n"))
  }
}

get.award.ui.id = function(award.name,ps=get.ps()) {
  award.ind = which(names(ps$rps$awards) == award.name)
  paste0("awardUI__",award.ind)
}

show.shiny.awards = function(ps=get.ps(), ups=get.ups()) {
  awards = intersect(names(ps$rps$awards), names(ups$awards))
  for (award.name in awards) {
    show.shiny.award(award.name)
  }
}

show.shiny.award = function(award.name) {
  html = shiny.award.ui(award.name=award.name)
  id = get.award.ui.id(award.name)
  setUI(id, html)
}

shiny.award.ui = function(award.name, ps=get.ps(), ups = get.ups()) {
  restore.point("shiny.award.ui")
  
  award = ps$rps$awards[[award.name]]
  html = award$html
  
  if (is.null(html)) return(NULL)
  restore.point("shiny.award.ui")
  
  award.ind = which(names(ups$awards) == award.name)[1] 
  
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
awards = function(ups = get.ups(), as.html=FALSE, details=TRUE, ps = get.ps()) {
  restore.point("awards")
  
  awards = ps$rps$awards[names(ups$awards)]
  if (!as.html) {
    cat(paste0("Hi ",ups$user.name,", you have earned ", length(ups$awards)," awards:\n"))
    if (!details) {
      print(names(ups$awards))
    } else {
      for (ad in awards) {
        cat(paste0("\n*** ",ad$award.name, " ***\n", ad$txt,"\n"))
      }
    }
  } else {
    if (!details) {
      txt = paste0("<h4>",names(ups$awards),"...</h4>")
      
    } else {
      txt = sapply(awards, function(ad) {
        paste0(ad$html)
      })
    }
    txt = c(paste0("<h3>You have earned ", length(ups$awards)," awards</h3>"),txt)

    txt = HTML(paste0(txt, collapse="\n"))
    txt
  }
}

has.award = function(award.name,ups=get.ups()) {
  award.name %in% names(ups$awards)
}
