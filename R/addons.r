make.addons.list = function(addons="quiz") {
   li = lapply(addons, function(addon) {
     fun = paste0("rtutor.addon.",addon)
     do.call(fun,list())
   })
   names(li) = addons
  li
}

process.checked.addon = function(rta, ps = get.ps(), ups=get.ups()) {
  ao.dt = ps$rps$ao.dt
  row = which(ao.dt$id == rta$id)
  
  
  if (rta$solved & !rta$was.solved) {
    award.name = ao.dt$award.name[row]
    if (!is.na(award.name)) {
      give.award(award.name, ps=ps)
      show.shiny.award(award.name = award.name)
    }
    rta$was.solved=TRUE
    
    if (!is.null(ups$aou)) {
      ups$aou$solved[row] = TRUE
      ups$aou$points[row] = max(rta$had.points, rta$points)
      if (!is.null(rta$score)) {
        if (!is.na(rta$score))
          ups$aou$score[row] = max(c(ups$aou$score[row], rta$score), na.rm=TRUE)
      }
      update.ups(ups)
    }
  }
  if (rta$points>rta$had.points) rta$had.points=rta$points

}