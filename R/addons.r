make.addons.list = function(addons="quiz") {
   li = lapply(addons, function(addon) {
     fun = paste0("rtutor.addon.",addon)
     do.call(fun,list())
   })
   names(li) = addons
  li
}

process.checked.addon = function(rta, ps = get.ps()) {
  ao.dt = ps$rps$ao.dt
  row = which(ao.dt$id == rta$id)
  
  
  if (rta$solved & !rta$was.solved) {
    award.name = ao.dt$award.name[row]
    if (!is.na(award.name)) {
      give.award(award.name, ps=ps)
      show.shiny.award(award.name = award.name)
    }
    rta$was.solved=TRUE
  }
  if (rta$points>rta$had.points) rta$had.points=rta$points

}