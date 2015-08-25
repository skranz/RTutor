make.addons.list = function(addons="quiz") {
   li = lapply(addons, function(addon) {
     fun = paste0("rtutor.addon.",addon)
     do.call(fun,list())
   })
   names(li) = addons
  li
}