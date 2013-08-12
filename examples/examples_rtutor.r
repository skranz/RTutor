examples = function() {  
  library(restorepoint); set.restore.point.options(deep.copy=FALSE)
  library(whisker)
  library(RTutor)  
  ps = init.problem.set("examples","C:/libraries/RTutor/RTutor/problemsets")
  ps
  create.stud.ps(ps,ps.dir = "C:/libraries/RTutor/RTutor/problemsets")
  
}
