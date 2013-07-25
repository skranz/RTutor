examples = function() {  
  library(restorepoint)
  library(RTutor)
  
  set.problemset.structure.path("C:/libraries/RTutor/RTutor/problemsets/")
  set.problemset.user.path("C:/libraries/RTutor/RTutor/problemsets/")
  
  ps = load.problem.set("examples")
  write.ps.skeleton(ps)
  
}
