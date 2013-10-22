# Run this file to generate an empty problem set for students from your structure
# icecream_struc.r

library(restorepoint)
set.restore.point.options(deep.copy=FALSE)
library(whisker)
library(RTutor)  

setwd("C:/RTutor/problemsets/Intro")
ps = init.problem.set("intro","C:/RTutor/problemsets/Intro")
create.stud.ps(ps,ps.dir = "C:/RTutor/problemsets/Intro")
