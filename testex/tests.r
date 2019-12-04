tibble(x=1:5,y=2:6)


# 1. We create the problem set
setwd("testex/ps")
library(RTutor)
ps.name = "fill_in_test"; sol.file = paste0(ps.name,"_sol.Rmd")
libs = c("dplyr") 
create.ps(sol.file=sol.file, ps.name=ps.name,libs=libs, rps.has.sol=TRUE,user.name = "test", addons="quiz")


# 2. Check sample solution should run without error
check.problem.set('fill_in_test', getwd(),"fill_in_test_sample_solution.Rmd", reset=FALSE, from.knitr = FALSE, do.check=TRUE)

hint()
stats()
awards()
make.submission(from.knitr = FALSE)

# 3. Check empty file

check.problem.set('fill_in_test', getwd(),"fill_in_test.Rmd" , reset=FALSE,from.knitr = FALSE, do.check=TRUE)

# Check if hint works
hint()


# Go back to project directory
setwd("../..")

