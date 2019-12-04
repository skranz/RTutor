stats = read.csv("testex/stats.csv")
if (NROW(stats)>0) {
  cat("\nStats: ")
  print(stats)
} else {
  cat("No record changes or errors in examples.")
}

cat("\n")
txt = readLines("testex/log.Rmd")
cat(paste0(txt, collapse="\n"))
