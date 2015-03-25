# Structure of a log entry

# A csv file

# date (date with time)
# user.name
# ps.name
# chunk.ind
# chunk.name
# type: check, hint, run, start, load, import
# success: TRUE, FALSE, NA (has a check passed)
# e.ind:
# test.ind:
# seed: a random seed
# chunk.code: replace " with §
# run.code: replace " with §

log.event = function(type,ok=TRUE, ..., user.name=get.user.name(),ps=get.ps()) {

  if (!require(jsonlite))
    return()
  
  if (is.false(ps$log.events))
    return()
  
  args = list(...)
  
  restore.point("log.event")
  
  time = Sys.time()
  li = c(list(
      type=type,
      time=format(time,"%Y-%m-%d %H:%M:%S"),
      user = user.name,
      umph = as.numeric(time),
      ok=ok
    ),args)
  
  log.file = ps$log.file

  json = paste0(jsonlite::toJSON(li),"\n,")
  try({
    con = file(log.file,open="at")
    writeLines(json, con)
    close(con) 
  })
}

