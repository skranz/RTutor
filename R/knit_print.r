
rtutor.knit_print.data.frame = function(x, inline = FALSE, MAX.ROW=22, ...) {
  if (NROW(x)>MAX.ROW) {
    rows = 1:MAX.ROW
    h1 = RTutor:::html.table(x[rows,],...)
    html = c(h1, as.character(p(paste0("... ", NROW(x)-MAX.ROW, " rows not shown  ..."))))

  } else {
    html = RTutor:::html.table(x,...)
  }
  asis_output(html)
}
