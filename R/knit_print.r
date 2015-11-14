
rtutor.knit_print.data.frame = function(x, inline = FALSE, MAX.ROW=30, ...) {
  if (NROW(x)>MAX.ROW) {
    # n = floor(MAX.ROW / 2)
    # rows = 1:n
    # h1 = RTutor:::html.table(x[rows,])
    # rows = (NROW(x)-n+1):NROW(x)
    # h2 = RTutor:::html.table(x[rows,])
    # html = c(h1, as.character(p(paste0("... ", NROW(x)-2*n, " rows inbetween ommited  ..."))), h2)
    rows = 1:MAX.ROW
    h1 = RTutor:::html.table(x[rows,])
    html = c(h1, as.character(p(paste0("... ", NROW(x)-MAX.ROW, " rows not shown  ..."))))

  } else {
    html = RTutor:::html.table(x)
  }
  asis_output(html)
}
