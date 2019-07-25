# Some functions to deal with fill-in chunks

check.fill.in.lines = function(txt) {
  return(TRUE)
  if (!(all(startsWith(txt, "#") | nchar(trimws(txt))==0))) {
    stop(paste0("In a fill_in block ALL lines have to be commented out and start with '# '. A comment line should look like\n\n# # My comment\n\n. Your following fill_in block violates this rule:\n\n", paste0(txt,collapse="\n")))
  }
}

mark.fill.in.lines = function(txt) {
  #check.fill.in.lines(txt)
  
  # If all lines in a fill_in block are
  # comments, remove the comments.
  if (all(substring(txt,1,2)=="# " | nchar(trimws(txt))==0)) {
    txt[nchar(trimws(txt))==0] = "# "
  }
  txt = paste0("#___",txt)
  txt
}

fill.in.lines.to.comment = function(txt) {
  rows = startsWith(txt,"#___# ") 
  txt[rows] = substring(txt[rows],5)
  txt
}
fill.in.lines.to.code = function(txt) {
  rows = startsWith(txt,"#___# ") 
  txt[rows] = substring(txt[rows],7)
  txt
}
remove.fill.in.lines = function(txt) {
  txt = txt[!startsWith(txt,"#___# ")]
  txt
}

