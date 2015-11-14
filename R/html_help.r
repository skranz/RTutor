#" ?foo / help(foo)
#' @export
get.help.txt = function(topic, options=list(), type=c("txt","HTML")[1]) {
  x = help(topic)
  n = length(x)
  topic = attr(x, 'topic')
  if (n == 0) return(paste(
    "No documentation for '", topic, "' in specified packages and libraries",
    sep = ''
  ))
  base = basename(file <- as.character(x))
  pkg  = basename(dirname(dirname(file)))
  if (n > 1) return(paste(
    "Help on topic '", topic, "' was found in the following packages:\n\n",
    paste('  *', pkg, collapse = '\n'), sep = ''
  ))

  db = tools::Rd_db(pkg)
  Rd = db[[which(base == sub('[.]Rd$', '', basename(names(db))))]]
  Rd = extract_Rd(Rd, options$render.args$help$sections)

  # call tools::Rd2[txt,HTML,latex]
  convert = getFromNamespace(paste('Rd', type, sep = '2'), 'tools')
  out = paste(capture.output(convert(Rd)), collapse = '\n')
  # only need the body fragment (Rd2HTML(fragment = TRUE) does not really work)
  if (type == 'HTML') {
    out = gsub('.*?<body>(.*)</body>.*', '<div class="r-help-page">\\1</div>', out)
    out = gsub('<pre>', '<pre class="r">', out)
  }

  # I do not know where _\b came from in the text mode...
  if (type == 'txt') gsub('_\b', '', out) else (out)
}



extract_Rd = function(Rd, section) {
  if (length(section) == 0) return(Rd)
  # extract the section names, and remove the leading backslash
  sections = sub('^\\\\', '', unlist(lapply(Rd, attr, 'Rd_tag')))
  section  = c('title', 'name', section)  # title and name are required
  Rd[which(!(sections %in% section))] = NULL
  Rd
}

