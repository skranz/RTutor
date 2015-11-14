#" ?foo / help(foo)
#' @export
knit_print.help_files_with_topic = function(x, options) {
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

  type = knitr:::pandoc_to()
  if (is.null(type)) {
    type = knitr:::out_format()
    if (type == 'html') type = 'HTML' else if (type != 'latex') type = 'txt'
  } else {
    type = if (type %in% c('html', 'markdown')) 'HTML' else {
      # it seems \bold in \usepackage{Rd} conflicts with a certain package in
      # the Pandoc template, so unfortunately we cannot use latex format here

      # if (type %in% c('latex', 'beamer')) 'latex' else
      'txt'
    }
  }

  # call tools::Rd2[txt,HTML,latex]
  convert = getFromNamespace(paste('Rd', type, sep = '2'), 'tools')
  out = paste(capture.output(convert(Rd)), collapse = '\n')
  # only need the body fragment (Rd2HTML(fragment = TRUE) does not really work)
  if (type == 'HTML') {
    out = gsub('.*?<body>(.*)</body>.*', '<div class="r-help-page">\\1</div>', out)
    out = gsub('<pre>', '<pre class="r">', out)
  }

  # I do not know where _\b came from in the text mode...
  if (type == 'txt') gsub('_\b', '', out) else asis_output(out)
}



browser.help <- function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), try.all.packages = getOption("help.try.all.packages"), help_type = "html", browser=NULL) 
{
    types <- c("text", "html", "pdf")
    if (!missing(package)) 
        if (is.name(y <- substitute(package))) 
            package <- as.character(y)
    if (missing(topic)) {
        if (!is.null(package)) {
            help_type <- if (!length(help_type)) 
                "text"
            else match.arg(tolower(help_type), types)
            if (interactive() && help_type == "html") {
                if (tools:::httpdPort == 0L) 
                  tools::startDynamicHelp()
                if (tools:::httpdPort <= 0L) 
                  return(library(help = package, lib.loc = lib.loc, 
                    character.only = TRUE))
                #browser <- if (.Platform$GUI == "AQUA") {
                #  get("aqua.browser", envir = as.environment("tools:RGUI"))
                #}
                #else getOption("browser")
                browseURL(paste0("http://127.0.0.1:", tools:::httpdPort, 
                  "/library/", package, "/html/00Index.html"), 
                  browser=browser)
                return(invisible())
            }
            else return(library(help = package, lib.loc = lib.loc, 
                character.only = TRUE))
        }
        if (!is.null(lib.loc)) 
            return(library(lib.loc = lib.loc))
        topic <- "help"
        package <- "utils"
        lib.loc <- .Library
    }
    ischar <- tryCatch(is.character(topic) && length(topic) == 
        1L, error = identity)
    if (inherits(ischar, "error")) 
        ischar <- FALSE
    if (!ischar) {
        reserved <- c("TRUE", "FALSE", "NULL", "Inf", "NaN", 
            "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
        stopic <- deparse(substitute(topic))
        if (!is.name(substitute(topic)) && !stopic %in% reserved) 
            stop("'topic' should be a name, length-one character vector or reserved word")
        topic <- stopic
    }
    help_type <- if (!length(help_type)) 
        "text"
    else match.arg(tolower(help_type), types)
    paths <- utils:::index.search(topic, find.package(if (is.null(package)) 
        loadedNamespaces()
    else package, lib.loc, verbose = verbose))
    tried_all_packages <- FALSE
    if (!length(paths) && is.logical(try.all.packages) && !is.na(try.all.packages) && 
        try.all.packages && is.null(package) && is.null(lib.loc)) {
        for (lib in .libPaths()) {
            packages <- .packages(TRUE, lib)
            packages <- packages[is.na(match(packages, .packages()))]
            paths <- c(paths, index.search(topic, file.path(lib, 
                packages)))
        }
        paths <- paths[paths != ""]
        tried_all_packages <- TRUE
    }
    paths <- unique(paths)
    attributes(paths) <- list(call = match.call(), topic = topic, 
        tried_all_packages = tried_all_packages, type = help_type)
    class(paths) <- "my_help_files_with_topic"
    paths
}


print.my_help_files_with_topic <- function (x, ..., browser=NULL) 
{
    #browser <- getOption("browser")
    topic <- attr(x, "topic")
    type <- attr(x, "type")
    paths <- as.character(x)
    if (!length(paths)) {
        writeLines(c(gettextf("No documentation for %s in specified packages and libraries:", 
            sQuote(topic)), gettextf("you could try %s", sQuote(paste0("??", 
            topic)))))
        return(invisible(x))
    }
    if (type == "html") 
        if (tools:::httpdPort == 0L) 
            tools::startDynamicHelp()
    if (attr(x, "tried_all_packages")) {
        paths <- unique(dirname(dirname(paths)))
        msg <- gettextf("Help for topic %s is not in any loaded package but can be found in the following packages:", 
            sQuote(topic))
        if (type == "html" && tools:::httpdPort > 0L) {
            path <- file.path(tempdir(), ".R/doc/html")
            dir.create(path, recursive = TRUE, showWarnings = FALSE)
            out <- paste0("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n", 
                "<html><head><title>R: help</title>\n", "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=\"UTF-8\">\n", 
                "<link rel=\"stylesheet\" type=\"text/css\" href=\"/doc/html/R.css\">\n", 
                "</head><body>\n\n<hr>\n")
            out <- c(out, "<p>", msg, "</p><br>")
            out <- c(out, "<table width=\"100%\" summary=\"R Package list\">\n", 
                "<tr align=\"left\" valign=\"top\">\n", "<td width=\"25%\">Package</td><td>Library</td></tr>\n")
            pkgs <- basename(paths)
            links <- paste0("<a href=\"http://127.0.0.1:", tools:::httpdPort, 
                "/library/", pkgs, "/help/", topic, "\">", pkgs, 
                "</a>")
            out <- c(out, paste0("<tr align=\"left\" valign=\"top\">\n", 
                "<td>", links, "</td><td>", dirname(paths), "</td></tr>\n"))
            out <- c(out, "</table>\n</p>\n<hr>\n</body></html>")
            writeLines(out, file.path(path, "all.available.html"))
            browseURL(paste0("http://127.0.0.1:", tools:::httpdPort, 
                "/doc/html/all.available.html"), browser)
        }
        else {
            writeLines(c(strwrap(msg), "", paste(" ", formatDL(c(gettext("Package"), 
                basename(paths)), c(gettext("Library"), dirname(paths)), 
                indent = 22))))
        }
    }
    else {
        if (length(paths) > 1L) {
            if (type == "html" && tools:::httpdPort > 0L) {
                browseURL(paste0("http://127.0.0.1:", tools:::httpdPort, 
                  "/library/NULL/help/", topic), browser)
                return(invisible(x))
            }
            file <- paths[1L]
            p <- paths
            msg <- gettextf("Help on topic %s was found in the following packages:", 
                sQuote(topic))
            paths <- dirname(dirname(paths))
            txt <- formatDL(c("Package", basename(paths)), c("Library", 
                dirname(paths)), indent = 22L)
            writeLines(c(strwrap(msg), "", paste(" ", txt), ""))
            if (interactive()) {
                fp <- file.path(paths, "Meta", "Rd.rds")
                tp <- basename(p)
                titles <- tp
                if (type == "html" || type == "latex") 
                  tp <- tools::file_path_sans_ext(tp)
                for (i in seq_along(fp)) {
                  tmp <- try(readRDS(fp[i]))
                  titles[i] <- if (inherits(tmp, "try-error")) 
                    "unknown title"
                  else tmp[tools::file_path_sans_ext(tmp$File) == 
                    tp[i], "Title"]
                }
                txt <- paste0(titles, " {", basename(paths), 
                  "}")
                res <- menu(txt, title = gettext("Choose one"), 
                  graphics = getOption("menu.graphics"))
                if (res > 0) 
                  file <- p[res]
            }
            else {
                writeLines(gettext("\nUsing the first match ..."))
            }
        }
        else file <- paths
        if (type == "html") {
            if (tools:::httpdPort > 0L) {
                path <- dirname(file)
                dirpath <- dirname(path)
                pkgname <- basename(dirpath)
                browseURL(paste0("http://127.0.0.1:", tools:::httpdPort, 
                  "/library/", pkgname, "/html/", basename(file), 
                  ".html"), browser)
            }
            else {
                warning("HTML help is unavailable", call. = FALSE)
                att <- attributes(x)
                xx <- sub("/html/([^/]*)\\.html$", "/help/\\1", 
                  x)
                attributes(xx) <- att
                attr(xx, "type") <- "text"
                print(xx)
            }
        }
        else if (type == "text") {
            pkgname <- basename(dirname(dirname(file)))
            temp <- tools::Rd2txt(.getHelpFile(file), out = tempfile("Rtxt"), 
                package = pkgname)
            file.show(temp, title = gettextf("R Help on %s", 
                sQuote(topic)), delete.file = TRUE)
        }
        else if (type %in% "pdf") {
            path <- dirname(file)
            dirpath <- dirname(path)
            texinputs <- file.path(dirpath, "help", "figures")
            tf2 <- tempfile("Rlatex")
            tools::Rd2latex(.getHelpFile(file), out = tf2)
            .show_help_on_topic_offline(tf2, topic, type, texinputs)
            unlink(tf2)
        }
    }
    invisible(x)
}