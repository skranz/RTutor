# Some customization to yaml


# custom handler for booleans: only convert "TRUE" and "FALSE" to boolean
yaml.bool.handler.yes <- function(val) {
  #message(paste("bool: ", val))
  if (val=="TRUE" | val=="true")
    return(TRUE)
  return(val)
}

# custom handler for booleans: only convert "TRUE" and "FALSE" to boolean
yaml.bool.handler.no <- function(val) {
  #message(paste("bool: ", val))
  if (val=="FALSE" | val=="false")
    return(FALSE)
  return(val)
}

#' Reads a yaml file and returns as a list
#' @export
read.yaml = function(file=NULL, verbose=FALSE, keep.quotes=TRUE, quote.char = "__QUOTE__", text=NULL, catch.error = TRUE, check.by.row=FALSE, space.after.colon=FALSE, utf8=TRUE) {
  restore.point("read.yaml")
  if (!is.null(file)) {
    file.str = paste0(" in ", file)
  } else {
    file.str = ""
  }
  if (is.null(text)) {
    str = suppressWarnings(paste(readLines(file), collapse = "\n"))
  } else {
    str = text
  }

  if (utf8) {
   # str = enc2utf8(str)
    Encoding(str) <- "UTF-8"
  }


  #message(paste("read.yam:", file))
  # Convert tabs to spaces
  str = gsub("\t","   ",str)
  # Convert ":text" into ": text"
  if (space.after.colon) {
    str = gsub(":",": ",str)
    str = gsub(":  ",": ",str)
  }
  
  handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no)
  if (keep.quotes) {
    str = gsub('"',quote.char,str,fixed=TRUE)
    yaml.string.handler = function(val) {
      gsub(quote.char,'"',val,fixed=TRUE)
    }
    handlers[["str"]]=yaml.string.handler
  } 
  if (verbose)
    cat(str)



  if (check.by.row) {
    sep.str = strsplit(str,"\n", fixed=TRUE)[[1]]
    for (row in 1:length(sep.str)) {
      cat("\n try to read rows 1:",row,"\n")
      txt = paste0(sep.str[1:row],collapse="\n")
      tryCatch(
        yaml.load(txt, handlers=handlers),
        error = function(e) {
          str = paste0(as.character(e),file.str, " row ",row,"\n")
          rows = max(row-2,1):min(row+1,length(sep.str))
          str = paste0(str,paste0(rows,": ",sep.str[rows],collapse="\n"))
          stop(str, call.=FALSE)
        }
      )
    }
  }

  tryCatch(
    li <- yaml.load(str, handlers=handlers),
    error = function(e) {
      str = paste0(as.character(e),file.str)
      stop(str, call.=FALSE)
    }
  )

  if (utf8) {
    li = mark_utf8(li)
  }
  li
  #suppressWarnings(yaml.load(str, handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no)))

}

examples.read.yaml = function() {

  fn = paste0("D:/libraries/XEconDB/Structures/Games/LureOfAuthorityAlternative.yaml")
  obj = read.yaml(fn)
  obj$variants
}


#' Prints list read from a yaml file
#' @export
print.yaml = function(obj) {
  if (class(obj)=="character") {
    cat(obj)
  } else {
    cat(as.yaml(obj))
  }
}

read.yaml.blocks = function(txt, add.txt =TRUE, omit.header=FALSE, tab.width=3) {
  restore.point("read.yaml.blocks")

  first.char = substring(txt,1,1)
  start = nchar(txt)>0 & first.char!=" " & first.char!="#" & first.char !="\t"
  start = which(start)
  name = str.left.of(txt[start],":")
  end = c(start[-1]-1,length(txt))

  if (!add.txt) {
   ret = quick.df(name=name, start.row=start, end.row=end)
  } else {
    if (!omit.header) {
      block.txt = sapply(seq_along(start), function(i) {
        paste0(txt[int.seq(start[i]+omit.header,end[i])], collapse="\n")
      })
    } else {
      i = 4
      block.txt = sapply(seq_along(start), function(i) {
        space.str = paste0(rep(" ", tab.width), collapse="")
        str = txt[int.seq(start[i]+omit.header,end[i])]
        left = ifelse(str.starts.with(str,space.str), tab.width+1,1)
        str = substring(str, left)
        paste0(str, collapse="\n")
      })
    }
    ret = quick.df(name=name, start.row=start, end.row=end, txt=block.txt)
  }
  ret
}


