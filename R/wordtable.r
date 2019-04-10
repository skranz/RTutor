example.word.table = function() {
  df = data.frame(x=paste0("x",1:3), y=paste0("y",1:3))
  cat(word.xml.table(df))
}

word.xml.table = function(df, hjust=c("start","center","end")[2], titles=colnames(df), add.md=FALSE, cell.padding.top=15, cell.padding.bottom=15, zebra.fill = "ECECEC") {
  restore.point("word.xml.table")
  if (NROW(df)==0) return("")
  
  head = '<w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">'
  
  borders = '\n\t<w:tblBorders>
<w:top w:val="single" w:sz="1" w:space="0" w:color="auto" />
<w:start w:val="single" w:sz="1" w:space="0" w:color="auto" />
<w:bottom w:val="single" w:sz="1" w:space="0" w:color="auto" />
<w:end w:val="single" w:sz="1" w:space="0" w:color="auto" />
</w:tblBorders>\n'
  
  tblPr = paste0('\n<w:tblPr>',borders,'<w:jc w:val="', hjust,'"/></w:tblPr>\n')

  title = paste0('<w:tr><w:trPr><w:tblHeader/></w:trPr>',
    paste0('\n\t<w:tc><w:tcPr><w:tcBorders>
<w:bottom w:val="single" w:sz="1" w:space="0" w:color="auto" />
</w:tcBorders>\n\t<w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"></w:tcPr><w:p><w:pPr><w:spacing w:before="', cell.padding.top,'" w:after="',cell.padding.bottom,'"/></w:pPr><w:r><w:rPr><w:b w:val="true"/></w:rPr><w:t>',titles,'</w:t></w:r></w:p></w:tc>', collapse=""),
    paste0('</w:tr>\n')
  )

  shade.odd = paste0('<w:shd w:val="clear" w:color="auto" w:fill="',zebra.fill,'">')
  shade = rep("", NROW(df))
  shade[(1:NROW(df) %% 2) == 1] = shade.odd
  
  
  content.li = lapply(df, function(val) {
    paste0('\n\t<w:tc><w:tcPr>',shade,'</w:tcPr><w:p><w:pPr><w:spacing w:before="', cell.padding.top,'" w:after="',cell.padding.bottom,'"/>
</w:pPr><w:r><w:t>',val,'</w:t></w:r></w:p></w:tc>')
  })
  names(content.li) = NULL
  
  content = do.call(paste0, content.li)    
  content = paste0('<w:tr>',content,'</w:tr>', collapse="\n")
  
  xml = paste0(head,tblPr,"\n", title, content, " \n</w:tbl>")
  if (!add.md) return(xml)
  
  return(paste0("```{=openxml}\n",xml,"\n```"))
  
}

