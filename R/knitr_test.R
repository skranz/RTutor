library(rmarkdown)
library(knitr)

knit_print.data.frame = function (x, options = NULL, ...) {
  asis_output("Custom Data Frame Printer!")
}

txt = "
```{r}
data.frame(x=1,y=2)
```
"
html = knit2html(text=txt,fragment.only=TRUE)
html
