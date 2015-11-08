
## 2015-11-08: htmlwidget support

- Allow chunks that output htmlwidgets. You need to set the chunk option `output="htmlwidget"`, and specify the widget type in the chunk option `widget`, e.g. `widget="leaflet"`. An example is given in `LeafletExample_sol.Rmd`.

## 2015-11-02: quiz blocks

- Added quiz blocks. You need to provide the argument `addons="quiz"` in create.ps, and add `#< quiz quiz_name ... #>` blocks. For an example, see `QuizExample_sol.Rmd`.


## 2015-11-01: option for memoisation when reading data

- Added the argument `use.memoise` in create.ps. If set to `TRUE`, the functions listed in the argument `memoise.funs` will be memoised when running and showing a problem set. By default memoisation is done for a set of functions that read data from files. This saves time and memory if the same data set is repeatedly loaded in different 
exercises.

