# News for RTutor package

#### 2019-07-24

Allow adaptive custom hints.

Make automatic tests more informative for some common errors.

Created the companion package RTutorSAGI that helps to analyse
submission logs in order to identify parts where students systematically
get stuck.

#### 2019-04-10

Add simple functionality to render data frames as word table if
output solution is knitted as Word file

#### 2019-03-23

Add fill_in block to easier handle syntactially incorrect R code
that has placeholders. Use it when you want to show students something like

filter(df, a == ___)

where students have to replace the ___.

#### 2019-02-19

Several changes in the last 4 years, e.g.:

- optional chunks
- more robust hints
- automatically try to run previous chunks in shiny mode
- possibility to download submission file from shiny


### 2015-11-29 preknit and show in noeval mode

- When creating the problem set, we can now already knit for each
  chunk the sample solution.
- A preknitted shiny based problem set can be shown with the parameter
  `noeval=TRUE`. Then user code is then only parsed but not evaluated.
  Tests and hints only compare the parsed expression with the parsed
  sample solution. The noeval mode can be useful for hosting problem sets
  on an own webserver, where evaluation of user code is a high 
  security risk or the user shall have no direct access to the data.
  The cost is less flexibility in testing the user code,
  e.g. in noeval mode,  `x=1+2` will not be seen as correct if the
  sample solution is `x=2+1`.

### 2015-11-17 print data.frames as html table in shiny-based RTutor

- In the shiny interface of RTutor data.frames are now printed by
  default as HTML tables (and only a maximum number of rows are shown).
  The printing behavior can be modified on a global level in the call
  to show.ps or on the chunk level. See `DataFramesExample_sol.Rmd`
  for examples.

### 2015-11-13: hints at beginning of chunk possible

- You can now add a hint block at the beginning of a chunk,
  before any other command.
  While other hints are linked to the previous expression that will
  be tested, the initial hint works for the whole chunk.
  It will be shown in addition to any expression specific hint.
  It will also  be shown if the code cannot be evaluated
  or in a chunk that has no expression to be tested.


### 2015-11-08: htmlwidget support

- Allow chunks that output htmlwidgets. You need to set the chunk option `output="htmlwidget"`, and specify the widget type in the chunk option `widget`, e.g. `widget="leaflet"`. An example is given in `LeafletExample_sol.Rmd` in the `./inst/examples` folder.

### 2015-11-02: quiz blocks

- Added quiz blocks. You need to provide the argument `addons="quiz"` in create.ps, and add `#< quiz quiz_name ... #>` blocks. For an example, see `QuizExample_sol.Rmd` in the `./inst/examples` folder.


### 2015-11-01: option for memoisation when reading data

- Added the argument `use.memoise` in create.ps. If set to `TRUE`, the functions listed in the argument `memoise.funs` will be memoised when running and showing a problem set. By default memoisation is done for a set of functions that read data from files. This saves time and memory if the same data set is repeatedly loaded in different 
exercises.

### 2015-06-01: awards also in web-based problem sets

- awards can now be used and will be correctly displayed also in web-based problem sets

### 2015-04-01: optional code chunks and notes with chunks

- Add chunk option `optional=TRUE`. Optional chunks don't need to be solved in an exercise and their computations will not be available in subsequent chunks.

- We can now put RTutor chunks and texts in notes using the lines
```  
  #! start_note "Note Title"
   ...
  #! end_note
```
  Unlike info blocks the chunks inside a note can be solved.    Chunks inside a note should have the chunk option `optional=TRUE`.
