# figur
__package figur: rendering, disc saving, R markdown insertion and referencing of ggplot objects, data frames/matrices and R expressions__

## Keep your results an metadata at one place

The package tools enable bundling of ggplot graphics (_figure()_), data frames or matrices (_mdtable()_) and R expressions (_mdexpr()_) with metadata such as dimensions, file names, R makdown chunk names useful during disc saving and R markdown rendering. Additionally, expressions will be tested for any evaluation errors which saves time spent on debugging.

## Insert and cite your analysis results in R markdown

With the _insert()_ generic, you can generate ready-to-use code chunks with your plots, tables and R expression code results and insert them into .Rmd files. With _refer()_ you may easily reference your plots, tables in the markdown document or create inline R code.

## Save your plots and tables

With the _pickle()_ tool, the _figure_, _mdtable_ and _mdexpr_ objects may be saved on the disc in various formats with the pre-specified name and dimensions.
