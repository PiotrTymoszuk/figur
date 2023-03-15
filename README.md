# figur
R package __figur__: Smart Handling of Figures, Tables and Code in RMarkdown Documents

## Installation

You may fetch the package easily with `devtools`: 

```r

devtools::install_github('PiotrTymoszuk/figur')

```
## Basic usage

<details>
   <summary>Handling ggplot figures</summary>

### Handling ggplot figures

The core functionality of the `figur` package is the convenient storage and insertion/referencing of figures, preferably in the `ggplot` format, in RMarkdown documents. You may create `ggplot` graphs in an usual way:

```r

library(tidyverse)
library(cowplot) ## to create multi-graph panels

## plotting data 

  test_cars <- mtcars %>%
    rownames_to_column('car') %>%
    as_tibble
    
> test_cars
# A tibble: 32 × 12
   car                 mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
   <chr>             <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
 1 Mazda RX4          21       6  160    110  3.9   2.62  16.5     0     1     4     4
 2 Mazda RX4 Wag      21       6  160    110  3.9   2.88  17.0     0     1     4     4
 3 Datsun 710         22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
 4 Hornet 4 Drive     21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
 5 Hornet Sportabout  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
 6 Valiant            18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
 7 Duster 360         14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
 8 Merc 240D          24.4     4  147.    62  3.69  3.19  20       1     0     4     2
 9 Merc 230           22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
10 Merc 280           19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
# … with 22 more rows
# ℹ Use `print(n = ...)` to see more rows  

## plots

 car_dist <- test_cars %>%
    ggplot(aes(x = mpg,
               y = reorder(car, mpg))) +
    geom_bar(stat = 'identity',
             fill = 'steelblue') +
    theme_classic() +
    theme(axis.title.y = element_blank()) +
    labs(title = 'Miles per gallon')


  car_cyl <- test_cars %>%
    ggplot(aes(x = cyl,
               y = mpg,
               color = factor(gear))) +
    geom_point(shape = 16,
               position = position_jitter(width = 0.1, height = 0.15)) +
    theme_light() +
    labs(title = 'Mlies per gallon',
         fill = 'Gears')

  car_panel <- plot_grid(car_dist,
                         car_cyl,
                         ncol = 2,
                         labels = LETTERS)

```

Subsequently, they can be converted into `figure` class objects keeping together the graph, its width and height (`w` and `h`), label representing the file name on the disc, reference name (`ref_name`) and caption used later in the RMarkdown document. This can be easily done by calling `as_figure()`: 

```r

## call as_figure() to create a figure object

fig_list <- list(fig1 = as_figure(car_dist,
                                    w = 90,
                                    h = 90,
                                    label = 'test1',
                                    ref_name = 'test1_figure',
                                    caption = 'caption for Figure 1'),
                   fig2 = as_figure(car_cyl,
                                    w = 90,
                                    h = 90,
                                    label = 'test2',
                                    ref_name = 'test2_figure',
                                    caption = 'caption for Figure 2'))
                                    
 car_figure <- as_figure(car_panel,
                          label = 'car_panel',
                          w = 180,
                          h = 120,
                          unit = 'mm')

```
The subsequent insertion of a figure chunk in the RMarkdown document works seamlessly with the `insert()` method. The dimensions are automatically converted into inches or provided as a function call. By default, the figure chunk is copied to your clipboard. Alternatively, it can append an existing .Rmd file. 
Of note, the `insert()` method takes care for chunk names compatible with the standard Rmarkdown/bookdown format, e.g. by substitution of '_':

```r

## canonical Rmarkdown/bookdown format: the chunk is copied into the clipboard

insert(fig_list$fig1)

# ```{r fig-test1-figure, fig.width = 3.543307083, fig.height = 3.543307083, fig.cap = 'caption for Figure 1'}

# fig_list$fig1$plot

# ```

# __Figure \@ref(fig:fig-test1-figure). caption for Figure 1__ 
# _<<legend>>_

## dimensions as a functon call: insensitive to later resizing of the figure

insert(fig_list$fig2, relative_dim = TRUE)

# ```{r fig-test2-figure, fig.width = figur::convert(fig_list$fig2, to = 'in')$w, fig.height = figur::convert(fig_list$fig2, to = 'in')$h, fig.cap = 'caption # for Figure 2'}

# fig_list$fig2$plot

# ```

# __Figure \@ref(fig:fig-test2-figure). caption for Figure 2__ 
# _<<legend>>_

```
Finally, by calling `refer()` a Rmarkdown/bookdown-compatible reference to the figure is generated and, by default, copied to the clipboard. To save the figure on the disc, use `pickle()` - the file name and image dimensions are stored in the object: 

```r

## referencing

refer(fig_list$fig1)

Figure \@ref(fig:fig-test1-figure)

pickle(fig_list$fig1)

```
</details>

<details>
   <summary>Handling data frames</summary>

### Handling data frames

Basically, any data frame may be converted to an `mdtable` object, which as in case of graph-storing `figur` instance, bundles the data frame with its later reference and caption in the Rmarkdown document:
   
```r
   
   test_tbl <- as_mdtable(mtcars,
                         label = 'mt_cars',
                         ref_name = 'mt_cars',
                         caption = 'Car data')
               
> head(test_tbl)
                   mpg cyl disp  hp drat    wt  qsec vs am gear carb
Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
   
   
```
Insertion of the corresponding chunk into the Rmarkdown document and referencing is analogically done with the `insert()` and `refer()` methods. By default, the output is copied to the clipboard:
   
```r
   
> insert(test_tbl)
# ```{r tab-mt-cars, tab.cap = 'Car data'}

# flextable::flextable(test_tbl)

# ```

> refer(test_tbl)
Table \@ref(tab:tab-mt-cars)
   
```   
</details>

<details>
   <summary>Handling R code</summary>
   
### Handling R code
   
Management of R code in Rmarkdown may pose a challenge, especially in lengthy documents with multiple repeating inline code elements. Additionally, debugging may consume lots of time. A smarter alternative to the 'copy-paste' approach and tesing the code in the console is provided with the `mdexpr` object. Virtually any R expression may be wrapped with `mdexpr()` which stores the text code representation and evaluation result. By this means, any evaluation errors are directly reported:
   
```r
   
## error-free evaluation
   
test_mdexpr <- mdexpr(nrow(mtcars), ref_name = 'mtcar_size')
                  
> test_mdexpr
mdexpr: {nrow(mtcars)} = 32
   
## errors are raised at creation of the mdexpr:
   
> mdexpr(mtcars$mpg[1, 2], ref_name = 'mtcar_size')
Error in mtcars$mpg[1, 2] : incorrect number of dimensions                
     
```

The code chunk is inserted as an inline element with the `refer()` call and as a multi-line chunk with the `insert()` method. By default, the output is copied into the clipboard:
   
```r
   
> refer(test_mdexpr)
# `r nrow(mtcars)`
   
> insert(test_mdexpr)
#```{r mtcar-size}

#nrow(mtcars)

#```
   
```
   
</details>
   
<details>
   <summary>Bibliography</summary>
   
 ## Bibliography
   
In my experience, a combination of an external citation manager and R Studio is not the most efficient one. The `mdbib` object storing the bibliography derived from the most common BibTex format and enabling for search via regular expression and referencing directly from R can make management of literature references more straightforward. 

To create a `mdbib` object, just read your BibTex file from the disc with `read_bib()`:
  
```r
   
mol_bib <- read_bib('./test/mol_biblio.bib')
           
>  mol_bib
# A tibble: 10 × 33
   CATEGORY BIBTEXKEY ADDRESS ANNOTE AUTHOR BOOKT…¹ CHAPTER CROSS…² EDITION EDITOR HOWPU…³ INSTI…⁴ JOURNAL KEY   MONTH NOTE  NUMBER
 * <chr>    <chr>     <chr>   <chr>  <list> <chr>   <chr>   <chr>   <chr>   <list> <chr>   <chr>   <chr>   <chr> <chr> <chr> <chr> 
 1 ARTICLE  Cavalier… NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      JCO pr… NA    nov   NA    5     
 2 ARTICLE  Wu2020    NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      Molecu… NA    jun   NA    1     
 3 ARTICLE  Ding2022  NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      Fronti… NA    feb   NA    NA    
 4 ARTICLE  Wichmann… NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      Intern… NA    dec   NA    12    
 5 ARTICLE  Keck2015  NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      Clinic… NA    feb   NA    4     
 6 ARTICLE  Walter20… NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      PloS o… NA    feb   NA    2     
 7 ARTICLE  VanHooff… NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      Journa… NA    nov   NA    33    
 8 ARTICLE  Lawrence… NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      Nature… NA    jan   NA    7536  
 9 ARTICLE  Mermel20… NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      Genome… NA    apr   NA    4     
10 ARTICLE  Benjamin… NA      NA     <chr>  NA      NA      NA      NA      <chr>  NA      NA      bioRxiv NA    dec   NA    NA    
# … with 16 more variables: ORGANIZATION <chr>, PAGES <chr>, PUBLISHER <chr>, SCHOOL <chr>, SERIES <chr>, TITLE <chr>, TYPE <chr>,
#   VOLUME <chr>, YEAR <dbl>, ABSTRACT <chr>, DOI <chr>, ISSN <chr>, KEYWORDS <chr>, MENDELEY.TAGS <chr>, PMID <chr>, URL <chr>,
#   and abbreviated variable names ¹​BOOKTITLE, ²​CROSSREF, ³​HOWPUBLISHED, ⁴​INSTITUTION
# ℹ Use `colnames()` to see all variable names
   
```
Technically, the `mdbib` instance is nothing else as a data frame or tibble which may be searched with your favourite tool set like tidyverse's `filter()`. 
The `figur` package offers also a possibilty to search with regular expressions via `reglook()`. Finally, the citations can be easily pasted into your Rmarkdown document by calling `refer()`. The whole procedure works particularly caompact in a pipeline:
   
```r
   
 ## be default the output is copied into the clipboard:
   
   mol_bib %>%
    reglook(regex = '(The Cancer)|(GSE\\d+)') %>%
    refer
   
  # [@Cavalieri2021; @Lawrence2015]
   
```
   
</details>

## Terms of use

The package is available under a [GPL-3 license](https://github.com/PiotrTymoszuk/figur/blob/main/LICENSE).

## Contact

The package maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com).

## Acknowledgements

`figur` uses tools provided by the [rlang](https://rlang.r-lib.org/), [tidyverse](https://www.tidyverse.org/), [stringi](https://stringi.gagolewski.com/), [flextable](https://ardata-fr.github.io/flextable-book/), [knitr](https://yihui.org/knitr/), [clipr](https://github.com/mdlincoln/clipr) and [bib2df](https://github.com/ropensci/bib2df).
