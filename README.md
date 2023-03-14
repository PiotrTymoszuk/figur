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

## reference

refer(fig_list$fig1)

Figure \@ref(fig:fig-test1-figure)

pickle(fig_list$fig1)

```
</details>

<details>
   <summary>Handling data frames</summary>

### Handling data frames

   
</details>

<details>
   <summary>Handling R code</summary>
   
### Handling R code
   
</details>

## Terms of use

The package is available under the [GPL-3 license](https://github.com/PiotrTymoszuk/figur/blob/main/LICENSE).

## Contact

The package maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com).

## Acknowledgements

`figur` uses tools provided by the [rlang](https://rlang.r-lib.org/), [tidyverse](https://www.tidyverse.org/), [stringi](https://stringi.gagolewski.com/), [flextable](https://ardata-fr.github.io/flextable-book/), [knitr](https://yihui.org/knitr/), [clipr](https://github.com/mdlincoln/clipr) and [bib2df](https://github.com/ropensci/bib2df).
