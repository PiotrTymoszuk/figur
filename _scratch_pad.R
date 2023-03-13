# Testim of the package

# tools -------

  library(ggplot2)
  library(cowplot)
  library(tidyverse)
  library(figur)
  library(rmarkdown)
  library(bookdown)
  library(knitr)

# test data -------

  test_cars <- mtcars %>%
    rownames_to_column('car') %>%
    as_tibble

# figures ------

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

# Insertion -------

  insert_figure(as_figure(car_panel,
                          label = 'car_panel',
                          w = 180,
                          h = 120,
                          unit = 'mm'))

  insert_figure(fig_list[[1]],
                fig_list[[2]],
                ref_names = names(fig_list),
                legend_text = c('text1', 'text2'),
                html = TRUE,
                style_ref = 'my_legend')

  insert_figure(fig_list[[1]])

  fig_list$fig1 %>% plot

  insert(fig_list[[1]],
         html = TRUE,
         #file = 'test.Rmd',
         append = FALSE)

  insert(fig_list$fig2,
         html = FALSE,
         #file = 'test.Rmd',
         append = TRUE)

  refer(fig_list[[1]])


  test_tbl <- as_mdtable(mtcars,
                         label = 'mt_cars',
                         ref_name = 'mt_cars',
                         caption = 'Car data')

  test_mtx <- matrix(sample(1:100, 10), ncol = 2)

  refer(as_mdtable(test_mtx,
                   label = 'test_mtx',
                   ref_name = 'test_mtx1',
                   caption = 'Test matrix'))

  attributes(test_tbl)

  # pickle(test_tbl)

  insert(test_tbl, format = 'flextable')

  refer(test_tbl)

  test_mdexpr <- mdexpr(nrow(mtcars), ref_name = 'mtcar_size')

  figur::extract(test_mdexpr, 'ref_name')

  insert(test_mdexpr, echo = TRUE)

  insert(test_mdexpr, format = 'inline')

# Working with a markdown document ------

  report_exp <-
    list(car_number = mdexpr(nrow(mtcars)),
         var_number = mdexpr(ncol(mtcars)),
         average_mpg = mdexpr(mean(mtcars$mpg, na.rm = TRUE) %>%
                                signif(3)),
         car_names = mdexpr(rownames(mtcars), ref_name = 'car_models'))

  report_tables <-
    list(cars = as_mdtable(mtcars,
                           label = 'mt_cars',
                           ref_name = 'mt_cars',
                           caption = 'Car data'),
         mtx = as_mdtable(matrix(sample(1:100, 10), ncol = 2),
                          label = 'test_mtx',
                          ref_name = 'test_mtx1',
                          caption = 'Test matrix'))

  report_plots <-
    list(fig1 = as_figure(car_dist,
                          w = 90,
                          h = 60,
                          label = 'figure_1',
                          ref_name = 'fig_dist',
                          caption = 'Mileage of the MT cars.',
                          legend_text = 'test legend for figure 1'),
         fig2 = as_figure(car_cyl,
                          w = 90,
                          h = 60,
                          label = 'figure_2',
                          ref_name = 'fig_cyl',
                          caption = 'Cylinder count of the MT cars',
                          legend_text = 'test legend for figure 2'),
         panel = as_figure(plot_grid(car_dist,
                                     car_cyl,
                                     ncol = 2,
                                     labels = LETTERS),
                           label = 'figure_3',
                           w = 180,
                           h = 90,
                           ref_name = 'fig_panel',
                           caption = 'Summary results of the analysis',
                           legend_text = 'summary analysis panel'))

# additional references -----

  refer(report_exp$var_number)
  refer(report_exp$car_number)
  insert(report_exp$car_names, echo = TRUE)
  refer(report_exp$average_mpg)

  refer(report_tables$cars)
  insert(report_tables$cars)

  refer(report_plots$fig1)
  refer(report_plots$fig2)
  refer(report_plots$panel)


  insert(report_plots$fig1)
  insert(report_plots$fig2)

  insert(report_plots$panel, html = TRUE, relative_dim = TRUE)

# Links and HTML elements ---------

  xtml_ref <-
    mdlink('http://web4dummy.um-gallery.com/s02.1html-tag.html',
           ref_name = 'XTML tag reference')

  cmm_test_p <-
    mdhtml('<p style = "color: maroon">This text was inserted as a HTML element</p>')

  cmm_sep <-
    mdhtml('<hr />')

  insert(xtml_ref, html = FALSE, title = '_test reference_')

  insert(cmm_test_p)

  insert(cmm_sep)

# final html report ------

  render('./test/test_report.Rmd',
         output_format = html_document2(css = 'styles.css'),
         output_dir = './test')

# END -------
