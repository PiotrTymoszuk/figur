
library(ggplot2)
library(cowplot)
library(tidyverse)

test_cars <- mtcars %>%
  rownames_to_column('car') %>%
  as_tibble

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
