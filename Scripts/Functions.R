# Packages for function

library(tidyverse)

# Tidy functions

tidy_count <- function(df, x) {
  df |> 
    count({{ x }})
}

gg_hist <- function(df, x) {
  df |> 
    ggplot(aes({{ x }})) + geom_histogram()
  
}

gg_scatter <- function(df, x, y) {
  df |> 
    ggplot(aes({{ x }}, {{ y }})) + geom_point()
  
}

gg_box <- function(df, x, y) {
  df |> 
    mutate({{ x }} := forcats::fct_reorder({{ x }}, {{ y }})) |> 
    ggplot(aes({{ x }}, {{ y }})) + geom_boxplot()
  
}




