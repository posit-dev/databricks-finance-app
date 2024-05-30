# helpers.R
library(dplyr)
library(ggplot2)
library(dbplyr)
library(dbplot)

# Returns binned interest rates for 50 most similar applicants
find_rates_for_similar_applicants <- function(data, 
                                              .term, 
                                              .all_util, 
                                              .bc_util, 
                                              .bc_open_to_buy) {  
    
  data |>
    mutate(distance = (term - as.numeric(.term))^2 +
             (all_util - .all_util)^2 +
             (bc_util - .bc_util)^2 +
             (bc_open_to_buy - .bc_open_to_buy)^2,
           rank = min_rank(distance)) |> 
    filter(rank <= 50 & !is.na(rank)) |> 
    db_compute_bins(int_rate, binwidth = 0.5)
  
}

# Plots binned interest rates
plot_rate_distribution <- function(distribution) {
  
  distribution |>
    ggplot() +
    geom_col(aes(x = int_rate, y = count), 
             fill = "#4682b4", color = "#4682b4", alpha = 0.4) +
    labs(x = "Interest Rate (%)", y = "Number of applicants") +
    scale_x_continuous(limits = c(0, 35),
                       breaks = seq(from = 0, to = 35, by = 5)) +
    theme_minimal()
  
}
