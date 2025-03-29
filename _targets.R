library(targets)
library(tarchetypes)



source("functions.R")
tar_option_set(package = c("ggplot2", "tidyverse", "readxl", "ggpmisc", "stringr", "ggrepel", "RColorBrewer"))
list(
  tar_target(birds, load_data()),
  tar_target(birds_grouped, summarize_data(birds)),
  tar_target(birds_grouped_recent, subset(birds_grouped, YEAR>=1900)),
  
  tar_target(women_current, plot_women(birds_grouped)),
  tar_target(women_future, plot_women(add_future(birds_grouped), fullrange=TRUE)),
  tar_target(nonwestern_current, plot_nonwestern(birds_grouped)),
  tar_target(nonwestern_future, plot_nonwestern(add_future(birds_grouped), fullrange=TRUE)),
  tar_target(new_birds, plot_new_birds_over_time(birds_grouped)),
  
  tar_target(predict_women, get_predictions(birds_grouped_recent, "proportion_women")),
  tar_target(predict_nonwestern, get_predictions(birds_grouped_recent, "proportion_nonwestern")),
  tar_target(predict_new_birds, get_predictions(birds_grouped_recent, "n_species")),
  
  tar_target(prediction_proportion_new_species_women, get_predictions(birds_grouped_recent, "proportion_women_per_new_species")),
  tar_target(prediction_proportion_new_species_nonwestern, get_predictions(birds_grouped_recent, "proportion_nonwestern_per_new_species")),
  tar_target(prediction_proportion_new_species_men, get_predictions(birds_grouped_recent, "proportion_men_per_new_species")),
  tar_target(prediction_proportion_new_species_western, get_predictions(birds_grouped_recent, "proportion_western_per_new_species")),
  
  tar_target(prediction_years_women, get_prediction_range(predict_women)),
  tar_target(prediction_years_nonwestern, get_prediction_range(predict_nonwestern)),
  tar_target(prediction_years_nonwestern_population, get_prediction_range(predict_nonwestern, target=proportion_nonwestern())),
  
  tar_target(last_ten_year_average, mean(tail(birds_grouped_recent$n_species,10))),
  
  tar_target(full_predictions, get_total_number_of_eponymous_species_of_each_kind(birds_grouped, women=prediction_proportion_new_species_women, men=prediction_proportion_new_species_men, nonwestern=prediction_proportion_new_species_nonwestern, western=prediction_proportion_new_species_western, last_ten_year_average=last_ten_year_average)),
  tar_target(pivoted_predictions, convert_longer(full_predictions)),
  tar_target(plot_predictions, plot_full_predictions(pivoted_predictions)),
  tar_target(plot_predictions_present, plot_full_predictions(pivoted_predictions, filter_to_present=TRUE))
)

