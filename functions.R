
# Pulls in data from 
#George Sangster, Eponyms of birds mostly honour scientists and show positive inclusivity trends, Zoological Journal of the Linnean Society, Volume 203, Issue 3, March 2025, zlaf022, https://doi.org/10.1093/zoolinnean/zlaf022

load_data <- function() {
	birds <- readxl::read_xlsx("data/zlaf022_suppl_supplementary_material.xlsx", sheet=1, skip=3, col_types="text")
	birds <- subset(birds, !is.na(birds$YEAR))
	birds$YEAR <- as.numeric(birds$YEAR)
	return(birds)
}

summarize_data <- function(birds) {
	birds_grouped <- birds |> group_by(YEAR) %>%
		summarise(n_species = n(), n_women = sum(gender=='female', na.rm=TRUE), n_men = sum(gender=='male', na.rm=TRUE), n_western = sum(REGION=="western", na.rm=TRUE), n_all_region = sum(REGION!="", na.rm=TRUE))
	birds_grouped$n_nonwestern <- birds_grouped$n_all_region - birds_grouped$n_western
	birds_grouped$proportion_women <- birds_grouped$n_women / c(birds_grouped$n_women + birds_grouped$n_men)
	birds_grouped$proportion_nonwestern <- birds_grouped$n_nonwestern / c(birds_grouped$n_nonwestern + birds_grouped$n_western)
	birds_grouped$proportion_women_per_new_species <- birds_grouped$n_women / birds_grouped$n_species
	birds_grouped$proportion_nonwestern_per_new_species <- birds_grouped$n_nonwestern / birds_grouped$n_species
	birds_grouped$proportion_western_per_new_species <- birds_grouped$n_western / birds_grouped$n_species
	birds_grouped$proportion_men_per_new_species <- birds_grouped$n_men / birds_grouped$n_species
	return(birds_grouped)
}

add_future <- function(birds_grouped, maxyear=2400) {
	birds_future <- data.frame(YEAR=seq(from=1+max(birds_grouped$YEAR), to=maxyear, by=1))	
	birds_grouped <- birds_grouped |> full_join(birds_future, by="YEAR")
	return(birds_grouped)
}

plot_women <- function(birds_grouped, fullrange=FALSE) {
	g <- ggplot(birds_grouped, aes(x=YEAR, y=proportion_women)) + geom_point(alpha=0.5) + geom_smooth(data=subset(birds_grouped, YEAR>=1900), method="lm", fullrange=fullrange) + labs("Proportion of women in eponyms", x="Year", y="Proportion of women") + theme_minimal() + ylim(0, 1) + geom_hline(yintercept=0.5, linetype="dashed", color="red") + theme(legend.position = "none")
	ggsave(g, filename=ifelse(!fullrange, "output/women_now.jpg", "output/women_future.jpg"), width=6, height=4, dpi=300)
	return(g)
}

# Population of Europe + Canada + USA + Australia + New Zealand / World population, based on https://www.worldometers.info/world-population/ . Mexico is not considered western in the paper. 
proportion_nonwestern <- function() {
	return(1-(744575504+347275807+40126723+26974026+5251899) / 8213958603)
}

plot_nonwestern <- function(birds_grouped, fullrange=FALSE) {
	g <- ggplot(birds_grouped, aes(x=YEAR, y=proportion_nonwestern)) + geom_point(alpha=0.5) + geom_smooth(data=subset(birds_grouped, YEAR>=1900), method="lm", fullrange=fullrange) + labs("Proportion of non-Western people in eponyms", x="Year", y="Proportion of non-Western") + theme_minimal() + ylim(0, 1) + geom_hline(yintercept=0.5, linetype="dashed", color="red") + geom_hline(yintercept=proportion_nonwestern(), linetype="dashed", color="red")+ theme(legend.position = "none") 
	ggsave(g, filename=ifelse(!fullrange, "output/nonwestern_now.jpg", "output/nonwestern_future.jpg"), width=6, height=4, dpi=300)
	return(g)
}

plot_new_birds_over_time <- function(birds_grouped) {
	g <- ggplot(birds_grouped, aes(x=YEAR, y=n_species)) + geom_point(alpha=0.5) + 
	geom_smooth(data=subset(birds_grouped, YEAR>=1900), color="purple") + 
	geom_smooth(data=subset(birds_grouped, YEAR>=1900), method="lm") + 

	labs("Number of new birds named per year", x="Year", y="Number of new birds") + theme_minimal() + theme(legend.position = "none")
	ggsave(g, filename="output/new_birds.jpg", width=6, height=4, dpi=300)
	return(g)
}

## github copilot helped substantially with this function
get_predictions <- function(birds_grouped_recent, focal_column, target=0.5) {
	lm_data <- birds_grouped_recent[, c("YEAR", focal_column)]
	colnames(lm_data) <- c("YEAR", "focal_column")
	# Fit a linear model to the data
	model <- lm(focal_column ~ YEAR, data = lm_data)
	
	# Get the coefficients of the model
	intercept <- coef(model)[1]
	slope <- coef(model)[2]
	
	# Calculate the year when the target value is reached
	target_year <- (target - intercept) / slope
	

  	# Create a data frame for prediction
  	prediction_data <- data.frame(YEAR = seq(from = max(birds_grouped_recent$YEAR)+1, to = 3025, by = 1), focal_column=NA)
  
	predictions <- data.frame()
	
  	# Get predictions with confidence intervals
  	prediction_with_ci <- predict(model, newdata = prediction_data, interval = "confidence")
  
  	# Combine predictions into a data frame
  	predictions <- data.frame(
    YEAR = prediction_data$YEAR,
    PREDICTION = prediction_with_ci[, "fit"],
    LOWER_CI = prediction_with_ci[, "lwr"],
    UPPER_CI = prediction_with_ci[, "upr"]
  )
  predictions$PREDICTION[predictions$PREDICTION>1] <- 1
  predictions$LOWER_CI[predictions$LOWER_CI>1] <- 1
  predictions$UPPER_CI[predictions$UPPER_CI>1] <- 1
  
  predictions$PREDICTION[predictions$PREDICTION<0] <- 0
  predictions$LOWER_CI[predictions$LOWER_CI<0] <- 0
  predictions$UPPER_CI[predictions$UPPER_CI<0] <- 0
	
	return(predictions)	
}

get_prediction_range <- function(predictions, target=0.5) {
	# Get the range of years when the target value is reached
	expectation <- round(predictions$YEAR[predictions$PREDICTION >= target][1])
	lowest <- round(predictions$YEAR[predictions$UPPER_CI >= target][1])
	highest <- round(predictions$YEAR[predictions$LOWER_CI >= target][1])
	
	# Return the range of years
	return(paste0(expectation, " (", lowest, ", ", highest, ")"))
}

get_total_number_of_eponymous_species_of_each_kind <- function(birds_grouped, women=prediction_proportion_new_species_women, men=prediction_proportion_new_species_men, nonwestern=prediction_proportion_new_species_nonwestern, western=prediction_proportion_new_species_western, last_ten_year_average) {
	prediction_df <- data.frame(YEAR=seq(from=min(women$YEAR), to=max(women$YEAR), by=1), n_species=last_ten_year_average, n_women=NA, n_men=NA, n_nonwestern=NA, n_western=NA)
	for (row_index in sequence(nrow(prediction_df))) {
		n_species_local <- prediction_df$n_species[row_index]
		n_women_local <- n_species_local * women$PREDICTION[row_index]
		n_men_local <- n_species_local * men$PREDICTION[row_index]
		if(n_women_local+n_men_local > n_species_local) {
			correction_term <- 	n_species_local / (n_women_local+n_men_local)
			n_women_local <- n_women_local * correction_term
			n_men_local <- n_men_local * correction_term
		}
		
		n_nonwestern_local <- n_species_local * nonwestern$PREDICTION[row_index]
		n_western_local <-n_species_local * western$PREDICTION[row_index]	
		if(n_nonwestern_local+n_western_local > n_species_local) {
			correction_term <- 	n_species_local / (n_nonwestern_local+n_western_local)
			n_nonwestern_local <- n_nonwestern_local * correction_term
			n_western_local <- n_western_local * correction_term
		}
		
		prediction_df$n_women <- n_women_local
		prediction_df$n_men <- n_men_local
		prediction_df$n_nonwestern <- n_nonwestern_local
		prediction_df$n_western <- n_western_local
	}
	all_results <- bind_rows(birds_grouped, prediction_df)
	all_results$cumulative_women <- cumsum(all_results$n_women)
	all_results$cumulative_men <- cumsum(all_results$n_men)
	all_results$cumulative_nonwestern <- cumsum(all_results$n_nonwestern)
	all_results$cumulative_western <- cumsum(all_results$n_western)
	return(all_results)
}

convert_longer <- function(full_predictions) {
	local_predictions <- full_predictions[, c("YEAR", "cumulative_women", "cumulative_men", "cumulative_nonwestern", "cumulative_western")]
	pivoted_predictions <- pivot_longer(local_predictions, c( "cumulative_women", "cumulative_men", "cumulative_nonwestern", "cumulative_western"), names_to="kind", values_to="n_species")
	return(pivoted_predictions)
}

plot_full_predictions <- function(pivoted_predictions, filter_to_present=FALSE) {
	focal_breaks <- c(1758, 1900, 2025, 2525, 3025)
	if(filter_to_present) {
		pivoted_predictions <- pivoted_predictions |> filter(YEAR <= 2022)
		focal_breaks <- c(1758, 1900, 2022)
	}
	pivoted_predictions$kind <- stringr::str_to_title(gsub("cumulative_", "", pivoted_predictions$kind))
	
	label_positions <- pivoted_predictions %>%
    group_by(kind) %>%
    filter(YEAR == max(YEAR)) 
	
	g <- ggplot(pivoted_predictions, aes(x=YEAR, y=n_species, color=kind)) + geom_line() + labs("Total number of eponymous birds of each type over time", x="Year", y="Number of  birds with eponymous names") + theme_minimal() + scale_x_continuous(breaks=focal_breaks, expand = expansion(mult = c(0, 0.2))) + geom_text_repel(
      data = label_positions,
      aes(label = kind),
      hjust = -0.1,  # Slightly offset the labels to the right
      size = 3
    ) + theme(legend.position="none") + scale_color_brewer(palette = "Dark2")
	ggsave(g, filename=ifelse(filter_to_present, "output/full_predictions_present.jpg", "output/full_predictions_future.jpg"), width=6, height=4, dpi=300)
	return(g)
}