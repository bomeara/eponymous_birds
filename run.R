source("_targets.R")

tar_make()

tar_load_everything()

print("Women reach 50% of new eponymous species:")
print(prediction_years_women)

print("Nonwestern reach 50% of new eponymous species:")
print(prediction_years_nonwestern)

print("Nonwestern reach proprtional number of new eponymous species (population weighted):")
print(prediction_years_nonwestern_population)