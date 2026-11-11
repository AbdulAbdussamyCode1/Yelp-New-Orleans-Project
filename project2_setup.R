library(jsonlite)
library(dplyr)

# Import the Yelp business data
business_raw <- jsonlite::stream_in(
  file("data/raw/yelp_academic_dataset_business.json"),
  verbose = FALSE
)

# Keep the columns needed for the project
business <- business_raw |>
  select(
    business_id,
    name,
    address,
    city,
    state,
    postal_code,
    latitude,
    longitude,
    stars,
    review_count,
    categories
  )

# Filter for New Orleans businesses
business_new_orleans <- business |>
  filter(
    city == "New Orleans",
    state == "LA"
  )

# Save the filtered data as an RDS file
dir.create("data/new_orleans", recursive = TRUE, showWarnings = FALSE)

saveRDS(
  business_new_orleans,
  "data/new_orleans/business_new_orleans.rds"
)

# Confirm the result
dim(business_new_orleans)