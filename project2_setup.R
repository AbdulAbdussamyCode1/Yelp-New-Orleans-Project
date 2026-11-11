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

# Free memory before processing the large review file
rm(business_raw, business)
gc()

# Get the business IDs for New Orleans
new_orleans_ids <- business_new_orleans |>
  distinct(business_id) |>
  pull(business_id)

# Store only reviews connected to New Orleans businesses
review_chunks <- list()
chunk_number <- 0L

review_handler <- function(chunk) {
  
  filtered_chunk <- chunk |>
    filter(business_id %in% new_orleans_ids) |>
    select(
      business_id,
      user_id,
      stars,
      date,
      text,
      useful,
      funny,
      cool
    )
  
  if (nrow(filtered_chunk) > 0) {
    chunk_number <<- chunk_number + 1L
    review_chunks[[chunk_number]] <<- filtered_chunk
  }
}

# Read the 5 GB review file in chunks
review_connection <- file(
  "data/raw/yelp_academic_dataset_review.json",
  open = "r"
)

jsonlite::stream_in(
  review_connection,
  handler = review_handler,
  pagesize = 1000,
  verbose = FALSE
)

close(review_connection)

# Combine the filtered review chunks
reviews_new_orleans <- bind_rows(review_chunks)

# Check the result
dim(reviews_new_orleans)

# Save the filtered reviews as an RDS file
saveRDS(
  reviews_new_orleans,
  "data/new_orleans/reviews_new_orleans.rds"
)