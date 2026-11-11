library(tidyverse)
library(sf)
library(tigris)
library(viridis)
library(scales)

options(tigris_use_cache = TRUE)

# Load the New Orleans datasets
business_new_orleans <- readRDS(
  "data/new_orleans/business_new_orleans.rds"
)

reviews_new_orleans <- readRDS(
  "data/new_orleans/reviews_new_orleans.rds"
)

# Find restaurant categories
restaurant_categories_long <- business_new_orleans |>
  filter(
    !is.na(categories),
    str_detect(
      categories,
      regex("Restaurants", ignore_case = TRUE)
    )
  ) |>
  separate_rows(categories, sep = ",") |>
  mutate(
    category = str_to_lower(str_squish(categories))
  )

# Display the most common categories
restaurant_categories <- restaurant_categories_long |>
  count(category, sort = TRUE)

print(head(restaurant_categories, 30))

# Choose related New Orleans restaurant categories
selected_categories <- c(
  "cajun/creole",
  "seafood",
  "southern"
)

# Select businesses in those categories
selected_restaurants <- restaurant_categories_long |>
  filter(category %in% selected_categories) |>
  distinct(business_id) |>
  inner_join(
    business_new_orleans,
    by = "business_id"
  )

print(dim(selected_restaurants))

# Join the selected restaurants to their reviews
selected_reviews <- reviews_new_orleans |>
  semi_join(
    selected_restaurants,
    by = "business_id"
  )

print(dim(selected_reviews))

# Create a table of selected restaurant categories
category_table <- selected_restaurants |>
  summarise(
    restaurants = n(),
    average_rating = mean(stars, na.rm = TRUE)
  )

print(category_table)

# Download official Louisiana place boundaries
la_places <- tigris::places(
  state = "LA",
  cb = TRUE,
  year = 2024,
  class = "sf"
)

# Convert restaurant longitude and latitude into map points
restaurant_points <- selected_restaurants |>
  filter(
    !is.na(longitude),
    !is.na(latitude)
  ) |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# Match restaurants to official place boundaries
restaurants_with_place <- st_join(
  restaurant_points,
  la_places |>
    st_transform(4326) |>
    select(place_name = NAME),
  join = st_within,
  left = TRUE
)

# Check the official place names
place_table <- restaurants_with_place |>
  st_drop_geometry() |>
  count(place_name, sort = TRUE)

print(place_table)

# Map of selected restaurants in New Orleans
ggplot() +
  geom_sf(
    data = la_places |>
      st_transform(4326),
    fill = "gray95",
    color = "gray50"
  ) +
  geom_sf(
    data = restaurant_points,
    aes(color = stars),
    alpha = 0.6,
    size = 1
  ) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Yelp rating"
  ) +
  labs(
    title = "Cajun, Creole, Seafood, and Southern Restaurants",
    subtitle = "New Orleans Yelp businesses",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

category_summary <- restaurant_categories_long |>
  filter(category %in% selected_categories) |>
  distinct(business_id, category) |>
  inner_join(
    business_new_orleans |>
      select(business_id, stars),
    by = "business_id"
  ) |>
  group_by(category) |>
  summarise(
    restaurants = n(),
    average_rating = round(mean(stars, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(desc(restaurants))

print(category_summary)

# Download two geographic systems
zcta_boundaries <- tigris::zctas(
  cb = TRUE,
  year = 2020,
  class = "sf"
) |>
  st_transform(4326)

tract_boundaries <- tigris::tracts(
  state = "LA",
  county = "Orleans",
  cb = TRUE,
  year = 2020,
  class = "sf"
) |>
  st_transform(4326)

# Assign each restaurant to a ZIP Code Tabulation Area
restaurants_with_zcta <- st_join(
  restaurant_points,
  zcta_boundaries |>
    select(zcta = ZCTA5CE20),
  join = st_within,
  left = TRUE
)

# Assign each restaurant to a Census tract
restaurants_with_tract <- st_join(
  restaurant_points,
  tract_boundaries |>
    select(tract = GEOID),
  join = st_within,
  left = TRUE
)

# Create a comparison table
geography_summary <- tibble(
  geography = c("ZIP Code Tabulation Areas", "Census tracts"),
  restaurants = c(
    nrow(restaurants_with_zcta),
    nrow(restaurants_with_tract)
  ),
  geographic_areas = c(
    n_distinct(restaurants_with_zcta$zcta, na.rm = TRUE),
    n_distinct(restaurants_with_tract$tract, na.rm = TRUE)
  ),
  unmatched_restaurants = c(
    sum(is.na(restaurants_with_zcta$zcta)),
    sum(is.na(restaurants_with_tract$tract))
  )
)

print(geography_summary)

# ZIP Code Tabulation Area map
zip_map <- ggplot() +
  geom_sf(
    data = zcta_boundaries,
    fill = "gray95",
    color = "gray60",
    linewidth = 0.2
  ) +
  geom_sf(
    data = restaurants_with_zcta,
    aes(color = stars),
    alpha = 0.6,
    size = 1
  ) +
  scale_color_viridis_c(name = "Yelp rating") +
  labs(
    title = "Selected Restaurants by ZIP Code Area",
    subtitle = "New Orleans Cajun, Creole, Seafood, and Southern restaurants"
  ) +
  theme_minimal()

print(zip_map)

# Census tract map
tract_map <- ggplot() +
  geom_sf(
    data = tract_boundaries,
    fill = "gray95",
    color = "gray60",
    linewidth = 0.2
  ) +
  geom_sf(
    data = restaurants_with_tract,
    aes(color = stars),
    alpha = 0.6,
    size = 1
  ) +
  scale_color_viridis_c(name = "Yelp rating") +
  labs(
    title = "Selected Restaurants by Census Tract",
    subtitle = "New Orleans Cajun, Creole, Seafood, and Southern restaurants"
  ) +
  theme_minimal()

print(tract_map)