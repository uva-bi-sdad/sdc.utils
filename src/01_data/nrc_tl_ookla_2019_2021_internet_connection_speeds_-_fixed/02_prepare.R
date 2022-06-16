# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
2019-01-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2019-01-01_performance_fixed_tiles.zip")
2020-01-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2020-01-01_performance_fixed_tiles.zip")
2021-01-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2021-01-01_performance_fixed_tiles.zip")
2022-01-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2022-01-01_performance_fixed_tiles.zip")
2019-04-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2019-04-01_performance_fixed_tiles.zip")
2020-04-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2020-04-01_performance_fixed_tiles.zip")
2021-04-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2021-04-01_performance_fixed_tiles.zip")
2019-07-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2019-07-01_performance_fixed_tiles.zip")
2020-07-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2020-07-01_performance_fixed_tiles.zip")
2021-07-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2021-07-01_performance_fixed_tiles.zip")
2019-10-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2019-10-01_performance_fixed_tiles.zip")
2020-10-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2020-10-01_performance_fixed_tiles.zip")
2021-10-01_performance_fixed_tiles <- sf::st_read("data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/original/2021-10-01_performance_fixed_tiles.zip")

nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed <- 

# Assign geoid
nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed$geoid <- ""

# Assign region_type
nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed$region_type <- "Tile"

# Assign region_name
nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed$region_name <- ""

# Assign year
nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed$year <- "2019"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed[, c("geoid", "region_name", "region_type", "year", "geometry", "measure", "measure_unit", "measure_type", "value")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(final_dataset_simplified, "data/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed/distribution/nrc_tl_ookla_2019_2021_internet_connection_speeds_-_fixed.geojson")

# Update file manifest
data_file_checksums()
