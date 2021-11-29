library(tidyverse)
library(raster)
library(lubridate)
#donkey <- read_csv("observations-201459.csv")
#ducks <- read_csv("observations-201466.csv")
lizards <- read_delim("0067310-210914110416597.csv")
pinks <- read_delim("0067573-210914110416597.csv")
flannel <- read_delim("0067517-210914110416597.csv")
mistletoe <- read_delim("0067565-210914110416597.csv")
christmas1 <- read_delim("0067620-210914110416597.csv")
christmas2 <- read_delim("0067628-210914110416597.csv")
ducks <- read_delim("0067624-210914110416597.csv")
bitou <- read_delim("0067720-210914110416597.csv")
cheese <- read_delim("0067724-210914110416597.csv")

all <-
  bind_rows(flannel,
            mistletoe,
            pinks,
            christmas1,
            christmas2,
            ducks,
            bitou,
            cheese,
            lizards)


burn_data_gbif <- function(path, species_df, buffer_size = 2000) {
  require(sf)
  require(lubridate)
  species_df <- dplyr::filter(species_df, !is.na(decimalLatitude))
  species_df <- dplyr::filter(species_df, year >= 2003)
  all_numbers <- str_extract(path, "(?<=A)[0-9]*")
  year <- substr(all_numbers, start = 1, stop = 4)
  julian_date <- substr(all_numbers, start = 5, stop = 7)
  sp <-
    SpatialPoints(cbind(species_df$decimalLongitude, species_df$decimalLatitude))
  fire_raster <- raster::raster(path)
  clip1 <-
    crop(fire_raster, extent(sp)) #this speeds things up a lot
  species_df$burn_data_julian_date <-
    raster::extract(clip1,
                    sp, buffer = buffer_size, fun = max)
  species_df$fire_year <- year
  fire_date <- as.Date(paste0(year, "-01-01"))
  yday(fire_date) <- as.numeric(julian_date)
  species_df$fire_date <- fire_date
  species_df$fire_month <- month(fire_date)
  return(species_df)
}


list_of_files <-
  list.files("Win20", recursive =  TRUE, full.names = TRUE) %>% str_subset("burndate")

#testing first function
Sys.time()
z <- burn_data_gbif(list_of_files[[1]], species_df = pinks)
Sys.time()


calculate_burn_obs <-
  function(list_of_files = list_of_files,
           species_df = species_df,
           cores = 1) {
    require(parallel)
    species_df <- dplyr::filter(species_df, year >= 2002) %>%
      dplyr::filter(coordinateUncertaintyInMeters < 2000 |
                      is.na(coordinateUncertaintyInMeters))
    output <-
      mclapply(list_of_files,
               burn_data_gbif,
               species_df = species_df,
               mc.cores = cores)
    big_df <- bind_rows(output)
    big_df$fire_date[big_df$burn_data_julian_date <= 0] <- NA
    big_df$date_differnce <-
      as.numeric(as_date(big_df$eventDate) - big_df$fire_date)
    big_df$date_differnce[big_df$date_differnce < 0] <- NA
    
    big_df %>%
      group_by(scientificName,
               occurrenceID,
               eventDate,
               coordinateUncertaintyInMeters,
               year) %>%
      summarize(date_difference = min(date_differnce, na.rm = T)) -> most_recent_fire_per_obs
    return(most_recent_fire_per_obs)
  }

one <- calculate_burn_obs(list_of_files, species_df = all)

one %>%
  mutate(fire = (date_difference < 730)) %>%
  mutate(sp = word(scientificName, 1, 2)) %>%
  ggplot(aes(x = as_date(eventDate), fill = fire)) + geom_histogram() +
  facet_grid(sp ~ ., scales = "free")
ggsave("fire_yes.png", height = 11, width = 3)

one %>% dplyr::filter(date_difference < 730) %>%
  mutate(sp = word(scientificName, 1, 2)) %>%
  ggplot(aes(x = date_difference, fill = year)) + geom_histogram() + facet_grid(sp ~
                                                                                  ., scales = "free")
ggsave("juve.png", height = 11, width = 3)

one %>%
  group_by(word(scientificName, 1, 2)) %>%
  summarise(prop_fire = sum(date_difference < 730) / n()) %>%
  arrange(prop_fire)
