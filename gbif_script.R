library(tidyverse)
library(raster)
library(lubridate)
#donkey <- read_csv("observations-201459.csv")
#ducks <- read_csv("observations-201466.csv")

list_of_gbif_files <-
  list.files("data/gbif/", full.names = TRUE) %>% str_subset("210914110416597")



read_delim("data/gbif/Fire ephemeral species/0070546-210914110416597.csv")

all <-
  bind_rows(lapply(list_of_gbif_files,read_delim))


burn_data_gbif <- function(path, species_df, buffer_size = 1000) {
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
    species_df <- dplyr::filter(species_df, year >= 2003) %>%
      dplyr::filter(coordinateUncertaintyInMeters < 1000 |
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
      group_by(species,
               occurrenceID,
               eventDate,
               coordinateUncertaintyInMeters,
               countryCode,
               year) %>%
      summarize(date_difference = min(date_differnce, na.rm = T)) -> most_recent_fire_per_obs
    return(most_recent_fire_per_obs)
  }


androclava<-filter(all,genus=="Androcalva")
one <- calculate_burn_obs(list_of_files, species_df = androclava,cores = 4)

one %>%
  mutate(fire = (date_difference < 1825)) %>%
  mutate(sp = word(species, 1, 2)) %>%
  ggplot(aes(x = as_date(eventDate), fill = fire)) + geom_histogram() +
  facet_grid(sp ~ ., scales = "free")
ggsave("fire_yes.png", height = 20, width = 4)




one %>% dplyr::filter(date_difference < 1825) %>%
  mutate(date_difference_months = date_difference/365.25*12) %>%
  mutate(sp = word(scientificName, 1, 2)) %>%
  ggplot(aes(x = date_difference_months)) + geom_histogram() + facet_grid(sp ~
                                                                                  ., scales = "free")
ggsave("juve.png", height = 20, width = 4)

one %>%
  group_by(sp=word(species, 1, 2)) %>%
  summarise(prop_fire = sum(date_difference < 1825) / n(),n=n()) %>%
  arrange(prop_fire)->out


one %>% dplyr::filter(date_difference < 1825) %>%
  mutate(sp = word(species, 1, 2)) %>%
  mutate(date_difference_years = date_difference/365.25) %>%
  dplyr::filter(sp %in% c("Androcalva rosea","Androcalva reticulata","Androcalva pearnii","Androcalva loxophylla")) %>%
  ggplot(aes(x = date_difference_years)) + 
  geom_histogram() + facet_grid(sp ~ ., scales = "free")+
  theme_bw()+xlab("Time from fire to collection / observation (years)")
ggsave("selected_species.png",height=11,width=6)



