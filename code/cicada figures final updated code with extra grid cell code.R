#load all packages
library(tidyverse)
library(dplyr)
library(viridis)
library(maps)
library(sf)
library(forcats)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(patchwork)
library(tidyr)
library(stringr)


#load all data
#load all inat records
cicada <- read_csv("data/30-7-25_cicadas_inat.csv")

#load all specimen data from ALA
cicada_spec_0 <- read_csv("data/31-7-25_cicadas_specimens.csv")


#get rid of extra-Australian specimen records that have been incorrectly geocoordinated but still appear in download due to assigned state/territory name
cicada_spec <- cicada_spec_0 %>%
  filter(decimalLongitude >= 105 & decimalLongitude <= 160 & decimalLatitude<=-9)


#remove records with coordinate uncertainty 50,000 m or greater
cicada <- cicada %>%
  filter(positional_accuracy < 50000 | is.na(positional_accuracy))

cicada_spec <- cicada_spec %>%
  filter(coordinateUncertaintyInMeters < 50000 | is.na(coordinateUncertaintyInMeters))


#filter the inat data for records that have audio (this includes records that have both audio and photo(s), but former is the bare minimum)

sound <- cicada %>%
  filter(!is.na(sound_url))

#filter the inat data for records that have photos only
photo <- cicada %>%
  filter(is.na(sound_url))


#get audio records identified to species only
sound_non_missing <- sound %>%
  filter(!is.na(taxon_species_name)) 

sound_sf <- sound_non_missing %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



#first major figure
#frequency histogram of audio records per species
#ordered from most observed to least,only includes species with at least one audio record

species_counts <- sound_non_missing %>%
  count(taxon_species_name) %>%
  mutate(taxon_species_name = fct_reorder(taxon_species_name, n, .desc = TRUE))

ggplot(species_counts, aes(x = taxon_species_name, y = n, fill = taxon_species_name)) +
  geom_col() +
  labs(
    x = "Species",
    y = "Count",
    title = "Species Frequency Distribution"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_continuous(
    limits = c(0, 1200),
    breaks = seq(0, 1200, by = 200),
    expand = c(0, 0)
  ) +
  scale_fill_viridis_d(option = "viridis")



#second major figure
#heatmap of occurrences of where sound recordings are made


#convert audio data to sf 
sound_sf <- st_as_sf(sound, coords = c("longitude", "latitude"), crs = 4326)

#get Australia state boundaries as sf
aus_states_sf <- ne_states(country = "Australia", returnclass = "sf")

#1x1 degree grid over Australia
grid <- st_make_grid(
  aus_states_sf,
  cellsize = 1,        
  square = TRUE
) %>%
  st_sf(grid_id = 1:length(.))  

#assign each record to a grid cell
sound_joined <- st_join(sound_sf, grid, left = FALSE)

#count records in each grid cell
sound_counts <- sound_joined %>%
  st_drop_geometry() %>%
  count(grid_id, name = "count")

#join counts back to the grid
sound_binned_sf <- grid %>%
  left_join(sound_counts, by = "grid_id")

#filter out empty cells (with NA count)
sound_binned_sf_nonzero <- sound_binned_sf %>%
  filter(!is.na(count))

#world map of Australia for background polygons
australia_map <- map_data("world", region = "Australia")

#plot everything
ggplot() +
  geom_sf(data = sound_binned_sf_nonzero, aes(fill = count)) +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group),
               color = "black", fill = NA) +
  geom_sf(data = aus_states_sf, fill = NA, color = "black", size = 0.6) +
  scale_fill_viridis(
    name = "Number of records",
    option = "viridis",
    trans = "log",
    limits = c(1, 3500),
    breaks = c(1, 10, 100, 1000, 3500),
    labels = scales::comma_format(),
    guide = guide_colorbar(
      ticks.colour = "black",
      barwidth = 3,
      barheight = 14,
      ticks.linewidth = 0.7,
      ticks.length = unit(0.3, "cm")
    )
  ) +
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
  theme_classic()



ggplot() +
  # Step 1: draw all grid cells, including those with NA, using fill with default pale green for NA
  geom_sf(data = sound_binned_sf, aes(fill = count), color = "grey20", size = 0.3) +
  
  # Step 2: draw polygon for Australia outline (for visual context)
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group),
               color = "black", fill = NA) +
  
  # Step 3: state boundaries
  geom_sf(data = aus_states_sf, fill = NA, color = "black", size = 0.6) +
  
  # Step 4: fill scale with NA fill color and legend
  scale_fill_viridis(
    name = "Number of records",
    option = "viridis",
    trans = "log",
    limits = c(1, 3500),
    breaks = c(1, 10, 100, 1000, 3500),
    labels = scales::comma_format(),
    na.value = "palegreen",  # <- color for NA (your first requirement)
    guide = guide_colorbar(
      ticks.colour = "black",
      barwidth = 3,
      barheight = 14,
      ticks.linewidth = 0.7,
      ticks.length = unit(0.3, "cm")
    )
  ) +
  
  # Coordinate limits for Australia
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
  
  # Clean theme
  theme_classic()




# Convert audio data to sf
sound_sf <- st_as_sf(sound, coords = c("longitude", "latitude"), crs = 4326)

# Get Australia state boundaries as sf
aus_states_sf <- ne_states(country = "Australia", returnclass = "sf")

# Create 1x1 degree grid over bounding box
grid <- st_make_grid(
  aus_states_sf,
  cellsize = 1,
  square = TRUE
) %>%
  st_sf(grid_id = 1:length(.))

# Keep only grid cells that intersect Australia's landmass
grid_on_land <- grid[st_intersects(grid, st_union(aus_states_sf), sparse = FALSE), ]

# Assign each record to a grid cell
sound_joined <- st_join(sound_sf, grid_on_land, left = FALSE)

# Count records in each grid cell
sound_counts <- sound_joined %>%
  st_drop_geometry() %>%
  count(grid_id, name = "count")

# Join counts back to the land-only grid
sound_binned_sf <- grid_on_land %>%
  left_join(sound_counts, by = "grid_id")

# Background map outline
australia_map <- map_data("world", region = "Australia")

# Plot
ggplot() +
  # Grid cells (both with and without data)
  geom_sf(data = sound_binned_sf, aes(fill = count), color = "grey20", size = 0.3) +
  
  # Outline of Australia for context
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group),
               color = "black", fill = NA) +
  
  # State borders
  geom_sf(data = aus_states_sf, fill = NA, color = "black", size = 0.6) +
  
  # Color scale with pale green for NA cells
  scale_fill_viridis(
    name = "Number of records",
    option = "viridis",
    trans = "log",
    limits = c(1, 3500),
    breaks = c(1, 10, 100, 1000, 3500),
    labels = scales::comma_format(),
    na.value = "palegreen",  # <- this fills NA cells that are on land
    guide = guide_colorbar(
      ticks.colour = "black",
      barwidth = 3,
      barheight = 14,
      ticks.linewidth = 0.7,
      ticks.length = unit(0.3, "cm")
    )
  ) +
  
  # Map extent
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
  
  # Theme
  theme_classic()




#note that I found it too time-consuming to replicate this for Christmas and Lord Howe Islands, where there are some audio records, so I just manually figured out 
#what those two grid cells should be and added to figure in post-production (each island = one grid cell only)



#third major figure
#another heat map, based off same underlying code as figure above
#but this is plotting cells that have specimen records, but no audio records
#unlike the above heatmap which uses all audio records, this figure uses only records identified to species (across all datasets)



#filter data and organise new dataset (from specimens and photos dataset, get specimens only and get species only)
non_audio <- cicada_spec |>
  dplyr::filter(!is.na(species) & nzchar(species)) |>
  dplyr::select(
    species,
    longitude = decimalLongitude,
    latitude = decimalLatitude
  )



sound2 <- sound |>
  dplyr::filter(!is.na(taxon_species_name)) |>
  dplyr::select(
    species = taxon_species_name,
    longitude,
    latitude
  )

#convert to sf
non_audio_sf <- sf::st_as_sf(non_audio, coords = c("longitude", "latitude"), crs = 4326)
sound_sf <- sf::st_as_sf(sound2, coords = c("longitude", "latitude"), crs = 4326)

#create grid
aus_states_sf <- rnaturalearth::ne_states(country = "Australia", returnclass = "sf")

grid_raw <- sf::st_make_grid(
  aus_states_sf,
  cellsize = 1,
  square = TRUE
)

grid <- sf::st_sf(grid_id = seq_along(grid_raw), geometry = grid_raw)

#spatial join and count for non-audio
non_audio_joined <- sf::st_join(non_audio_sf, grid, left = FALSE)

non_audio_counts <- non_audio_joined |>
  sf::st_drop_geometry() |>
  dplyr::group_by(grid_id) |>
  dplyr::summarise(
    non_audio_count = dplyr::n(),
    species_richness = dplyr::n_distinct(species),
    .groups = "drop"
  )

#spatial join and count for audio records
sound_joined <- sf::st_join(sound_sf, grid, left = FALSE)

sound_counts <- sound_joined |>
  sf::st_drop_geometry() |>
  dplyr::count(grid_id, name = "sound_count")

#combine into one grid
grid_data <- grid |>
  dplyr::left_join(non_audio_counts, by = "grid_id") |>
  dplyr::left_join(sound_counts, by = "grid_id") |>
  dplyr::mutate(
    has_non_audio = !is.na(non_audio_count),
    has_sound = !is.na(sound_count),
    category = dplyr::case_when(
      has_sound & has_non_audio ~ "both",
      has_sound & !has_non_audio ~ "sound_only",
      !has_sound & has_non_audio ~ "non_audio_only",
      TRUE ~ NA_character_
    )
  )



check<-filter(grid_data, category == "both")
check<-filter(sound_binned_sf, count>0)

#background map
australia_map <- ggplot2::map_data("world", region = "Australia")

#plot everything
ggplot2::ggplot() +
  #non-audio only: coloured by richness
  ggplot2::geom_sf(
    data = dplyr::filter(grid_data, category == "non_audio_only"),
    ggplot2::aes(fill = species_richness),
    color = NA
  ) +
  #sound data overlay (sound-only and both)
  ggplot2::geom_sf(
    data = dplyr::filter(grid_data, category %in% c("sound_only", "both")),
    fill = "palegreen", alpha = 0.4, color = NA
  ) +
  #map outlines
  ggplot2::geom_polygon(
    data = australia_map,
    ggplot2::aes(x = long, y = lat, group = group),
    color = "black", fill = NA
  ) +
  ggplot2::geom_sf(data = aus_states_sf, fill = NA, color = "black", size = 0.6) +
  #colour scale
  viridis::scale_fill_viridis(
    name = "Species richness",
    option = "plasma",
    breaks = seq(0, max(grid_data$species_richness, na.rm = TRUE), by = 5),
    na.value = NA,
    guide = ggplot2::guide_colorbar(
      ticks.colour = "black",
      barwidth = 3,
      barheight = 14,
      ticks.linewidth = 0.7,
      ticks.length = grid::unit(0.3, "cm")
    )
  ) +
  ggplot2::coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
  ggplot2::theme_classic()


#note that I found it too time-consuming to replicate this for Norfolk Island, where there are some photo/specimen records but no audio records, 
#so I just manually figured out what that grid cell should be and added to figure in post-production (island = one grid cell only)






#same thing as above, but compare photos to audio, instead of specimens

#filter data and organise new dataset (from specimens and photos dataset, get photos only and get species only)
non_audio <- photo |>
  dplyr::filter(!is.na(taxon_species_name)) |>
  dplyr::select(
    species = taxon_species_name,
    longitude,
    latitude
  )



sound2 <- sound |>
  dplyr::filter(!is.na(taxon_species_name)) |>
  dplyr::select(
    species = taxon_species_name,
    longitude,
    latitude
  )

#convert to sf
non_audio_sf <- sf::st_as_sf(non_audio, coords = c("longitude", "latitude"), crs = 4326)
sound_sf <- sf::st_as_sf(sound2, coords = c("longitude", "latitude"), crs = 4326)

#create grid
aus_states_sf <- rnaturalearth::ne_states(country = "Australia", returnclass = "sf")

grid_raw <- sf::st_make_grid(
  aus_states_sf,
  cellsize = 1,
  square = TRUE
)

grid <- sf::st_sf(grid_id = seq_along(grid_raw), geometry = grid_raw)

#spatial join and count for non-audio
non_audio_joined <- sf::st_join(non_audio_sf, grid, left = FALSE)

non_audio_counts <- non_audio_joined |>
  sf::st_drop_geometry() |>
  dplyr::group_by(grid_id) |>
  dplyr::summarise(
    non_audio_count = dplyr::n(),
    species_richness = dplyr::n_distinct(species),
    .groups = "drop"
  )

#spatial join and count for audio records
sound_joined <- sf::st_join(sound_sf, grid, left = FALSE)

sound_counts <- sound_joined |>
  sf::st_drop_geometry() |>
  dplyr::count(grid_id, name = "sound_count")

#combine into one grid
grid_data <- grid |>
  dplyr::left_join(non_audio_counts, by = "grid_id") |>
  dplyr::left_join(sound_counts, by = "grid_id") |>
  dplyr::mutate(
    has_non_audio = !is.na(non_audio_count),
    has_sound = !is.na(sound_count),
    category = dplyr::case_when(
      has_sound & has_non_audio ~ "both",
      has_sound & !has_non_audio ~ "sound_only",
      !has_sound & has_non_audio ~ "non_audio_only",
      TRUE ~ NA_character_
    )
  )

#background map
australia_map <- ggplot2::map_data("world", region = "Australia")

#plot everything
ggplot2::ggplot() +
  #non-audio only: coloured by richness
  ggplot2::geom_sf(
    data = dplyr::filter(grid_data, category == "non_audio_only"),
    ggplot2::aes(fill = species_richness),
    color = NA
  ) +
  #sound data overlay (sound-only and both)
  ggplot2::geom_sf(
    data = dplyr::filter(grid_data, category %in% c("sound_only", "both")),
    fill = "palegreen", alpha = 0.4, color = NA
  ) +
  #map outlines
  ggplot2::geom_polygon(
    data = australia_map,
    ggplot2::aes(x = long, y = lat, group = group),
    color = "black", fill = NA
  ) +
  ggplot2::geom_sf(data = aus_states_sf, fill = NA, color = "black", size = 0.6) +
  #colour scale
  viridis::scale_fill_viridis(
    name = "Species richness",
    option = "plasma",
    limits = c(1, 15),
    breaks = seq(0, max(grid_data$species_richness, na.rm = TRUE), by = 5),
    na.value = NA,
    guide = ggplot2::guide_colorbar(
      ticks.colour = "black",
      barwidth = 3,
      barheight = 14,
      ticks.linewidth = 0.7,
      ticks.length = grid::unit(0.3, "cm")
    )
  ) +
  ggplot2::coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
  ggplot2::theme_classic()





#fourth major figure
#building pie charts to see the 'completeness'of each genus re how many species in each genus have audio recordings

#first read in Australian Faunal Directory (AFD) cicada names and do some organisation

taxonomy <- read_csv("data/afd_taxa_31-7-25.csv")

taxonomy2 <- taxonomy %>%
  filter(NAME_TYPE == "Valid Name")

all_genera <- taxonomy2 %>%
  filter(!is.na(GENUS)) %>%
  distinct(GENUS)


all_species <- taxonomy2 %>%
  filter(RANK == "Species") %>%
  mutate(SCIENTIFIC_NAME = str_trim(str_replace(SCIENTIFIC_NAME, "^((\\S+\\s+\\S+)).*", "\\1")))

#how many unique species recorded in the audio dataset
sound_unique <- sound2 %>%
  distinct(species)

#197 species 

#ditto photos (and this includes records that have both audio and photos)

photo_all <- cicada %>%
  filter(!is.na(image_url))

photo3 <- photo_all |>
  dplyr::filter(!is.na(taxon_species_name)) |>
  dplyr::select(
    species = taxon_species_name,
    longitude,
    latitude
  )

photo_unique <- photo3 %>%
  distinct(species)

#263 species


#ditto specimens

spec_unique <- spec %>%
  distinct(species)

#320 for specimens

#how many genera covered by the audio records
sound_genera <- sound_unique %>%
  mutate(genus = word(species, 1)) %>%
  distinct(genus)

#53 out of 100 genera




#some more data wrangling

sound_unique2 <- sound_unique %>%
  mutate(genus = word(species, 1))


all_species_clean <- all_species %>%
  dplyr::select(SCIENTIFIC_NAME, GENUS)


sound_species_list <- sound_unique2 %>%
  dplyr::pull(species)


all_species_clean2 <- all_species_clean %>%
  dplyr::mutate(found_in_sound = SCIENTIFIC_NAME %in% sound_species_list)

species_per_genus_in_sound <- all_species_clean2 %>%
  dplyr::group_by(GENUS) %>%
  dplyr::summarise(
    total_species_in_genus = n_distinct(SCIENTIFIC_NAME),
    species_found_in_sound = sum(found_in_sound),
    .groups = "drop"
  )



#why are there 4 missing species? 193 vs the 197 there should be
missing_species_df <- species_counts %>%
  filter(!taxon_species_name %in% all_species_clean$SCIENTIFIC_NAME)

missing_species_df <- dplyr::select(missing_species_df, taxon_species_name)


#Falcatpsalta aquila --> orthographic variant, it's in AFD as Falcatpsalta aquilus
#Myopsalta wollomombi --> ditto, Myopsalta wollomombii
#Yoyetta bushi --> just described this week, not in AFD
#Yoyetta nathani --> just described this week, not in AFD

#so AFD has 406 species, but as of 24 July 2025, should be 410 (four Yoyetta species, only 2 of them have audio on iNat, others photos only [crepita, corbinorum])
#so need to add those 4 to the AFD list and rerun analysis, and also amend the 2 species names to match iNat (more convenient)

#fix the two values in SCIENTIFIC_NAME
all_species_clean$SCIENTIFIC_NAME[all_species_clean$SCIENTIFIC_NAME == "Falcatpsalta aquilus"] <- "Falcatpsalta aquila"
all_species_clean$SCIENTIFIC_NAME[all_species_clean$SCIENTIFIC_NAME == "Myopsalta wollomombii"] <- "Myopsalta wollomombi"

#create new rows as a df
new_rows <- data.frame(
  SCIENTIFIC_NAME = c("Yoyetta bushi", "Yoyetta nathani", "Yoyetta crepita", "Yoyetta corbinorum"),
  GENUS = rep("Yoyetta", 4),
  stringsAsFactors = FALSE
)

#append new rows
all_species_clean <- rbind(all_species_clean, new_rows)


#now go back and redo that last set of steps

#which species are in audio data
all_species_clean2 <- all_species_clean %>%
  dplyr::mutate(found_in_sound = SCIENTIFIC_NAME %in% sound_species_list)

#summarise by GENUS
species_per_genus_in_sound <- all_species_clean2 %>%
  dplyr::group_by(GENUS) %>%
  dplyr::summarise(
    total_species_in_genus = n_distinct(SCIENTIFIC_NAME),
    species_found_in_sound = sum(found_in_sound),
    .groups = "drop"
  )

#plot 

pie_data <- species_per_genus_in_sound %>%
  dplyr::mutate(
    found_pct = (species_found_in_sound / total_species_in_genus) * 100,
    not_found_pct = 100 - found_pct
  ) %>%
  dplyr::select(GENUS, found_pct, not_found_pct) %>%
  tidyr::pivot_longer(cols = c(found_pct, not_found_pct),
                      names_to = "category", values_to = "percent")

ggplot(pie_data, aes(x = "", y = percent, fill = category)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ GENUS, ncol = 10) +  # Adjust layout here (e.g., ncol = 10 → 10 x 10 grid)
  scale_fill_manual(
    values = c("found_pct" = "#1f77b4", "not_found_pct" = "#d3d3d3"),  # blue and light gray
    labels = c("Species Found", "Species Not Found")
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 6),
    panel.spacing = unit(0.5, "lines")
  )

#redo above plot but with species numbers for each genus in parentheses and fix clipped letters


species_per_genus_in_sound <- species_per_genus_in_sound %>%
  dplyr::mutate(GENUS_LABEL = paste0(GENUS, " (", total_species_in_genus, ")"))


pie_data <- species_per_genus_in_sound %>%
  dplyr::mutate(
    found_pct = (species_found_in_sound / total_species_in_genus) * 100,
    not_found_pct = 100 - found_pct
  ) %>%
  dplyr::select(GENUS_LABEL, found_pct, not_found_pct) %>%
  tidyr::pivot_longer(
    cols = c(found_pct, not_found_pct),
    names_to = "category",
    values_to = "percent"
  )


ggplot(pie_data, aes(x = "", y = percent, fill = category)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ GENUS_LABEL, ncol = 10) +
  scale_fill_manual(
    values = c("found_pct" = "#1f77b4", "not_found_pct" = "#d3d3d3"),
    labels = c("Species Found", "Species Not Found")
  ) +
  theme_void() +
  theme(
    strip.text = element_text(size = 6, margin = margin(b = 4)),  # prevents descender clipping
    legend.position = "bottom",
    panel.spacing = unit(0.5, "lines")
  )




#same charts, but individually instead of all 100 panelled into one

output_dir <- "genus_pie_charts"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


all_species_clean2 <- all_species_clean %>%
  dplyr::mutate(found_in_sound = SCIENTIFIC_NAME %in% sound_species_list)

species_per_genus_in_sound <- all_species_clean2 %>%
  dplyr::group_by(GENUS) %>%
  dplyr::summarise(
    total_species_in_genus = dplyr::n_distinct(SCIENTIFIC_NAME),
    species_found_in_sound = sum(found_in_sound, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(GENUS_LABEL = paste0(GENUS, " (", total_species_in_genus, ")"))


pie_data_all <- species_per_genus_in_sound %>%
  dplyr::mutate(
    found_pct = (species_found_in_sound / total_species_in_genus) * 100,
    not_found_pct = 100 - found_pct
  ) %>%
  dplyr::select(GENUS, GENUS_LABEL, found_pct, not_found_pct) %>%
  tidyr::pivot_longer(
    cols = c(found_pct, not_found_pct),
    names_to = "category",
    values_to = "percent"
  )


unique_genera <- unique(pie_data_all$GENUS)

message("Creating pie charts for ", length(unique_genera), " genera...")

for (genus in unique_genera) {
  # Subset data for the current genus
  genus_data <- pie_data_all %>%
    dplyr::filter(GENUS == genus)
  
  # Get the label for the title and filename
  genus_label <- unique(genus_data$GENUS_LABEL)
  safe_filename <- stringr::str_replace_all(genus_label, "[^[:alnum:] _\\(\\)\\-]", "_")
  
  # Create the plot
  p <- ggplot2::ggplot(genus_data, ggplot2::aes(x = "", y = percent, fill = category)) +
    ggplot2::geom_col(width = 1) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::scale_fill_manual(
      values = c("found_pct" = "#1f77b4", "not_found_pct" = "#d3d3d3"),
    ) +
    ggplot2::labs(title = genus_label) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 24, family = "Calibri", hjust = 0.5, vjust = -5)
    )
  
  # Save plot to file
  output_path <- file.path(output_dir, paste0(safe_filename, ".png"))
  
  ggplot2::ggsave(
    filename = output_path,
    plot = p,
    width = 4,
    height = 4,
    dpi = 300
  )
  
  message("Saved: ", output_path)
}


#looking at number of observers for sound records
observers<-sound %>%
  count(user_login)
