library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(tigris)

options(tigris_use_cache = TRUE)

setwd("~/Desktop/BSTA374_Project")

#load datasets
cdc <- read_csv("VSRR_Provisional_County-Level_Drug_Overdose_Death_Counts.csv")
pop <- read_csv("co-est2024-alldata.csv")

#check variables
names(cdc) #this dataset has drug overdose deaths by county and period
names(pop) #this dataset contains Census population estimates by county

#preview datasets
head(cdc)
head(pop)

#get Pennsylvania county boundaries using tigris 
pa <- counties(state = "PA", cb = TRUE, year = 2024)

#check counties
nrow(pa) #should be total of 67 counties in PA
head(pa)

#project to EPSG:5070 for mapping the contiguous state 
pa <- st_transform(pa, 5070)

#plot counties without mortality first
ggplot(pa) +
  geom_sf(fill = "grey90", color = "white") +
  labs(title = "Pennsylvania Counties") +
  theme_void()

#clean CDC overdose mortality data by first filtering PA overdose deaths, 2020-2024
cdc_pa <- cdc |>
  filter(
    ST_ABBREV == "PA",
    Month == 12,
    Year %in% 2020:2024
  ) |>
  mutate(
    GEOID = sprintf("%05d", as.integer(FIPS)),
    year = as.integer(Year),
    deaths = as.numeric(`Provisional Drug Overdose Deaths`)
  ) |>
  select(GEOID, year, deaths)

nrow(cdc_pa) #counts total number of observations 
head(cdc_pa)

#check observations by year
cdc_pa |>
  count(year)


# Filter Pennsylvania counties
pop_pa <- pop |>
  filter(STATE == "42", COUNTY != "000") |>
  mutate(
    GEOID = paste0(STATE, COUNTY)
  )

# Check counties
nrow(pop_pa) 
head(pop_pa)


#separate population estimates by year
pop_long <- bind_rows(
  pop_pa |> mutate(year = 2020, Population = POPESTIMATE2020),
  pop_pa |> mutate(year = 2021, Population = POPESTIMATE2021),
  pop_pa |> mutate(year = 2022, Population = POPESTIMATE2022),
  pop_pa |> mutate(year = 2023, Population = POPESTIMATE2023),
  pop_pa |> mutate(year = 2024, Population = POPESTIMATE2024)
) |>
  select(GEOID, year, Population)

#check population data
nrow(pop_long)
head(pop_long)

#merge datasets where I'm joining population and overdose deaths to counties
pa_data <- pa |>
  left_join(pop_long, by = "GEOID") |>
  left_join(cdc_pa, by = c("GEOID", "year"))

# Check the data
nrow(pa_data)

sum(is.na(pa_data$Population))
sum(is.na(pa_data$deaths))

#Calculate mortality rate per 100,000
pa_data <- pa_data |>
  mutate(
    crude_rate = 100000 * deaths / Population
  )

# View results
pa_data |>
  st_drop_geometry() |>
  select(NAME, year, deaths, Population, crude_rate) |>
  head()

#create overdose mortality map
ggplot(pa_data) +
  geom_sf(
    aes(fill = crude_rate),
    color = "white"
  ) +
  facet_wrap(~year) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey85",
    name = "Deaths per 100,000"
  ) +
  labs(
    title = "Pennsylvania Drug Overdose Mortality",
    subtitle = "County-level rates, 2020-2024"
  ) +
  theme_void()



library(leaflet)

# mp 1: Pennsylvania county boundaries
#convert Pennsylvania boundaries for Leaflet
pa_leaflet <- st_transform(pa, 4326)

# Interactive Pennsylvania county map
leaflet(pa_leaflet) |>
  addTiles() |>
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 0.3
  ) |>
  setView(lng = -77.8, lat = 41.0, zoom = 7)

#map 2: Pennsylvania with CartoDB basemap
leaflet(pa_leaflet) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 0.3
  ) |>
  setView(lng = -77.8, lat = 41.0, zoom = 7)

#select 2024 mortality data
pa_2024 <- pa_data |>
  filter(year == 2024) |>
  st_transform(4326)

#create color palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = pa_2024$crude_rate,
  na.color = "grey85"
)

#map 3: 2024 overdose mortality map
leaflet(pa_2024) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillColor = ~pal(crude_rate),
    color = "white",
    weight = 1,
    fillOpacity = 0.8,
    label = ~paste0(
      NAME, ": ",
      ifelse(
        is.na(crude_rate),
        "Missing or suppressed",
        paste0(round(crude_rate, 1), " deaths per 100,000")
      )
    )
  ) |>
  addLegend(
    pal = pal,
    values = ~crude_rate,
    title = "Deaths per 100,000",
    opacity = 0.8
  )
