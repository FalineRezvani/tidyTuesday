---
title: "waterInsecurity"
output: html_document
date: "2025-01-29"
developer: Faline Rezvani
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# The code to load tidycensus and set API key available
# <here> https://waterdata.usgs.gov/blog/acs-maps/

# API keys can be requested <here> https://api.census.gov/data/key_signup.html

```{r}
# Load tidycensus for processing and visualizing data
# Loading libraries for spatial data processing and column name cleaning
library(tidycensus)
library(janitor)
library(sf)
```


```{r}
# Set your Census API key
#census_api_key("Insert API Key Here", install = TRUE)
```

```{r}
# Reload environment first time to use the key without restarting R
#readRenviron("~/.Renviron")
```

```{r}
# View your API key
#Sys.getenv("CENSUS_API_KEY")
```

```{r}
# Load American Community Survey 1-year estimates data for 2023
acs_vars <- load_variables(2023, "acs1", cache = TRUE)

# View the first couple rows of data
#head(acs_vars)
```

# More information on the Appalachian States can be found
# <here> https://www.arc.gov/appalachian-states/

```{r}
# List of states to filter Census data by
appalachia <- c('Alabama', 'Georgia', 'Kentucky', 'Maryland', 'Mississippi',
               'New York', 'North Carolina', 'Ohio', 'Pennsylvania',
               'South Carolina', 'Tennessee', 'Virginia', 'West Virginia')
```

```{r}
# Define the variable names: total population and plumbing lacking households
# B01003_001: total population, B25049_004: households lacking plumbing
vars <- c("B01003_001", "B25049_004")
```

## Function to load census data courtesey of waterdata.usgs.gov

```{r}
get_census_data <- function(geography, var_names, states, year, proj, survey_var) {

  # Obtain data and feature geometry for ACS data and processing it
  df <- get_acs(
    geography = geography,
    variable = var_names,
    state = states,
    year = year,
    geometry = TRUE,
    survey = survey_var) |>
    # standardize and clean column names
    clean_names() |>
    # set projection of data
    st_transform(proj)
  
    return(df) 
}
```

```{r}
# Pull data for 2023 and 2022 
appalachia_2023 <- get_census_data(
  geography = 'county', 
  var_names = vars, 
  states = appalachia, 
  year = 2023, 
  proj = "EPSG:5070", 
  survey_var = "acs1")
```

```{r}
#appalachia_2022 <- get_census_data(
#  geography = 'county', 
#  var_names = vars, 
#  states = appalachia, 
#  year = 2022, 
#  proj = "EPSG:5070", 
#  survey_var = "acs1"
#)
```

```{r}
#View(appalachia_2023)
```

```{r}
library(tidyverse)
```

```{r}
# Processing 'appalachia_2023' df
# Pivot data to wider format for 2023
appalachia_2023_wide <- appalachia_2023 |> 
  # new column variable_long based on the value of the variable column
  mutate(
    variable_long = case_when(
      variable == "B01003_001" ~ "total_pop",
      variable == "B25049_004" ~ "plumbing",
      .default = NA_character_  # In case there are any other variables
      )
    ) |> 
  select(geoid, name, variable_long, estimate, geometry) |> 
  pivot_wider(
    names_from = variable_long,
              values_from = estimate
    ) |> 
  # Add a column for percent of population lacking plumbing in 2023
  mutate(
    percent_lacking_plumbing = (plumbing / total_pop) * 100
    )
```


```{r}
View(appalachia_2023_wide)
```

```{r}
library(tigris) # For downloading and using cartographic boundary shapefiles 
library(rmapshaper) # For simplifing spatial objects
library(showtext) # For adding custom fonts
library(scico) # For adding color palettes
```

# Building the visualization

```{r}
# Download a generalized (1:500k) states file
# Set projection with st_transform
# Standardize column names with clean_names()
# Filter data for set of 'appalachia' states we defined earlier
# simplify spatial data
appalachia_states_sf <- 
  states(cb = TRUE) |> 
  st_transform("EPSG:5070") |> 
  clean_names() |>
  filter(name %in% appalachia) |>
  ms_simplify(keep = 0.2) 
```

```{r}
appalachia_counties_sf <- 
  # download a generalized (1:500k) counties file
  counties(cb = TRUE) |>
  # set projection
  st_transform("EPSG:5070") |> 
  # standardize column names
  clean_names() |>  
  # filter data for counties spanning Appalachian Mountain Range
  filter(state_name %in% appalachia) |> 
  # simplify spatial data
  ms_simplify(keep = 0.2) 
```

```{r}
# Load custom font for legend title                
font_legend <-"Source Sans Pro"
font_add_google(font_legend)
showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 500)
showtext_auto(enable = TRUE)
```


```{r}
# Function to plot layers of data
map_plumbing <- function(data, year){
  map_chloropleth <- 
    data |>  #  load the census data 
    ggplot(
      aes(fill = percent_lacking_plumbing) # Set fill variable
    ) +  
    geom_sf(
      color = "white", # Plot with white boundaries
      linewidth = 0.05 # Set the width of the boundary lines
    ) +  
    geom_sf(
      data = appalachia_states_sf, # Add state boundaries for overlay
      fill = NA, # No fill for this layer
      color = "white",  # Set the boundary color to white
      linewidth = 0.1, # Set the width of the boundary lines
      linetype = "solid" # Use solid lines for the boundaries
    ) +  
    geom_sf(
      data = appalachia_counties_sf, # Add county boundaries for overlay
      fill = NA, # No fill for this layer
      color = "grey80", # Set the boundary line color to gray
      linewidth = 0.09, # Set the width of the boundary lines
      linetype = "solid" # Use solid lines for the boundaries
    ) +  
    theme_void() +  # Remove axis text and labels
    theme(
      text = element_text(family = font_legend, size = 16), # Custom font 
      legend.margin = margin(t = 5, b = 2), # Set margins for the legend
      legend.position = 'bottom', # Position the legend at the bottom
      legend.title.align = 0.5 # Center-align the legend title
    ) +  
    guides(
      fill = guide_colorbar( # Customize the color bar legend
        title.position = "top",  # Position the title at the top
        # Customize title text
        title.theme = element_text(
          face = 'bold',
          family = font_legend,
          size = 8
        ), 
        direction = "horizontal", # Orient the color bar horizontally
        position = "bottom",  # Position the color bar at the bottom 
        barwidth = 20, # Set the width of the color bar
        barheight = 1) # Set the height of the color bar 
    ) + 
    scale_fill_scico(
      name = sprintf("Households lacking plumbing (%s)", year), # Legend title
      limits = c(0, 1), # Set the range of values for the fill scale
      breaks = c(0, 1, 2, 3, 4, 5), # Set break points for the legend 
      palette = "lajolla",  # Use the "lajolla" palette from `scico`
      direction = -1, # Swap "lajolla" palette direction
      end = 0.95, # Edit interval within palette to sample colors from
      na.value="white", # Assign NA values as white
      labels = function(x) paste0(x, "%"))  # Paste % labels on legend
  
  return(map_chloropleth) 
}
```


```{r}
# Display map
map_plumbing(data = appalachia_2023_wide, year = 2023)
```

```{r}
ggsave("mapPlumbing.png")
```


