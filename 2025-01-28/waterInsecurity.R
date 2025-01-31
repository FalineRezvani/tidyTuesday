---
title: "waterInsecurity"
output: html_document
date: "2025-01-29"
developer: Faline Rezvani
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Code to load tidycensus, set API key, and load U.S. census data available on the
# [U.S. Geological Survey (USGS) website](https://waterdata.usgs.gov/blog/acs-maps/).

# API keys can be requested [here](https://api.census.gov/data/key_signup.html).

```{r}
library(tidycensus) # Processing and visualizing data
library(tidyverse) # Collection of data analysis packages
library(janitor) # Column name cleaning
library(sf) # Spatial data processing
library(tigris) # Downloading and using cartographic boundary shapefiles 
library(rmapshaper) # Simplifing spatial objects
library(showtext) # Custom fonts
library(scico) # Color palettes
```

```{r}
# Set your Census API key
#census_api_key("Your API Key Here", install = TRUE)
```

```{r}
# First time, reload your environment so you can use the key without restarting R.
#readRenviron("~/.Renviron")
```

```{r}
# You can check your API key with:
#Sys.getenv("CENSUS_API_KEY")
```

```{r}
# Load American Community Survey 1-year estimates data for 2023
# Each row reflects a variable in the American Community Survey
acs_vars <- load_variables(2023, "acs1", cache = TRUE)
```

# More information on the Appalachian States can be found
# [here](https://www.arc.gov/appalachian-states/).

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

```{r}
# Function to pull and tidy American Community Survey data
get_census_data <- function(geography, var_names, states, year, proj, survey_var) {

  df <- get_acs(
    geography = geography,
    variable = var_names,
    state = states,
    year = year,
    geometry = TRUE,
    survey = survey_var) |>
    clean_names() |>
    st_transform(proj)
  
    return(df) 
}
```

```{r}
# Pull data for 2023
appalachia_2023 <- get_census_data(
  geography = 'county', 
  var_names = vars, 
  states = appalachia, 
  year = 2023, 
  proj = "EPSG:5070", 
  survey_var = "acs1")
```

```{r}
# Pull data for 2022
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
# Processing 'appalachia_2023' df
# ACS data is in long format, each instance is assigned a variable
# Pivot data to wide format to obtain metadata from each instance
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
# Plumbing plot function
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
# View your maps
map_plumbing(data = appalachia_2023_wide, year = 2023)
```

```{r}
ggsave("mapPlumbing.png")
```

# Vulnerability indicators have been identified by the USGS and further analysis regarding unique populations is needed.
# [Unequal Access to Water report](https://labs.waterdata.usgs.gov/visualizations/vulnerability-indicators/index.html#/en)
# Inspecting unique population of 65+

```{r}
# Defining variable names
# B01003_001: total population
# B01001A_014: Male/White/65-74, B01001A_015: Male/White/75-84, B01001A_016: Male/White/85+
# B01001A_029: Female/White/65-74, B01001A_030: Female/White/75-84, B01001A_031: Female/White/85+
# B01001B_014: Male/Black/65-74, B01001B_015: Male/Black/75-84, B01001B_016: Male/Black/85+
# B01001B_029: Female/Black/65-74, B01001B_030: Female/Black75-84, B01001B_031: Female/Black/85+
# B01001D_014: Male/Asian/65-74, B01001D_015: Male/Asian/75-84, B01001D_016: Male/Asian/85+
# B01001D_029: Female/Asian/65-74, B01001D_030: Female/Asian/75-84, B01001D_031: Female/Asian/85+
# B01001I_014: Male/Latino/65-74, B01001I_015: Male/Latino/75-84, B01001I_016: Male/Latino/85+
# B01001I_029: Female/Latino/65-74, B01001I_030: Female/Lation/75-84, B01001I_031: Female/Latino/85+
vars_senior <- c("B01003_001", "B01001A_014", "B01001A_015", "B01001A_016", "B01001A_029", "B01001A_030",
                 "B01001A_031", "B01001B_014", "B01001B_015", "B01001B_016", "B01001B_029", "B01001B_030",
                 "B01001B_031", "B01001D_014", "B01001D_015", "B01001D_016", "B01001D_029", "B01001D_030",
                 "B01001D_031", "B01001I_014", "B01001I_015", "B01001I_016", "B01001I_029", "B01001I_030",
                 "B01001I_031")
```

```{r}
# Pull data for 2023
senior_2023 <- get_census_data(
  geography = 'county', 
  var_names = vars_senior, 
  states = appalachia, 
  year = 2023, 
  proj = "EPSG:5070", 
  survey_var = "acs1")
```


```{r}
# Processing 'senior_2023' df
# Pivot data to wider format
senior_2023_wide <- senior_2023 |> 
  # new column variable_long based on the value of the variable column
  mutate(
    variable_long = case_when(
      variable == "B01003_001" ~ "total_pop",
      variable == "B01001A_014" ~ "w_m_seniora",
      variable == "B01001A_015" ~ "w_m_seniorb",
      variable == "B01001A_016" ~ "w_m_seniorc",
      variable == "B01001A_029" ~ "w_f_seniora",
      variable == "B01001A_030" ~ "w_f_seniorb",
      variable == "B01001A_031" ~ "w_f_seniorc",
      variable == "B01001B_014" ~ "b_m_seniora",
      variable == "B01001B_015" ~ "b_m_seniorb",
      variable == "B01001B_016" ~ "b_m_seniorc",
      variable == "B01001B_029" ~ "b_f_seniora",
      variable == "B01001B_030" ~ "b_f_seniorb",
      variable == "B01001B_031" ~ "b_f_seniorc",
      variable == "B01001D_014" ~ "a_m_seniora",
      variable == "B01001D_015" ~ "a_m_seniorb",
      variable == "B01001D_016" ~ "a_m_seniorc",
      variable == "B01001D_029" ~ "a_f_seniora",
      variable == "B01001D_030" ~ "a_f_seniorb",
      variable == "B01001D_031" ~ "a_f_seniorc",
      variable == "B01001I_014" ~ "l_m_seniora",
      variable == "B01001I_015" ~ "l_m_seniorb",
      variable == "B01001I_016" ~ "l_m_seniorc",
      variable == "B01001I_029" ~ "l_f_seniora",
      variable == "B01001I_030" ~ "l_f_seniorb",
      variable == "B01001I_031" ~ "l_f_seniorc",
      .default = NA_character_  # In case there are any other variables
      )
    ) |>
  select(geoid, name, variable_long, estimate, geometry) |>
  pivot_wider(names_from = variable_long, values_from = estimate)
```

```{r}
# Converting NA values to '0'
senior_2023_wide <- replace(senior_2023_wide, is.na(senior_2023_wide), 0)
```

```{r}
# Add a column for percent of population 65+ in 2023
senior_2023_wide <- mutate(senior_2023_wide, percent_senior = ((w_m_seniora + w_m_seniorb + w_m_seniorc + w_f_seniora + w_f_seniorb + w_f_seniorc + b_m_seniora + b_m_seniorb + b_m_seniorc + b_f_seniora + b_f_seniorb + b_f_seniorc + a_m_seniora + a_m_seniorb + a_m_seniorc + a_f_seniora + a_f_seniorb + a_f_seniorc + l_m_seniora + l_m_seniorb + l_m_seniorc + l_f_seniora + l_f_seniorb + l_f_seniorc) / total_pop) * 100)
```

```{r}
# Senior plot function
map_senior <- function(data, year){
  map_chloropleth <- 
    data |>  #  load the census data 
    ggplot(
      aes(fill = percent_senior) # Set fill variable
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
      name = sprintf("Population Over 65 (%s)", year), # Legend title
      limits = c(0, 40), # Set the range of values for the fill scale
      breaks = c(0, 10, 20, 30, 40), # Set break points for the legend 
      palette = "lajolla",  # Use the "lajolla" palette from `scico`
      direction = -1, # Swap "lajolla" palette direction
      end = 0.95, # Edit interval within palette to sample colors from
      na.value="white", # Assign NA values as white
      labels = function(x) paste0(x, "%"))  # Paste % labels on legend
  
  return(map_chloropleth) 
}
```

```{r}
# View your maps
map_senior(data = senior_2023_wide, year = 2023)
```

```{r}
ggsave("mapSenior.png")
```

# Need help isolating seniors with the condition 'B25049_004: households lacking plumbing'
