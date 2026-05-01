# Exemplary Function Code
Below is a custom R function that I used in a project that required creating 20 graphs from OECD's TiVA (Trade in Value Added) data set. Rather than spend the time formatting each graph individually, I used this R function to make the process more efficient. Below is the code for the custom function as well as an example of the code using the TiVA data set. 

## Code for TiVA Project 
graph_standard <- function(data, time_var, obs_var, source_var) {
ggplot(data, aes(x = {{time_var}}, 
                 y = {{obs_var}}, 
                 color = {{source_var}}, 
                 group = {{source_var}})) +
  geom_line(size=1) +  
  geom_point(size=1.5) +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.1)) 
  ) + 
  labs(
    x = "Year", 
    y = "Share of Value Add", 
    color = "Source Area",  
    caption = 'Source: OECD TiVA Database'
  ) +
  theme_minimal() + 
  theme( 
      plot.title = element_text(size=14, face = 'bold', hjust = 0.5), 
      axis.title=element_text(size=10, face="bold"),
      legend.position = "bottom" 
  ) 
}

## Code for Example of TiVA Graph 
my_colors <- c( 
  `IRL` = '#FFDAB9', 
  `CHE` = '#7D26CD',  
  `GBR` = '#FF6EB4', 
  `DEU` = "#39FF14",  
  `CHN` = "#E63946",  
  `CAN` = "#F4A261",  
  `IND` = "#2A9D8F",  
  `ITA` = '#E066FF',  
  `DNK` = '#3A5FCD',  
  `MEX` = "#FFB6C1",  
  `JPN` = "#6A4C93",  
  `KOR` = "#E9C46A",  
  `VNM` = "#264653",   
  `USA` = "#1874CD", 
  `EU28` = "#CD9B9B",  
  `AUS` = "#6E8B3D",  
  `CHL` = "#9AFF9A",  
  `MYS` = "#CD9B9B", 
  `NZL` = "#FF4500", 
  `PER` = "#FFA500", 
  `SGP` = "#BBFFFF", 
  `FRA` = "#457B9D", 
  `WXD` = "#6D6875")
  
data <- read.csv("OECD.STI.PIE.DSD_TIVA_FDVA@DF_FinalDemand.csv")

regional_groups <- c("OECD", "WXOECD", "APEC", "ASEAN", "S2", "EU27_2020", 
                     "EU28", "EU15", "EU28XEU15", "EA19", "G20", "E", "S2_S8", 
                     "NAFTA", "A5_A7", "F", "W", "D", "W_O")  
                     
fdemand_countries <- 
  data |>
  filter(!VALUE_ADDED_SOURCE_AREA %in% regional_groups)

us_fdemand <- 
  fdemand_countries |>
  filter(FINAL_DEMAND_AREA == "USA")

us_fdemand_per <- 
  us_fdemand |>
  filter(VALUE_ADDED_SOURCE_AREA != "USA") |>
  group_by(TIME_PERIOD) |>
  mutate(
    total_value = sum(OBS_VALUE, na.rm = T),
    percent_OBS = (OBS_VALUE / total_value) 
  ) |>
  slice_max(order_by = percent_OBS, n = 6) |>
  ungroup() 

graph_standard(us_fdemand_per, TIME_PERIOD, percent_OBS, VALUE_ADDED_SOURCE_AREA) + 
  labs(title = "Top Six Share of Value Added in US Final Demand")

## Example Graph 

<img width="962" height="594" alt="image" src="https://github.com/user-attachments/assets/272670da-780b-4d9e-8d9e-e68441d07193" />


