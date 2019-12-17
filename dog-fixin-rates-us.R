## Get Data
library(readr)
str<-'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/'
dog_moves<-read_csv(paste0(str,'dog_moves.csv'))
dog_travel<-read_csv(paste0(str,'dog_travel.csv'))
dog_descriptions<-read_csv(paste0(str,'dog_descriptions.csv'))

## Calculate Fixin' Rates
fixed_rates<-dog_descriptions %>%
  group_by(contact_state) %>%
  summarise(fixed_perct=mean(fixed),
            total_dogs=n()) %>%
  filter(total_dogs > 10)

## Get hexagon file ready
library(geojsonio)
library(broom)
library(rgeos)
# converts to percent format
source('~/Common Functions/percent.R')
# Download US hexes - there are ways to generate as well - tigiris/geogrid
#https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
json_file<-"~/Common Functions/us_states_hexgrid.geojson"
spdf<-geojson_read(json_file, what="sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name),
         state_id=spdf@data$iso3166_2)

spdf_fortified <- tidy(spdf, region = "state_id")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

library(ggplot2)
spdf_fortified %>% 
  left_join(., fixed_rates, by=c("id"="contact_state")) %>% 
  ggplot() + 
  geom_polygon(aes(fill=fixed_perct, x=long, y=lat, group=group), size=0, alpha=.9) + 
  geom_text(data=centers, aes(x=x, y=y, label=id), color="black") +
  scale_fill_gradient(low = "red", high = "white", 
                      name="", 
                      breaks = 0.10*0:10, labels = percent(0.1*0:10, digits=0),
                      guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                            keywidth=unit(12, units = "mm"), 
                                            label.position = "bottom", 
                                            title.position = 'top', 
                                            nrow=1) ) + 
  theme_void() + 
  coord_map() +
  labs(title="Fixed Dog Rates by State",
       caption="#tidytuesday - Petfinder.com data collected September 19, 2019.") +
  theme(legend.position = c(0.5, 0.9),
        plot.title = element_text(size= 22, hjust=0.5, 
                                  color = "#4e4d47", 
                                  margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )

