#load packages
library(tidyverse)
library(dplyr)
library(sf)

#load in squirrel data from tidy tuesday
nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

#load central park GIS file to overlay data on
central_park_sf <- read_sf("~/Desktop/CentralAndProspectParks/") #file location

#print tibble
nyc_squirrels

#count primary_fur color & highlights
nyc_squirrels %>%
   count(highlight_fur_color, highlight_fur_color, sort = TRUE)

#aggregate fur color data by hectare
fur_color_hectare <- nyc_squirrels %>%
      filter(!is.na(primary_fur_color)) %>%
      group_by(hectare) %>%
      summarise(long = mean(long),
                lat = mean(lat), 
                pct_gray = mean(primary_fur_color == "Gray", na.rm = TRUE),
                n = n())

#line graph % of grey squirrels as a function of longitude
fur_color_hectare %>%
      filter(n >= 10) %>%
      ggplot(aes(lat, pct_gray)) + 
      geom_point() + 
      geom_smooth()

#test gray squirrel proportion by long using linear regression
fur_color_hectare %>%
      mutate(n_gray = round(pct_gray * n)) %>%
      glm(cbind(n_gray, n - n_gray) ~ lat, data = ., family = "binomial") %>%
      summary()

#graph proportion of gray squirrels by hectare

fur_color_hectare %>%
   filter(n >= 10) %>%
   ggplot() + 
   geom_sf(data = central_park_sf) + 
   geom_point(aes(long, lat, size = n, color = pct_gray)) + 
   theme_void() + 
   scale_color_gradient2(low = "blue", high = "red", mid = "pink",
                         midpoint = .75,labels = scales::percent)+
   labs(color = "% gray squirrels", 
        size = "# of squirrels",
        title = "Gray Squirrel Distribution") +
   coord_sf(datum = NA)

#Results: Squirrels more likely to be gray in Northern latitude

#count squirrel behavior
nyc_squirrels %>%
      count(approaches, indifferent, runs_from, sort = TRUE)

#is a squirrel run away as a function of latitutde?
glm(runs_from ~ lat, data = nyc_squirrels, family = "binomial") %>%
      summary()

#aggregate by runs_from behavior
runs_from_behavior <- nyc_squirrels %>%
      add_count(hectare) %>%
      group_by(hectare, n) %>%
      summarize_at(vars(long, lat, runs_from, indifferent), mean) %>%
      ungroup() %>%
      filter(n >= 10)

#graph runs_from behavior overlayed NYC Grid
ggplot(runs_from_behavior) + 
      geom_sf(data = central_park_sf) + 
      geom_point(aes(long, lat, size = n, color = runs_from)) + 
      theme_void() + 
      scale_color_gradient2(low = "blue", high = "red", mid = "pink",
                            midpoint = .3,labels = scales::percent)+
      labs(color = "% of squirrels run", 
           size = "# of squirrels",
           title = "Squirrels in the northwest are more likeley to run away") +
           coord_sf(datum = NA)