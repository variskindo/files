#Example-1 plotly
library(plotly)

df <- read.csv("2014_world_gdp_with_codes.csv")
head(df)
#is.data.frame(df)

fig <- plot_ly(df, type='choropleth', locations=df$CODE, z=df$GDP..BILLIONS., text=df$COUNTRY, colorscale="Blues")

fig


#ggplot
#example-2
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)


#Create a simple map World map
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "red")

head(world_map)

world_map <- map_data("state")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")
world_map <- map_data("usa")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")
world_map <- map_data("nz")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")
world_map <- map_data("county")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")
world_map <- map_data("italy")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

world_map <- map_data("state")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")
head(world_map)

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

#Map for specific regions Some EU Contries

eu_countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)
# Retrieve the map data
eup_maps <- map_data("world", region = eu_countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region_lab_data <- eup_maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

#map-plot

ggplot(eup_maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region_lab_data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")




#example 3
library(plotly)
library(dplyr)
county_df <- map_data("county")
state_df <- map_data("state")
df<-read.csv("votes.csv")
head(df)


#region
df <- subset(df, select = c(Clinton, Trump, county_name))
head(df)

df$county_name %<>%
  gsub(" county", "", .) %>%
  gsub(" parish", "", .) %>%
  gsub(" ", "", .) %>%
  gsub("[.]", "", .)
head(df)

county_df$subregion <- gsub(" ", "", county_df$subregion)
head(county_df)

#vote measurement and then determine who won which county by 
#checking which candidate had the greater amount of votes.
df$Clinton <- df$Clinton*100
df$Trump <- df$Trump*100
for (i in 1:length(df[,1])) {
  if (df$Clinton[i] > df$Trump[i]) {
    df$win[i] = 'Clinton'
  } else {
    df$win[i] = 'Trump'
  }
}
head(df)

#remove duplicates
names(df) <- c("Clinton", "Trump", "subregion", "win")
choropleth <- inner_join(county_df, df, by = "subregion")
head(choropleth)
choropleth <- choropleth[!duplicated(choropleth$order), ]
head(choropleth)

#ggplot
p <- ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = win), 
               colour = alpha("white", 1/2), size = 0.1)  +
  geom_polygon(data = state_df, colour = "white", fill = NA) + 
  scale_fill_manual(values = c('blue','red')) +
  theme_void()

#ggplotly
p <- ggplotly(p, tooltip = 'text') %>% 
  layout(
    hovermode = 'x',
    margin = list(
      t = 20,
      b = 20,
      l = 20,
      r = 20),
    legend = list(
      orientation = 'h',
      x = 0.5,
      y = 1.01,
      xanchor = 'center'))
# use style to modify layer
p <- style(p, hoverinfo = 'none', traces = c(3))
# use plotly_build to modify layer
p <- plotly_build(p)
str(p$x$layout$annotations) # check annotations
p$x$layout$annotations = NULL # remove annotation