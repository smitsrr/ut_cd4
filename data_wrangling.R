library(tigris)
library(leaflet)
library(rlist)
library(dplyr)
library(rgdal)    # for readOGR(...)
library(tidyverse)
library(data.table)
library(RColorBrewer)

###################################################################3
# READ IN SLCO DATA from OpenElections

# #url<- "https://github.com/openelections/openelections-data-ut/raw/master/2018/20181106__ut__general__salt_lake__precinct.csv"
# #download.file(url, 'slco_districts.csv')
slco<- read.csv('slco_districts.csv', stringsAsFactors = F)

# Sanity check to make sure vote counts match elections website
slco %>%
  filter(office %in% c('U.S. SENATE', 'PROPOSITION NUMBER 2', 
                          'U.S. REPRESENTATIVE DISTRICT #4')) %>%
  group_by(office, candidate) %>%
  summarise(vote = sum(votes)) %>%
  arrange(office, desc(vote))
# Good to know that straight party votes are duplicated in the races they counted in. 

# Calculate dem percent for each race and precinct
# filter out straight party since I know those are duplicated (futher analyses later!)
slco_prop <- slco %>%
  filter(office%in% c('U.S. SENATE', 'PROPOSITION NUMBER 2', 
                      'U.S. REPRESENTATIVE DISTRICT #4', 
                      'PROPOSITION NUMBER 3')) %>%
  group_by(precinct, office) %>%
  mutate(votes_cast = sum(votes)) %>%
  ungroup() %>%
  mutate(candidate_pct = votes/votes_cast, 
         precinct_fct = as.factor(precinct)) %>%
  filter(party == 'DEM' |
           candidate == "FOR")  %>%
  mutate(candidate = paste0(office, '-', candidate))

slco_prop2<- slco_prop %>%
  select(-office, -party, -votes, -vbm, -early_voting, -vote_center,-votes_cast) %>%
  spread(candidate, candidate_pct)

qplot(slco_prop2$`PROPOSITION NUMBER 3-FOR`, slco_prop2$`U.S. REPRESENTATIVE DISTRICT #4-BEN MCADAMS`) +
  geom_abline(slope = 1, inercept = 0)

data.table(key = 'precinct') 
# One row per precinct, just plotting % Ben to start!
#set the key for the joining of things later. 

####################################################################
#MAPPING

## TRY NEW PRECINCT SHAPE FILEA; https://gis.utah.gov/data/political/voter-precincts/
ut.map<- readOGR(dsn="VistaBallotAreas")
   # just to inspect what data we have going on here. 
ut.map.data<- data.frame(ut.map@data)
   # Filter the spatial polygon just to the data we have so far
slco.map<- ut.map[ut.map$PrecinctID %in% slco_prop$precinct_fct,]

map <- ggplot() + 
  geom_polygon(data = slco.map, aes(x = long, y = lat, group = group)
                               , colour = "black", fill = NA) +
  theme_void()
map

#### NEED TO JOIN IN THE VOTING DATA WITH THIS MAP. key = precinct/precinctID
slco.map.tbl<- slco.map@data 
slco.map.tbl<- cbind(id=rownames(slco.map.tbl),slco.map.tbl) %>%
  mutate(PrecinctID = as.character(PrecinctID)) %>%
  data.table(key = "PrecinctID")

vote.map<- slco.map.tbl[slco_prop, nomatch=0] # Nomatch clause makes this an inner join

# make the map layer
map.df<-data.table(fortify(slco.map))
setkey(map.df,id)
# add in interesting data
map.df<- map.df[vote.map]  # should be joined on ID

ggplot(map.df, aes(x=long, y=lat, group = group)) +
  geom_polygon()+
  coord_quickmap()+
  coord_map("polyconic" ) +
  theme_void()

map<- ggplot() + 
  geom_polygon(data = map.df, aes(x = long, y = lat, group = group)
               , colour = "black") +
  theme_void()
map



map.df<- map.df[county.data]  # should be joined on ID

# Join the voting data with geographies (can only do one race at a time)
vote.data <- slco_prop %>%
  filter(candidate == "BEN MCADAMS") %>%
  inner_join(slco.map, by=c("precinct" = "PrecinctID"))


%>%
  fortify()%>%
  data.table()

map<- ggplot() +
  geom_polygon(data=vote.data, aes(x=long, y=lat, group = group), 
               color = "gray", fill=candidate_pct)+
  theme_void()

ggplot(vote.data, aes(x=long, y=lat, group = group)) +
  geom_polygon( aes( fill = candidate_pct)) 




# use the TIGER dataset from the census to draw the state outlines
# http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_5m.zip
US.states <- readOGR(dsn=".",layer="cb_2016_us_state_5m")
exclude_states<- c("02", "15", "78", "60", "66", "69", "72")
US.states<- US.states[!US.states$GEOID %in% exclude_states,]
us_states<- merge(fortify(US.states), as.data.frame(US.states), by.x="id", by.y=0)



cd114 <- congressional_districts(cb = TRUE, resolution = '20m')
cd114_ut<- filter(cd114@data, cd114@data$STATEFP == "49")


leaflet(cd114) %>%
  addTiles() %>%
  addPolygons()

leaflet(ut) %>%
  addTiles() %>%
  addPolygons()

## Goal: Plot at least 2 layers on a map:
# inside of CD4:
# -County boundaries
# -Precinct boundaries

# Essentially then I want to be able to toggle what else to look at and correlate:
# - % minority (namely hispanis)
# - % home ownership
# - #children/dependents
# - Median income

# Might also use some change over time models to predict what things are going to 
# look like in a few years. 
# maybe I could get precinct-level voting data for 2016 and see change patterns? 
# Basically correlate CHANGES in voting patterns with CHANGES in demographics, instead
# of just correlating voting patterns in 2018 with current demographics. 

# look to see how voter For  prop 2, but against mcAdams?



# Map through ACS1 estimates to see how they change through the years
mi_cities <- map_df(2012:2016, function(x) {
  get_acs(geography = "place", 
          variables = c(totalpop = "B01003_001"), 
          state = "MI", 
          survey = "acs1", 
          year = x) %>%
    mutate(year = x)
})

mi_cities %>% arrange(NAME, year)



