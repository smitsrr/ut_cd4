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

setwd('..')
setwd('./openelections-data-ut') # git repo: https://github.com/openelections/openelections-data-ut.git
slco_2018<-  read.csv('./2018/20181106__ut__general__salt_lake__precinct.csv', stringsAsFactors = F)
slco_2018<- slco_2018 %>%
  select(-vbm, -early_voting, -vote_center) 

utco_2018<- read.csv('./2018/20181106__ut__general__utah__precinct.csv', stringsAsFactors = F)
utco_2018 <- filter(utco_2018, precinct != 'UTAH UT') #filter out this icky 'total' Row!

cd4_18<- rbind(slco_2018, utco_2018)

## keep only precincts that were cd4 territory
cd4_precincts<- cd4_18 %>%
  filter(office == "U.S. REPRESENTATIVE DISTRICT #4" |
           (office == "US House" &
              district == 4))
cd4_precincts<- unique(cd4_precincts$precinct)

cd4_18<- filter(cd4_18, precinct %in% cd4_precincts)%>%
  mutate(year = 2018)

## IMPORT 2016 ELECTION DATA
slco_2016<- read.csv('./2016/20161108__ut__general__salt_lake__precinct.csv', stringsAsFactors = F)

utco_2016<- read.csv('./2016/20161108__ut__general__utah__precinct.csv', stringsAsFactors = F)

cd4_16<- rbind(slco_2016, utco_2016)
## keep only precincts that were cd4 territory
cd4_precincts<- cd4_16 %>%
  filter(office == "U.S. REPRESENTATIVE DISTRICT #4" |
           (office == "U.S. House" &
              district == 4))
cd4_precincts<- unique(cd4_precincts$precinct)

cd4_16<- filter(cd4_16, precinct %in% cd4_precincts) %>%
  mutate(year = 2016)

cd4<- rbind(cd4_16, cd4_18) %>%
  group_by(year, county, precinct, office, district) %>%
  mutate(votes_cast = sum(votes))

setwd('..')
write.csv(cd4, "./ut_cd4_house_race/district4.csv", row.names = F)


cd4 %>%
  filter(office == "U.S. REPRESENTATIVE DISTRICT #4" |
           (office == "U.S. House" &
              district == 4)) %>%
  group_by(year, candidate) %>%
  summarize(votes = sum(votes)) %>%
  arrange(year, desc(votes))


##############################################################

# Calculate dem percent for each race and precinct
# filter out straight party since I know those are duplicated (futher analyses later!)
slco_prop <- slco_18 %>%
  filter(office%in% c('U.S. SENATE', 'PROPOSITION NUMBER 2', 
                      'U.S. REPRESENTATIVE DISTRICT #4', 
                      'PROPOSITION NUMBER 3') ) %>%
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

# in order to make the correlation matrix I'm interested in, I need to have only precinct down the rows, and 
# each column is a value (% dem), but then I'll also want a column for num_votes for that race

slco_corr<- slco %>%
  filter(office != 'STRAIGHT PARTY') %>%
  group_by(precinct, office) %>%
  mutate(votes_cast = sum(votes)) %>%
  ungroup() %>%
  select(-district)

# how many precincts did Ben compete in? 
slco %>%
  filter(office == 'U.S. REPRESENTATIVE DISTRICT #4')%>%
  group_by(precinct) %>%
  tally()

unique(slco)

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


##################################
## 2016 election!
##################################







ut.map<- readOGR(dsn="VistaBallotAreas")
# just to inspect what data we have going on here. 
ut.map.data<- data.frame(ut.map@data)
# Filter the spatial polygon just to the data we have so far
cd4.map<- ut.map[ut.map$PrecinctID %in% cd4_precincts,]



map <- ggplot() + 
  geom_polygon(data = cd4.map, aes(x = long, y = lat, group = group)
               , colour = "black", fill = NA) +
  theme_void()
map

