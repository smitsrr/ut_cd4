library(tigris)
library(leaflet)
library(rlist)
library(dplyr)
library(rgdal)    # for readOGR(...)
library(tidyverse)

###################################################################3
# TRY TO READ IN SLCO DATA  

# Load the packages required to read XML files.
library("XML")
library(xml2)
library("methods")

# Give the input file name to the function.


xml<- read_xml("detail.xml")

races_tidy3 <- xml %>% 
  xml_find_all('//Contest') %>% 
  map_df(~flatten(c(xml_attrs(.x), 
                    map(xml_children(.x), 
                        ~set_names(as.list(xml_text(.x)), xml_name(.x)))))) %>%
  type_convert()

races_tidy3
#This get's me part of the way there, but i am more interested in all of the children. 
# the final data frame should look something like:
# Race (text)  race (key), choice, votetype, precinct, votes






## Exploration!!
result <- xmlParse(file = "detail.xml")
rootnode <- xmlRoot(result) # Exract the root node form the xml file.
names(rootnode) ## found objects!
names(rootnode[3])

rootnode[[5]][[1]] ## = voter turnout

  #This extracts the nodeset
voter_turnout<- getNodeSet(rootnode, 
                           "//VoterTurnout/Precincts")
node<- voter_turnout[[1]][[1]]
voter<-xmlToDataFrame(node[[1]][[1]], collectNames = T, stringsAsFactors = F)
#now I need to get this in a loop so I can extract all of the names etc. 

# Get's the the results for CD4
## Will need to append the Straight party information as well. 
cd4<- getNodeSet(rootnode, "//Contest[@key='45']")
straight<- getNodeSet(rootnode, "//Contest[@text='STRAIGHT PARTY']")

cd4[[1]][[1]] ## <VoteType name="Number of Precincts for Race" votes="439">
cd4[[1]][[2]] ## Number of precincts reporting (don't need this one, since everthing is done)
cd4[[1]][[3]] ## <VoteType name="Times Blank Voted" votes="5842">
cd4[[1]][[4]] ## <VoteType name="Times Over Voted" votes="16">
cd4[[1]][[5]] ## <VoteType name="Number of Under Votes" votes="0">
cd4[[1]][[6]] ## all of the choice data start here
xmlGetAttr(cd4[[1]][[6]], name = "party")
xmlGetAttr(cd4[[1]][[7]], name = "party")
xmlGetAttr(cd4[[1]][[8]], name = "text")
xmlGetAttr(cd4[[1]][[9]], name = "text")
cd4[[1]][[7]] ## <VoteType name="Early Voting" votes="1213">
cd4[[1]][[8]]
cd4[[1]][[9]]
cd4[[1]][[10]]
cd4[[1]][[6]]


getNodeSet(cd4[[1]])
## for all the other races, I could just look at the top 2 candidates, assuming they will be rep/dem, 
##   then do 'remaining', and 'withheld'. 






####################################################################
#MAPPING

head(fips_codes)
utah_counties<- filter(fips_codes, fips_codes$state_name == 'Utah')
utah_counties<- filter(utah_counties, utah_counties$county %in% c("Utah County", 
                                                               "Salt Lake County", 
                                                               "Juab County", 
                                                               "Sanpete County"))

ut<- voting_districts("Utah") #Becomes a Large SpatialPolygonsDataFrame
ut_data<- ut[ut@data$COUNTYFP10 %in% utah_counties$county_code,]
ut_info<- data.frame(ut_data@data)
# there are just no variables that join 'ut' to the precinct identifiers in the county registration data. 
# Maybe there is a new file??

#might be able to get US Census data by state legislative districts, which might line up well with voting precincts. 
#keep only the polygons with ID in 
plot(ut_data)

## TRY NEW PRECINCT SHAPE FILEA; https://gis.utah.gov/data/political/voter-precincts/
ut_precincts<- readOGR(dsn="VistaBallotAreas")
precinct_data<- data.frame(ut_precincts@data)

map <- ggplot() + geom_polygon(data = ut_precincts, aes(x = long, y = lat, group = group)
                               , colour = "black", fill = NA) +
  theme_void()



##would like to inner-join for CD4. 
#first I can nail it down to the 4 counties that are relevant, then hopefully I can inner join
# to the county election officials data. 


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



