library(tigris)
library(leaflet)
library(rlist)
library(dplyr)
library(rgdal)    # for readOGR(...)
library(ggplot2)

# CD4 voter data!!
slco_cd4 <- read.csv("slco_cd4.csv", stringsAsFactors = F) %>%
  filter(slco_precinct != "Total:")


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

###################################################################3
# TRY TO READ IN SLCO DATA  

# Load the packages required to read XML files.
library("XML")
library("methods")

# Give the input file name to the function.
result <- xmlParse(file = "detail.xml")

# Exract the root node form the xml file.
rootnode <- xmlRoot(result)

# Print the result.
head(rootnode[5]) ## Voter turnout
voter_turnout <- xmlToDataFrame(rootnode[5])
