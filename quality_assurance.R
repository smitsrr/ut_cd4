## Quality Assurance
# check totals against https://electionresults.utah.gov/elections/countyCount/300150

library(dplyr)

# READ IN SLCO DATA from OpenElections
setwd('..')
setwd('./openelections-data-ut') # git repo: https://github.com/openelections/openelections-data-ut.git
slco_2018<-  read.csv('./2018/20181106__ut__general__salt_lake__precinct.csv', stringsAsFactors = F)
slco_2018<- slco_2018 %>%
  select(-vbm, -early_voting, -vote_center) 

utco_2018<- read.csv('./2018/20181106__ut__general__utah__precinct.csv', stringsAsFactors = F)
utco_2018 <- filter(utco_2018, precinct != 'UTAH UT') #filter out this icky 'total' Row!

cd4 %>%
  filter(office == "U.S. REPRESENTATIVE DISTRICT #4" |
           (office == "U.S. House" &
              district == 4)) %>%
  group_by(year, candidate) %>%
  summarize(votes = sum(votes)) %>%
  arrange(year, desc(votes))

###############################################################
# Sanity check to make sure vote counts match elections website
slco_2018 %>%
  filter(office %in% c('U.S. SENATE', 'PROPOSITION NUMBER 2', 
                       'U.S. REPRESENTATIVE DISTRICT #4')| 
           candidate == 'DEMOCRATIC') %>%
  group_by(office, candidate) %>%
  summarise(vote = sum(votes)) %>%
  arrange(office, desc(vote))
# Good to know that straight party votes are duplicated in the races they counted in. 

# didn't get propositions for ut county for 2018
utco_2018 %>%
  filter(office =='US House') %>%
  group_by(office, district, candidate) %>%
  summarise(vote = sum(votes)) %>%
  arrange(office, district, desc(vote))
#awesome - good to go!

## NOW CHECK 2016 RESULTS

slco_2016<- read.csv('./2016/20161108__ut__general__salt_lake__precinct.csv', stringsAsFactors = F)
## https://elections.utah.gov/Media/Default/2016%20Election/2016%20General%20Election%20-%20Statewide%20Canvass%203.pdf
utco_2016<- read.csv('./2016/20161108__ut__general__utah__precinct.csv', stringsAsFactors = F)

slco_2016%>%
  filter(office == 'U.S. House' ) %>%
  group_by(office, district, candidate) %>%
  summarise(vote = sum(votes)) %>%
  arrange(office, district, desc(vote))
## president is good. House is good. 

utco_2016 %>%
  filter(office =='U.S. House') %>%
  group_by(office, district, candidate) %>%
  summarise(vote = sum(votes)) %>%
  arrange(office, district, desc(vote))
# district 3 matches, but not district 4

utco_2016 %>%
  filter(office =='U.S. Senate') %>%
  group_by(office, candidate) %>%
  summarise(vote = sum(votes)) %>%
  arrange(office, district, desc(vote))
# Misty's number matches, but not the other 3. 

utco_2016 %>%
  filter(office =='President') %>%
  group_by(office, candidate) %>%
  summarise(vote = sum(votes)) %>%
  arrange(office, desc(vote))
# These are perfect
