library(tidyverse)
nyccounties <- c('Kings','Queens','New York','Bronx','Richmond')
# Number of housing units, 2017
dhous <- read_csv('ACSDP5Y2017.DP04_2020-03-31T170906/ACSDP5Y2017.DP04_data_with_overlays_2020-03-31T170838.csv',
                  skip=1) %>%
  select(fipslong = `id`,
         statecounty = `Geographic Area Name`,
         nhous17 = `Estimate!!HOUSING OCCUPANCY!!Total housing units`) %>%
  separate(statecounty, into=c('county','state'), sep=', ') %>%
  mutate(fips = str_sub(fipslong, start=10),
         county = str_remove(str_remove(county, ' County'), ' Parish')) %>%
  select(-fipslong)

# Land area 2010
dland <- read_csv("DEC_10_SF1_GCTPH1.US05PR/DEC_10_SF1_GCTPH1.US05PR.csv",
                  skip=1) %>%
  select(statecounty= `Geographic area`,
         county = `Geographic area_1`,
         fips = `Target Geo Id2`,
         landsq = `Area in square miles - Total area`) %>%
  mutate(statecounty = str_remove(statecounty, 'United States - '),
         county = str_remove(str_remove(county, ' County'), ' Parish')) %>%
  separate(statecounty, into=c('state','county2'), sep=' - ')
dland_state <- filter(dland, is.na(county2)) %>% select(state,fips,landsq)
dland <- filter(dland, !is.na(county2))
nycland <- dland %>% filter(state=='New York',
                            county %in% nyccounties) %>% 
  summarise(landsq = sum(landsq)) %>%
  mutate(county='New York City',
         fips = NA, 
         state='New York')
dland <- filter(dland, !(state=='New York' & county %in% nyccounties))
# add NYC back to land area data frame
dland <- bind_rows(dland, nycland)

# Census ACS county populations, 2017
cpop <- read_csv("ACS_17_5YR_DP05_county/ACS_17_5YR_DP05.csv", skip = 1) %>%
  select(fips = Id2,
         countystate = Geography,
         totpop = `Estimate; SEX AND AGE - Total population`)
cpop <- cpop %>% separate(countystate, into=c('county', 'state'), sep=', ') %>%
  mutate(county = str_remove(str_remove(county, ' County'), ' Parish'))

# Census ACS state populations
spop <- read_csv("ACS_17_5YR_DP05_state/ACS_17_5YR_DP05.csv", skip=1) %>%
  select(fips = Id2, state = Geography,
         totpop = `Estimate; SEX AND AGE - Total population`)

# calculate NYC population from individual counties
nycpop <- cpop %>% filter(state=='New York',
                          county %in% nyccounties) %>% 
  summarise(totpop = sum(totpop)) %>%
  mutate(county='New York City',
         fips = NA, state='New York')

# remove NYC-separated counties from population table
cpop <- filter(cpop, !(state=='New York' & county %in% nyccounties))
# add NYC back to population table
cpop <- bind_rows(cpop, nycpop)
cpop$pop1k <- cpop$totpop / 1000
spop$pop1k <- spop$totpop / 1000

cpop <- 
  cpop %>% 
  left_join(dhous, by=c('state','county', 'fips')) %>%
  left_join(select(dland, fips, state, county, landsq), by=c('state','county','fips'))

spop <- spop %>% left_join(dland_state, by=c('state','fips'))

write.table(cpop, 'cpop.txt',
            quote=F, sep=',', row.names=F)
write.table(spop, 'spop.txt',
            quote=F, sep=',', row.names=F)
