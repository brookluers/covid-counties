library(tidyverse)
dc <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
ds <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
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
nyccounties <- c('Kings','Queens','New York','Bronx','Richmond')
nycpop <- cpop %>% filter(state=='New York',
                county %in% nyccounties) %>% 
  summarise(totpop = sum(totpop)) %>%
  mutate(county='New York City',
         fips = NA, state='New York')

# remove NYC-separated counties from population table
cpop <- filter(cpop, !(state=='New York' & county %in% nyccounties))
# add NYC back to population table
cpop <- bind_rows(cpop, nycpop)
cpop$pop100 <- cpop$totpop / 100000
spop$pop100 <- spop$totpop / 100000

dc <- dc %>% left_join(cpop, by=c('state','county')) %>%
  mutate(log10c = ifelse(cases==0,NA,log10(cases)),
         log10d = ifelse(deaths==0,NA,log10(deaths)),
          case100 = cases / pop100,
         d100k = deaths / pop100,
         lc100 = ifelse(cases==0, NA, log10(cases / pop100)),
         ld100k = ifelse(deaths==0, NA, log10(deaths / pop100)),
         countystate=interaction(county,state))

ds <- ds %>% left_join(spop, by='state') %>%
  mutate(log10c = ifelse(cases==0,NA,log10(cases)),
         log10d = ifelse(deaths==0,NA,log10(deaths)),
        case100 = cases / pop100,
         d100k = deaths / pop100,
         lc100 = ifelse(cases==0, NA, log10(cases / pop100)),
         ld100k = ifelse(deaths==0, NA, log10(deaths / pop100)))

cscale_mm <- dc %>% group_by(state) %>%
  summarise(minc = min(cases),
            mind = min(deaths),
            maxc = max(cases),
            maxd = max(deaths))

statenames <- unique(dc$state)

respOptions <- c('Cases','Deaths','Cases per 100,000 people',
  'Deaths per 100,000 people')
respVarMap <- 
  setNames(c('cases', 'deaths', 'case100','d100k'),
           respOptions)
lrespVarMap <- 
  setNames(c('log10c', 'log10d','lc100','ld100k'),
           respOptions)
  
respLabels <- 
  setNames(c('Cases','Deaths','Cases per\n100,000 people',
             'Deaths per\n100,000 people'), respOptions)
