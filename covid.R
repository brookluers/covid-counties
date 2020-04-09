library(tidyverse)
library(zoo)
dc <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
ds <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
nyccounties <- c('Kings','Queens','New York','Bronx','Richmond')
# Required: run pop.R  to create spop.txt, cpop.txt
spop <- read_csv("spop.txt")
cpop <- read_csv("cpop.txt")
avgNdays <- 5
dc <- dc %>% left_join(cpop, by=c('state','county')) %>%
  mutate(log10c = ifelse(cases==0, NA,log10(cases)),
         log10d = ifelse(deaths==0, NA,log10(deaths)),
          case1k = cases / pop1k,
         d1k = deaths / pop1k,
         lc1k = ifelse(cases==0, NA, log10(cases / pop1k)),
         ld1k = ifelse(deaths==0, NA, log10(deaths / pop1k)),
         countystate = paste(county, state, sep=', ')) 
dc <- 
  dc %>%  
  group_by(county, state) %>% arrange(date) %>%
  mutate(newcase = c(cases[1], diff(cases)),
         newcase = ifelse(newcase < 0, 0, newcase),
         newdeath = c(deaths[1], diff(deaths)),
         newdeath = ifelse(newdeath < 0, 0, newdeath), 
         newcase_avg = rollmean(newcase, avgNdays, na.rm=TRUE, fill=NA, align='right'),
         newdeath_avg = rollmean(newdeath, avgNdays,na.rm=TRUE, fill=NA, align='right'),
         newcase_avg1k = newcase_avg / pop1k,
         newdeath_avg1k = newdeath_avg / pop1k,
         lnc = ifelse(newcase_avg == 0, NA, log10(newcase_avg)),
         lnd = ifelse(newdeath_avg == 0, NA, log10(newdeath_avg)),
         lnc1k = ifelse(newcase_avg1k==0, NA, log10(newcase_avg1k)),
         lnd1k = ifelse(newdeath_avg1k==0, NA, log10(newdeath_avg1k))) %>% ungroup

ds <- ds %>% left_join(spop, by='state') %>%
  mutate(log10c = ifelse(cases==0,NA,log10(cases)),
         log10d = ifelse(deaths==0,NA,log10(deaths)),
          case1k = cases / pop1k,
         d1k = deaths / pop1k,
         lc1k = ifelse(cases==0, NA, log10(cases / pop1k)),
         ld1k = ifelse(deaths==0, NA, log10(deaths / pop1k)))
ds <- 
  ds %>%
  group_by(state) %>% arrange(date) %>%
  mutate(newcase = c(cases[1], diff(cases)),
         newcase = ifelse(newcase < 0, 0, newcase),
         newdeath = c(deaths[1], diff(deaths)),
         newdeath = ifelse(newdeath < 0, 0, newdeath), 
         newcase_avg = rollmean(newcase, avgNdays, na.rm=TRUE, fill=NA, align='right'),
         newdeath_avg = rollmean(newdeath, avgNdays, na.rm=TRUE, fill=NA, align='right'),
         newcase_avg1k = newcase_avg / pop1k,
         newdeath_avg1k = newdeath_avg / pop1k,
         lnc = ifelse(newcase_avg == 0, NA, log10(newcase_avg)),
         lnd = ifelse(newdeath_avg == 0, NA, log10(newdeath_avg)),
         lnc1k = ifelse(newcase_avg1k==0, NA, log10(newcase_avg1k)),
         lnd1k = ifelse(newdeath_avg1k==0, NA, log10(newdeath_avg1k))) %>% ungroup

statenames <- unique(dc$state)

respOptions <- c('Cases (cumulative)','Deaths (cumulative)','Cases per 1,000 people (cumulative)',
  'Deaths per 1,000 people (cumulative)',
  'New cases', 'New deaths', 'New cases per 1,000 people',
  'New deaths per 1,000 people')
respVarMap <- 
  setNames(c('cases', 'deaths', 'case1k','d1k',
             'newcase_avg','newdeath_avg','newcase_avg1k', 'newdeath_avg1k'),
           respOptions)
lrespVarMap <- 
  setNames(c('log10c', 'log10d','lc1k','ld1k',
             'lnc', 'lnd', 'lnc1k', 'lnd1k'),
           respOptions)

respLabels <- 
  setNames(c('Cumulative cases','Cumulative deaths','Cumulative cases per\n1,000 people',
             'Cumulative deaths per\n1,000 people',
             paste('New cases\n(', avgNdays, '-day avg.)',sep=''),
             paste('New deaths\n(',avgNdays,'-day avg.)',sep=''),
             paste('New cases\nper 1,000 people\n(',avgNdays,'-day avg.)',sep=''),
             paste('New deaths\nper 1,000 people\n(',avgNdays,'-day avg.)',sep='')), 
           respOptions)
