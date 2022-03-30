library(devtools)
install_github(
  repo = 'walkerke/tidycensus',
  ref = 'estimates2021'
)


library(tidycensus)
library(dplyr)
library(reshape2)

# CENSUS
apikey <- Sys.getenv('CENSUS_API')
census_api_key(apikey)

# censusvars <- tidycensus::load_variables(
#   year = 2020,
#   dataset = 'acs5'
# )


flzcta <- get_acs(
  year = 2019,
  geography = 'zcta',
  state = 'FL',
  survey = 'acs5',
  # table = 'B25004'
  variables = c('B25004_006')
  # summary_var = 'B25001_001'
)
flzcta <- unique(flzcta$GEOID)

c <- get_acs(
  year = 2020,
  geography = 'zcta',
  # state = 'FL',
  survey = 'acs5',
  variables = c(
    'S0101_C01_002', # Total est. pop. under 5 years old
    'S0101_C02_002', # Est. pct younger than 5
    'S0101_C01_030', # Total estimated population ages 65 and older
    'S0101_C02_030', # Percent 65+
    'B01001_002', # Total estimated male population
    'B01001H_001', # non-Hispanic white
    'B01001B_001', # Black/African-American, non-Hipanic
    'B01001I_001', # Hispanic or Latino
    'S1901_C01_012', # Median income (in 2019 inlation-adjusted dollars)
    'S0101_C01_032', # Median age
    'S1501_C01_015', # Number of people 25 and older with a bachelor's degree or higher
    'S1501_C01_006', # Pop. 25 and older
    'S0502_C01_001', # Foreign-born population
    'S0502_C01_003', # Foreign-born non-citizen
    'S1601_C02_003', # Percent of pop. 5+ speaking language other than English at home
    'S2801_C02_012' # percent of households with internet subscriptions
  ),
  # cache_table = T,
  summary_var = 'B01001_001'
) %>%
  dcast(
    formula = ... ~ variable, # https://stackoverflow.com/questions/34234452/dcast-specific-column-and-keep-all
    value.var = 'estimate'
  ) %>%
  group_by(GEOID) %>%
  summarise(
    `estimated pop. younger than 5` = mean(S0101_C01_002, na.rm = T),
    `estimated pct. pop. younger than 5` = mean(S0101_C02_002, na.rm = T),
    `estimated pop. 65+` = mean(S0101_C01_030, na.rm = T),
    `estimated pct pop 65+` = mean(S0101_C02_030, na.rm = T),
    `estimated population` = mean(summary_est, na.rm = T),
    `Total estimated male population` = sum(B01001_002, na.rm = T),
    `non-Hispanic white` = sum(B01001H_001, na.rm = T),
    `Black/African-American, non-Hipanic` = sum(B01001B_001, na.rm = T),
    `Hispanic or Latino` = sum(B01001I_001, na.rm = T),
    `Median income (in 2019 inlation-adjusted dollars)` = sum(S1901_C01_012, na.rm = T),
    `Median age` = sum(S0101_C01_032, na.rm = T),
    `Number of people 25 and older with a bachelors degree or higher`= sum(S1501_C01_015, na.rm = T),
    `Pop. 25 and older` = sum(S1501_C01_006, na.rm = T),
    `Foreign-born population` = sum(S0502_C01_001, na.rm = T),
    `Percent foreign-born non-citizen` = sum(S0502_C01_003, na.rm = T),
    `Percent of pop. 5+ speaking language other than English at home` = sum(S1601_C02_003, na.rm = T),
    `percent of households with internet subscriptions` = sum(S2801_C02_012, na.rm = T)
  )

c$`percent male` <- c$`Total estimated male population` / c$`estimated population` * 100
c$`percent white` <- c$`non-Hispanic white` / c$`estimated population` * 100
c$`percent black` <- c$`Black/African-American, non-Hipanic` / c$`estimated population` * 100
c$`percent hispanic` <- c$`Hispanic or Latino` / c$`estimated population` * 100
c$`percent 25+ with college degree` <- c$`Number of people 25 and older with a bachelors degree or higher` / c$`Pop. 25 and older` * 100
c$`percent foreign-born` <- c$`Foreign-born population` / c$`estimated population` * 100
# c$`percent 65+` <- c$`estimated pop. 65+` / c$`estimated population` * 100

c$`est. pop. 5 and older` <- c$`estimated population` - c$`estimated pop. younger than 5`

cOccupancy <- get_acs(
  year = 2020,
  geography = 'zcta',
  # state = 'FL',
  survey = 'acs5',
  # table = 'B25004'
  variables = c('B25004_006'),
  summary_var = 'B25001_001'
) %>%
  dcast(
    formula = ... ~ variable, # https://stackoverflow.com/questions/34234452/dcast-specific-column-and-keep-all
    value.var = 'estimate'
  ) %>%
  group_by(GEOID) %>%
  summarise(
    `estimated households` = mean(summary_est, na.rm=T),
    `estimated vacant households for seasonal, recreational, or occasional use` = mean(B25004_006, na.rm=T)
  )

c <- merge(
  x = c,
  y = cOccupancy,
  by = 'GEOID',
  # all = T
) %>%
  filter(
    GEOID %in% flzcta
  )

write.csv(
  x = c,
  file = '2020-5year-assorted-stats-zcta5.csv',
  na = '',
  row.names = F
)
