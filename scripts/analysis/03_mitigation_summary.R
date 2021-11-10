library(tidyverse)
library(lubridate)

#mega_df <- readRDS("data/clean/big_disasters_df.RDS") %>%
mega_df <- readRDS("data/clean/mega_df_pt2.RDS") %>%
  filter(programFy>2010) %>%
  filter(programArea=="HMGP")

write_csv(mega_df, "data/clean/mega_df_pt2.csv", na="")
mega_df <- read_csv("data/clean/mega_df_pt2.csv")

# how many hazard mitigation projects since 2010?

nrow(mega_df)

## 8664

# how many are still open?

sum(is.na(mega_df$dateClosed))

## 4405

county_data <- read_csv("data/clean/county_combined.csv")

counties_only <- mega_df %>%
  filter(programArea=="HMGP") %>%
  filter(programFy>=2010) %>%
  filter(status!="Obligated") %>%
  mutate(fipsStateCode=case_when(
    nchar(stateNumberCode)==1 ~ paste0("0", stateNumberCode),
    TRUE ~ as.character(stateNumberCode)
  )) %>%
  mutate(fipsCountyCode=case_when(
    nchar(countyCode)==1 ~ paste0("00", countyCode),
    nchar(countyCode)==2 ~ paste0("0", countyCode),
    is.na(countyCode) ~ "000",
    TRUE ~ as.character(countyCode)
  )) %>%
  mutate(GEOID=paste0(fipsStateCode, fipsCountyCode)) %>%
  group_by(GEOID, status) %>%
  summarize(projects=n(),
            funding=sum(projectAmount),
            federal_funding=sum(federalShareObligated),
            average_months_approval=mean(months_approval_year, na.rm=T),
            average_months_closed=mean(months_closed, na.rm=T)) %>%
  group_by(GEOID) %>%
  mutate(percent_projects=round(projects/sum(projects, na.rm=T)*100,1),
         percent_funding=round(funding/sum(funding, na.rm=T)*100,1)) %>%
  left_join(county_data)

# 1. What percent of money is unspent

mega_df %>%
  group_by(status) %>%
  summarize(spent=sum(projectAmount, na.rm=T)) %>%
  mutate(percent_money=round(spent/sum(spent, na.rm=T)*100,1))# %>%
 # kable()

## 9.4 Billion unspent (85%)
## 1.6 Billion spent (15%)

# 2. What percent of projects are still open

mega_df %>%
  group_by(status) %>%
  summarize(total=n()) %>%
  mutate(percent_projects=round(total/sum(total, na.rm=T)*100,1)) #%>%
 # kable()

## half are open and half are closed!

# 3. What's the average wait time to approve projects after a disaster?

mega_df %>%
  #group_by(status) %>%
  summarize(months_approval=round(mean(months_approval, na.rm=T),1))

## 22 months

# 4. What's the average time between approval and close?

mega_df %>%
  #group_by(status) %>%
  summarize(months_closed=round(mean(months_closed, na.rm=T),1))

## 40 months

# 5. What's the average time between disaster and close?
mega_df %>%
  mutate(interval_disaster_closed=interval(declarationDate, dateClosed)) %>%
  mutate(months_disaster_closed=interval_disaster_closed %/% months(1)) %>%
  summarize(months_approval_closed=round(mean(months_disaster_closed, na.rm=T),1))


## 60 months

# 5. What's the average wait time for projects still open?

## STILL PENDING

# 6. How many projects are there a year?

mega_df %>%
  mutate(declaration_year=year(declarationDate)) %>%
  count(declaration_year)

#sum(is.na(mega_df$declarationDate))

## oddly enough there are fewer disasters declared in the past few years

mega_df %>%
  mutate(approved_year=year(dateApproved)) %>%
  count(approved_year)

## but there are more projects being approved annually

mega_df %>%
  mutate(approved_year=year(dateApproved)) %>%
  group_by(approved_year) %>%
  summarize(sum=sum(projectAmount, na.rm=T))

## the project costs approved have grown exponentially-- up 876% over 10 years.

# 7. Approval time taking more time annually?
mega_df %>%
  #group_by(status) %>%
  mutate(declaration_year=year(declarationDate)) %>%
  group_by(declaration_year) %>%

#  mutate(approved_year=year(dateApproved)) %>%
#  group_by(approved_year) %>%
  summarize(total=n(),
            months_approval=round(mean(months_approval, na.rm=T),1))

### this one is complicated

# 8. Closing time taking more time annually?
mega_df %>%
  #group_by(status) %>%
  mutate(approved_year=year(dateApproved)) %>%
  group_by(approved_year) %>%
  summarize(total=n(),
            months_closed=round(mean(months_closed, na.rm=T),1)) %>%
  ggplot(aes(x=approved_year, y=months_closed)) +
  geom_col() +
  labs(title="Months it takes to close projects by year")

### yes, very much so

# 9. time between disaster and closing by year?
mega_df %>%
  filter(!is.na(dateClosed)) %>%
  mutate(interval_disaster_closed=interval(declarationDate, dateClosed)) %>%
  mutate(months_disaster_closed=interval_disaster_closed %/% months(1)) %>%
  mutate(declaration_year=year(declarationDate)) %>%
  group_by(declaration_year) %>%
  summarize(months_approval_closed=round(mean(months_disaster_closed, na.rm=T),1),
            total=n())# %>%
  #ggplot(aes(x=declaration_year, y=months_approval_closed)) +
  #geom_col() +
  #labs(title="Months it takes to close projects by year")

mega_df %>%
  filter(is.na(dateClosed)) %>%
  mutate(dateClosed=case_when(
    is.na(dateClosed) ~ mdy_hms("10-01-2021 00:00:00"),
    TRUE ~ dateClosed)
  ) %>%
  mutate(interval_disaster_closed=interval(declarationDate, dateClosed)) %>%
  mutate(months_disaster_closed=interval_disaster_closed %/% months(1)) %>%
  mutate(declaration_year=year(declarationDate)) %>%
  group_by(declaration_year) %>%
  summarize(months_approval_closed=round(mean(months_disaster_closed, na.rm=T),1),
            total=n())# %>%
#ggplot(aes(x=declaration_year, y=months_approval_closed)) +
#geom_col() +
#labs(title="Months it takes to close projects by year")


# 9. What percent of money is unspent by rural / urban areas

mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  group_by(status, urban_rural) %>%
  summarize(spent=sum(projectAmount, na.rm=T)) %>%
  group_by(urban_rural) %>%
  mutate(percent_money=round(spent/sum(spent, na.rm=T)*100,1))

## 1 is urban, 6 is rural
## nothing really interesting here

# 10. What percent of projects are still open by rural / urban areas

mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  group_by(status, urban_rural) %>%
  #summarize(spent=sum(projectAmount, na.rm=T)) %>%
  summarize(total=n()) %>%
  group_by(urban_rural) %>%
  mutate(percent_projects=round(total/sum(total, na.rm=T)*100,1))

## 1 is urban, 6 is rural
## nothing really interesting here

# 11. What's the average wait time to approve projects by rural / urban areas?

mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  group_by(urban_rural) %>%
  summarize(months_approval=round(mean(months_approval, na.rm=T),1))

## hm, longer to approve in urban areas...

# 12. What's the average time between approval and close by rural / urban areas?

mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  group_by(urban_rural) %>%
  summarize(months_closed=round(mean(months_closed, na.rm=T),1))

## longer to close in urban areas...

# 13. What's the average wait time for projects still open by rural / urban areas?

## PENDING

# 14. What percent of money is unspent by poverty percentiles

mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  mutate(pov_quantile= ntile(pctpov, 4)) %>%
  group_by(status, pov_quantile) %>%
  summarize(spent=sum(projectAmount, na.rm=T)) %>%
  group_by(pov_quantile) %>%
  mutate(percent_money=round(spent/sum(spent, na.rm=T)*100,1))

## higher the number, the more poverty
## poorer the county, the less closed

# 15. What percent of projects are still open by poverty percentiles

mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  mutate(pov_quantile= ntile(pctpov, 4)) %>%
  group_by(status, pov_quantile) %>%
  summarize(total=n()) %>%
  group_by(status) %>%
  mutate(percent_projects=round(total/sum(total, na.rm=T)*100,1))

## not sure about this

# 16. What's the average wait time to approve projects by poverty percentiles

mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  mutate(pov_quantile= ntile(pctpov, 4)) %>%
  group_by(pov_quantile) %>%
  summarize(months_approval=round(mean(months_approval, na.rm=T),1))

## not sure about this


# 17. What's the average time between approval and close by poverty percentiles


mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  mutate(pov_quantile= ntile(pctpov, 4)) %>%
  group_by(pov_quantile) %>%
  summarize(months_closed=round(mean(months_closed, na.rm=T),1))

## this looks okay

# 18. What's the average wait time for projects still open by poverty percentiles

## Pending!

# 19. How many dollars per resident are being sent by rural / urban areas?

mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  group_by(urban_rural, status) %>%
  summarize(population=sum(population, na.rm=T),
            spent=sum(projectAmount, na.rm=T)) %>%
  mutate(per_capita=round(spent/population*100,1))

## fix this...

# 20. How many dollars per resident are being sent by poverty percentile?


mega_df %>%
  select(-majority) %>%
  left_join(county_data) %>%
  mutate(pov_quantile= ntile(pctpov, 4)) %>%
  group_by(pov_quantile, status) %>%
  summarize(population=sum(population, na.rm=T),
            spent=sum(projectAmount, na.rm=T)) %>%
  mutate(per_capita=round(spent/population*100,1))

## ehhh, not into it


# 21. Which counties aren't getting any money at all? What's their demographic profile?

no_mitigation <- readRDS("data/clean/no_mitigtation.RDS")

county_data_none <- county_data %>%
  mutate(pov_quantile= ntile(pctpov, 4)) %>%
  filter(GEOID %in% no_mitigation)

county_data_none %>%
  count(urban_rural)

county_data_none %>%
  count(pov_quantile)


# 22. Disasters happening while counties are waiting?



#1. Disasters happening while counties are waiting.
#2. What percentage of this money is unspent and
#3. Equity issues in how it's distributed
