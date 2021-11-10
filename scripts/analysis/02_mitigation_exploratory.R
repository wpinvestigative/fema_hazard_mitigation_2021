library(tidyverse)

#1. Disasters happening while counties are waiting.
#2. What percentage of this money is unspent and
#3. Equity issues in how it's distributed

m1 <- read_csv("data/raw/HazardMitigationPlanStatuses.csv")

m1 %>%
  count(planStatus)

m1 %>%
  count(stateAbbreviation, countyName, planStatus)

m2 <- read_csv("data/raw/HazardMitigationGrantProgramDisasterSummaries.csv") %>%
  mutate(remaining=lockedInCeilingAmount-obligatedTotalAmount)

table(m2$hmgpCloseoutStatus)
# might have overall money here (state versus county or whatever)
# merge disasters with this one?
#lockedInCeilingAmount-obligatedTotalAmount?



m3 <- read_csv("data/raw/HazardMitigationAssistanceProjectsByNfipCrsCommunities.csv")

m4 <- read_csv("data/raw/HazardMitigationAssistanceProjects.csv")

nev <- filter(m4, county=="Nevada")

table(m4$status)

m4slice <- m4 %>%
  filter(programFy %in% c(2010:2021)) %>%
  group_by(programArea, status) %>%
  summarize(projects=n(),
            funding=sum(projectAmount),
            federal_funding=sum(federalShareObligated))

m4slice_year <- m4 %>%
  filter(programFy %in% c(2010:2021)) %>%
  group_by(programFy, status) %>%
  summarize(projects=n(),
            funding=sum(projectAmount),
            federal_funding=sum(federalShareObligated))

obligated <- m4 %>%
  filter(status=="Obligated")

all_others <- m4 %>%
  filter(programFy %in% c(2010:2021)) %>%
  filter(status=="Approved" | status=="Closed")


m4_slice20102021 <- m4 %>%
  filter(programFy %in% c(2010:2021))


m4_counties <- m4 %>%
  filter(programFy %in% c(2010:2021)) %>%
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
  mutate(GEOID=paste0(fipsStateCode, fipsCountyCode))

m4_counties_status <- m4_counties %>%
  mutate(dateClosed=case_when(
    dateClosed==ymd_hms("0218-08-29 04:56:02") ~ ymd_hms("2018-08-29 04:56:02"),
    dateClosed==ymd_hms("0218-03-06 04:56:02") ~ ymd_hms("2018-03-06 04:56:02"),
    dateClosed==ymd_hms("1018-12-19 04:56:02") ~ ymd_hms("2018-12-19 04:56:02"),
    dateClosed==ymd_hms("0016-07-26 04:56:02") ~ ymd_hms("2016-07-26 04:56:02"),
    dateClosed==ymd_hms("0219-10-22 04:56:02") ~ ymd_hms("2019-10-22 04:56:02"),
    dateClosed==ymd_hms("1018-12-20 04:56:02") ~ ymd_hms("2018-12-20 04:56:02"),
    dateClosed==ymd_hms("0620-07-07 04:56:02") ~ ymd_hms("2016-07-07 04:56:02"),
    dateClosed==ymd_hms("0019-03-22 04:56:02") ~ ymd_hms("2019-03-22 04:56:02"),
    TRUE ~ dateClosed
  )) %>%
  group_by(GEOID, status) %>%
  summarize(projects=n(),
            funding=sum(projectAmount),
            federal_funding=sum(federalShareObligated)) %>%
  group_by(GEOID) %>%
  mutate(percent_projects=round(projects/sum(projects, na.rm=T)*100,1),
         percent_funding=round(funding/sum(projects, na.rm=T)*100,1))

#0218-08-29 04:56:02
#0218-03-06 04:56:02
#1018-12-19 04:56:02
#0019-03-22 04:56:02
#0016-07-26 04:56:02
#0219-10-22 04:56:02
#1018-12-20 04:56:02
#0620-07-07 04:56:02
#0019-03-22 04:56:02


m4_counties_status_closed <- m4_counties %>%
  filter(status=="Closed") %>%
  mutate(dateClosed=case_when(
    dateClosed==ymd_hms("0218-08-29 04:56:02") ~ ymd_hms("2018-08-29 04:56:02"),
    dateClosed==ymd_hms("0218-03-06 04:56:02") ~ ymd_hms("2018-03-06 04:56:02"),
    dateClosed==ymd_hms("1018-12-19 04:56:02") ~ ymd_hms("2018-12-19 04:56:02"),
    dateClosed==ymd_hms("0016-07-26 04:56:02") ~ ymd_hms("2016-07-26 04:56:02"),
    dateClosed==ymd_hms("0219-10-22 04:56:02") ~ ymd_hms("2019-10-22 04:56:02"),
    dateClosed==ymd_hms("1018-12-20 04:56:02") ~ ymd_hms("2018-12-20 04:56:02"),
    dateClosed==ymd_hms("0620-07-07 04:56:02") ~ ymd_hms("2016-07-07 04:56:02"),
    dateClosed==ymd_hms("0019-03-22 04:56:02") ~ ymd_hms("2019-03-22 04:56:02"),
    TRUE ~ dateClosed
  )) %>%
  mutate(dateInitiallyApproved=case_when(
    dateInitiallyApproved==ymd_hms("0016-06-10 04:56:02") ~ ymd_hms("2016-06-10 04:56:02"),
    dateInitiallyApproved==ymd_hms("1018-06-21 04:56:02") ~ ymd_hms("2018-06-21 04:56:02"),
    dateInitiallyApproved==ymd_hms("1018-06-21 04:56:02") ~ ymd_hms("2018-06-21 04:56:02"),
    dateInitiallyApproved==ymd_hms("7019-11-01 04:00:00") ~ ymd_hms("2019-11-01 04:00:00"),
    TRUE ~ dateInitiallyApproved
  )) %>%
  mutate(approval_date=case_when(
    is.na(dateInitiallyApproved) ~ dateApproved,
    TRUE ~ dateInitiallyApproved
  )) %>%
  mutate(timespan=interval(approval_date, dateClosed)) %>%
  mutate(months=timespan %/% months(1)) %>%
  group_by(GEOID, status) %>%
  summarize(months=mean(months)) %>%
  select(GEOID, months)


# loading in a shapefiles package
library(tidycensus)


# getting map
county_map <- get_acs(geography = "county",
                      variables = "B03002_001",
                      survey="acs5",
                      year=2019,
                      geometry=T,
                      shift_geo=T)


county_map <- county_map %>%
  left_join(m4_counties_status %>% filter(status=="Closed")) %>%
  left_join(m4_counties_status_closed)

ggplot(county_map) +
  geom_sf(aes(fill=percent_projects, geometry=geometry), color="white") +
  theme_void() +
#  scale_fill_manual(values = c("grey", "red")) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Percent of closed projects by county",
       subtitle="Since 2010",
       caption="Data: FEMA")


county_data <- read_csv("data/clean/county_combined.csv")


counties <- county_map %>% full_join(county_data)

counties %>%
  group_by(majority) %>%
  summarize(total=n(),
            percent_projects=mean(percent_projects, na.rm=T),
            avg_months=mean(months, na.rm=T))

counties %>%
  group_by(plurality) %>%
  summarize(total=n(),
            percent_projects=mean(percent_projects, na.rm=T),
            avg_months=mean(months, na.rm=T))


counties %>%
  group_by(urban_rural) %>%
  summarize(total=n(),
            percent_projects=mean(percent_projects, na.rm=T),
            avg_months=mean(months, na.rm=T))

counties %>%
  mutate(pov_quantile= ntile(pctpov, 4)) %>%
  group_by(pov_quantile) %>%
  summarize(total=n(),
            percent_projects=mean(percent_projects, na.rm=T),
            avg_months=mean(months, na.rm=T))

#ntiles of poverty
#1 is most poverty?
#


m5 <- read_csv("data/raw/HazardMitigationAssistanceMitigatedProperties.csv")

table(m5$status)

disasters <- read_csv("data/raw/DisasterDeclarationsSummaries090321.csv")

dis_ids <- disasters %>% select(disasterNumber, incidentType, declarationDate) %>% unique()

m4 <- m4 %>% left_join(dis_ids)

# can i connect specific originating disasters to mitigation projects?

# oka, i think so...



