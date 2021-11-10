library(tidyverse)
library(lubridate)

#mit <- read_csv("data/raw/HazardMitigationAssistanceMitigatedProperties.csv")

# import mitigation projects (no. 4 in the list)
proj <- read_csv("data/raw/HazardMitigationAssistanceProjects.csv")

# import disasters details
disasters <- read_csv("data/raw/DisasterDeclarationsSummaries090321.csv")

# just getting disasters by type and declaration date
dis_ids <- disasters %>% select(disasterNumber, incidentType, declarationDate) %>% unique()

# attaching that info to the projects dataframe
proj <- proj %>% left_join(dis_ids)

proj <- proj %>%
  # fixing problematic dates
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
  # estimating approval date
  # logic: if there is no InitiallyApproved date, then automatically it is dateApproved
  # if there are dates in InitiallyApproved and dateApproved, but InitiallyApproved is AFTER dateApproved,
  mutate(approval_date=case_when(
  is.na(dateInitiallyApproved) ~ dateApproved,
  !is.na(dateInitiallyApproved) & !is.na(dateApproved) & dateInitiallyApproved > dateApproved ~ dateApproved,
  TRUE ~ dateInitiallyApproved
  )) %>%
  mutate(dateClosed2=case_when(
    is.na(dateClosed) ~  mdy_hms("10-01-2021 00:00:00"),
    TRUE ~ dateClosed
  )) %>%
  # fiscal_date based on programFY
  #mutate(fiscal_date=mdy(paste0("10/01/", programFy-1))) %>%
  # if there's no approval date, use october 1, 2021 (why?)


  #mutate(approval_date=case_when(
  #  is.na(approval_date) ~ mdy_hms("10-01-2021 00:00:00"),
  #  TRUE ~ approval_date)
  #) %>%


  # measuring the time between fiscal date and approval date
  mutate(interval_approval=interval(declarationDate, approval_date)) %>%
  mutate(interval_approval_year=interval((declarationDate+months(18)), approval_date)) %>%
  mutate(interval_closed=interval(approval_date, dateClosed)) %>%
  mutate(interval_disaster=interval(declarationDate, dateClosed2)) %>%

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
  mutate(disaster_approval="-",
         disaster_approval_type="-",
         disaster_approval_year="-",
         disaster_approval_year_type="-",
         disaster_closed="-",
         disaster_closed_type="-",
         disaster_wide="-",
         disaster_wide_type="-")

# bringing in data from disasters again but this time creating a county geod dataframe
dis <- read_csv("data/raw/DisasterDeclarationsSummaries090321.csv") %>%
  mutate(GEOID=paste0(fipsStateCode,fipsCountyCode))

# creating array of county geoids
county_dis <- dis %>%
  select(GEOID) %>%
  unique()

# setting aside statewide declarations
state_dis <- dis %>%
  filter(designatedArea=="Statewide") %>%
  select(incidentBeginDate, state, fipsStateCode) %>%
  unique()

## finding disasters between intervals

# interval_approval
# *declarationDate - approval_date

# interval_approval_year
# *declarationDate +months(18) - approval_date

# interval_closed
# *approval_date - dateClosed

# interval_disaster
# *declarationDate - dateClosed2

#interval_approval
for (i in 1:nrow(county_dis)) {

  # going through each county geoid
  # pulling each disaster declaration that occurred in that county

  pulled_dis <- dis %>%
    filter(GEOID==county_dis$GEOID[i])

  # pulling mitigation projects in this county
  pulled_mit <- proj %>%
    filter(GEOID==county_dis$GEOID[i])

  # checking if there's more than zero disaster projects in this county
  if (nrow(pulled_mit)>0) {

    # going through every row of the mitigation projects
    for (x in 1:nrow(pulled_mit)) {

      # going through every row of disasters
      for(y in 1:nrow(pulled_dis)) {

        # making sure there is a start and end point in the intervals
        if (!grepl("NA", pulled_mit$interval_approval[x])) {

          # check if the start date of the specific disaster occurred within the interval of the specific project
          if (pulled_dis$incidentBeginDate[y] %within% pulled_mit$interval_approval[x]) {
            # appending disasters to itself
            pulled_mit$disaster_approval[x] <- paste0(pulled_mit$disaster_approval[x], ", ", pulled_dis$femaDeclarationString[y])
            pulled_mit$disaster_approval_type[x] <- paste0(pulled_mit$disaster_approval_type[x], ", ", pulled_dis$incidentType[y])
          }
        }
      }
    }
  }

if (i==1) {
  mega_df1 <- pulled_mit
} else {
  mega_df1 <- rbind(mega_df1, pulled_mit)
}
print(i)
}

# interval_approval_year
for (i in 1:nrow(county_dis)) {

  pulled_dis <- dis %>%
    filter(GEOID==county_dis$GEOID[i])

  pulled_mit <- proj %>%
    filter(GEOID==county_dis$GEOID[i])

  if (nrow(pulled_mit)>0) {
    for (x in 1:nrow(pulled_mit)) {
      for(y in 1:nrow(pulled_dis)) {
        if (!grepl("NA", pulled_mit$interval_approval_year[x])) {
          if (pulled_dis$incidentBeginDate[y] %within% pulled_mit$interval_approval_year[x]) {
            pulled_mit$disaster_approval_year[x] <- paste0(pulled_mit$disaster_approval_year[x], ", ", pulled_dis$femaDeclarationString[y])
            pulled_mit$disaster_approval_year_type[x] <- paste0(pulled_mit$disaster_approval_year_type[x], ", ", pulled_dis$incidentType[y])

            }
        }
      }
    }
  }

  if (i==1) {
    mega_df2 <- pulled_mit
  } else {
    mega_df2 <- rbind(mega_df2, pulled_mit)
  }
  print(i)
}

#interval_closed
for (i in 1:nrow(county_dis)) {

  pulled_dis <- dis %>%
    filter(GEOID==county_dis$GEOID[i])

  pulled_mit <- proj %>%
    filter(GEOID==county_dis$GEOID[i])

  if (nrow(pulled_mit)>0) {
    for (x in 1:nrow(pulled_mit)) {
      for(y in 1:nrow(pulled_dis)) {
        if (!grepl("NA", pulled_mit$interval_closed[x])) {
          if (pulled_dis$incidentBeginDate[y] %within% pulled_mit$interval_closed[x]) {
            pulled_mit$disaster_closed[x] <- paste0(pulled_mit$disaster_closed[x], ", ", pulled_dis$femaDeclarationString[y])
            pulled_mit$disaster_closed_type[x] <- paste0(pulled_mit$disaster_closed_type[x], ", ", pulled_dis$incidentType[y])

            }
        }
      }
    }
  }

  if (i==1) {
    mega_df3 <- pulled_mit
  } else {
    mega_df3 <- rbind(mega_df3, pulled_mit)
  }
  print(i)
}

#disaster_closed
for (i in 1:nrow(county_dis)) {

  pulled_dis <- dis %>%
    filter(GEOID==county_dis$GEOID[i])

  pulled_mit <- proj %>%
    filter(GEOID==county_dis$GEOID[i])

  if (nrow(pulled_mit)>0) {
    for (x in 1:nrow(pulled_mit)) {
      for(y in 1:nrow(pulled_dis)) {
        if (!grepl("NA", pulled_mit$interval_disaster[x])) {
          if (pulled_dis$incidentBeginDate[y] %within% pulled_mit$interval_disaster[x]) {
            pulled_mit$disaster_wide[x] <- paste0(pulled_mit$disaster_wide[x], ", ", pulled_dis$femaDeclarationString[y])
            pulled_mit$disaster_wide_type[x] <- paste0(pulled_mit$disaster_wide_type[x], ", ", pulled_dis$incidentType[y])

          }
        }
      }
    }
  }

  if (i==1) {
    mega_df4 <- pulled_mit
  } else {
    mega_df4 <- rbind(mega_df4, pulled_mit)
  }
  print(i)
}


mega_df1 <- mega_df1 %>%
  mutate(disaster_approval=gsub("-, ", "", disaster_approval)) %>%
  mutate(disaster_approval_type=gsub("-, ", "", disaster_approval_type))

mega_df1$disaster_approval <- ifelse(mega_df1$disaster_approval=="-", NA, mega_df1$disaster_approval)
mega_df1$disaster_approval_type <- ifelse(mega_df1$disaster_approval_type=="-", NA, mega_df1$disaster_approval_type)

mega_df2 <- mega_df2 %>%
  mutate(disaster_approval_year=gsub("-, ", "", disaster_approval_year))%>%
  mutate(disaster_approval_year_type=gsub("-, ", "", disaster_approval_year_type))

mega_df2$disaster_approval_year <- ifelse(mega_df2$disaster_approval_year=="-", NA, mega_df2$disaster_approval_year)
mega_df2$disaster_approval_year_type <- ifelse(mega_df2$disaster_approval_year_type=="-", NA, mega_df2$disaster_approval_year_type)

mega_df3 <- mega_df3 %>%
  mutate(disaster_closed=gsub("-, ", "", disaster_closed))%>%
  mutate(disaster_closed_type=gsub("-, ", "", disaster_closed_type))

mega_df3$disaster_closed <- ifelse(mega_df3$disaster_closed=="-", NA, mega_df3$disaster_closed)
mega_df3$disaster_closed_type <- ifelse(mega_df3$disaster_closed_type=="-", NA, mega_df3$disaster_closed_type)


mega_df4 <- mega_df4 %>%
  mutate(disaster_wide=gsub("-, ", "", disaster_wide))%>%
  mutate(disaster_wide_type=gsub("-, ", "", disaster_wide_type))

mega_df4$disaster_wide <- ifelse(mega_df4$disaster_wide=="-", NA, mega_df4$disaster_wide)
mega_df4$disaster_wide_type <- ifelse(mega_df4$disaster_wide_type=="-", NA, mega_df4$disaster_wide_type)

#
# mega_df1$disaster_approval_year <- NULL
# mega_df1$disaster_closed <- NULL
# mega_df1$disaster_wide <- NULL
# mega_df1$disaster_approval_year_type <- NULL
# mega_df1$disaster_closed_type <- NULL
# mega_df1$disaster_wide_type <- NULL

mega_df2b <- mega_df2 %>%
  select(disaster_approval_year, disaster_approval_year_type)

mega_df3b <- mega_df3 %>%
  select(disaster_closed, disaster_closed_type)

mega_df4b <- mega_df4 %>%
  select(disaster_wide, disaster_wide_type)


mega_df <- cbind(mega_df1, mega_df2b)
mega_df <- cbind(mega_df, mega_df3b)
mega_df <- cbind(mega_df, mega_df4b)




saveRDS(mega_df, "data/clean/mega_df_pt1.RDS")

#mega_df <- readRDS("data/clean/mega_df.RDS")

# which counties get no mitigation project money

no_mitigation <- c("")

proj <- filter(proj, year(declarationDate)>2010)
dis <- filter(dis, year(declarationDate)>2010)

for (i in 1:nrow(county_dis)) {

  pulled_dis <- dis %>%
    filter(GEOID==county_dis$GEOID[i])

  pulled_mit <- proj %>%
    filter(GEOID==county_dis$GEOID[i])

  if (nrow(pulled_mit)==0) {
  no_mitigation <- c(no_mitigation, county_dis$GEOID[i])
  }

  print(i)
}

saveRDS(no_mitigation, "data/clean/no_mitigtation.RDS")

no_mitigation <- readRDS("data/clean/no_mitigtation.RDS")

#overlapped <- filter(mega_df, !is.na(disaster))

#
# library(muckrakr)
#
# mega_df2 <- untangle(data=mega_df, x="projectType", pattern="[;]", verbose=TRUE)


# import censusdf data here

county_race_majority_annual <- read_csv("data/clean/county_race_majority.csv")

none <- data.frame(no_mitigation) %>%
  filter(no_mitigation!="") %>%
  count(no_mitigation) %>%
  rename(GEOID=no_mitigation) %>%
  left_join(county_race_majority_annual)

none %>%
  count(race) %>%
  mutate(percent=round(n/sum(n)*100,1))

county_race_majority_annual %>%
  ungroup() %>%
  count(race) %>%
  mutate(percent=round(n/sum(n)*100,1))

mega_df <- mega_df %>%
  #filter(!is.na(fiscal_date)) %>%
  #filter(!is.na(approval_date)) %>%
  mutate(months_approval=interval_approval %/% months(1)) %>%
  mutate(months_approval_year=interval_approval_year %/% months(1)) %>%
  mutate(months_closed=interval_closed %/% months(1)) %>%
  mutate(months_disaster=interval_disaster %/% months(1))

mega_df4 <- mega_df %>%
  left_join(county_race_majority_annual %>% select(-state, -county))

saveRDS(mega_df4, "data/clean/mega_df_pt2.RDS")


# this table is great
timing <- mega_df4 %>%
  group_by(race) %>%
  summarize(total=n(),
            avg_months_approval=mean(months_approval, na.rm=T),
            med_months_approval=median(months_approval, na.rm=T),
            avg_months_approval_year=mean(months_approval_year, na.rm=T),
            med_months_approval_year=median(months_approval_year, na.rm=T),
            avg_months_closed=mean(months_closed, na.rm=T),
            med_months_closed=median(months_closed, na.rm=T),
            ) %>%
  mutate(percent=round(total/sum(total)*100,1))

mostmoney <- mega_df %>%
  filter(programFy>2010) %>%
  group_by(programArea, projectType) %>%
  summarize(projects=n(),
            sum=sum(projectAmount, na.rm=T))


# i dont think i need this anymore ---

library(readxl)


pd <- read_excel("data/raw/disasters_mitigation_list.xlsx")

disasters <- read_csv("data/raw/DisasterDeclarationsSummaries090321.csv")

dis_ids <- disasters %>% select(disasterNumber, incidentType) %>% unique()

mega_df3 <- left_join(mega_df3, dis_ids)

mega_df <- left_join(mega_df, dis_ids)

mega_df$disasters_string <- NA
mega_df$disasters_during <- NA


for (i in 1:nrow(mega_df)) {

  disaster_array <- mega_df$disaster[i]

  if(!is.na(disaster_array)) {

    #disasters_row <- str_split("EM-3458-TX, EM-3458-TX, EM-3458-TX, EM-3501-TX, DR-4485-TX, DR-4485-TX, DR-4485-TX", ", ")
    disasters_row <- str_split(disaster_array, ", ")
    disasters_row <- disasters_row[[1]]
    disasters_fema <- filter(disasters) %>%
      filter(femaDeclarationString %in% disasters_row) %>%
      select(femaDeclarationString, incidentType) %>%
      unique()

    mega_df$disasters_string[i] <- paste0(disasters_fema$femaDeclarationString, collapse=", ")

    mega_df$disasters_during[i] <- paste0(disasters_fema$incidentType, collapse=", ")

    print(i)
  }

}

saveRDS(mega_df, "data/clean/big_disasters_df.RDS")

mega_df <- readRDS("data/clean/big_disasters_df.RDS")

nev <- filter(mega_df, state=="California" & county=="Nevada")


### okay, start here ---


# fires
fires <- filter(mega_df,
                     grepl("300.2", projectType) |
                  grepl("205.2", projectType) |
                  grepl("205.1", projectType) |
                  grepl("302.3", projectType) |#&
                  grepl("300.8", projectType) |
                  grepl("304.2", projectType) |
                  grepl("304.1", projectType) |
                  grepl("400.3", projectType) |
                  grepl("402.2", projectType) |
                  grepl("403.8", projectType) |
                  grepl("303.3", projectType)
                     ) %>%
  filter(programArea=="HMGP") %>%
  filter(programFy>=2010) %>%
  #filter(!is.na(county)) %>%
  mutate(fire_count_approval=str_count(disaster_approval_type, "Fire"),
         fire_count_approval_year=str_count(disaster_approval_year_type, "Fire"),
         fire_count_closed=str_count(disaster_closed_type, "Fire"))

# hurricane

hurricanes <- filter(mega_df,
                grepl("301.1", projectType) |
                  grepl("403.1", projectType) |
                  grepl("405.1", projectType) |
                  grepl("202.1", projectType) |#&
                  grepl("403.4", projectType) |
                  grepl("401.1", projectType) |
                  grepl("206.2", projectType) |
                  grepl("201.1", projectType) |
                  grepl("402.1", projectType) |
                  grepl("203.3", projectType) |
                  grepl("202.1", projectType) |
                  grepl("403.5", projectType) |
                  grepl("205.8", projectType) |
                  grepl("204.3", projectType) |
                  grepl("500.2a", projectType) |
                  grepl("201.3", projectType) |
                  grepl("203.1", projectType) |
                  grepl("403.2", projectType) |
                  grepl("404.1", projectType) |
                  grepl("206.1", projectType) |
                  grepl("205.7", projectType) |
                  grepl("303.1", projectType) |
                  grepl("500.3", projectType) |
                  grepl("500.3", projectType) |
                  grepl("204.4", projectType) |
                  grepl("200.2", projectType) |
                  grepl("201.2", projectType) |
                  grepl("200.3", projectType) |
                  grepl("500.1", projectType) |
                  grepl("202.4", projectType) |
                  grepl("203.2", projectType) |
                  grepl("500.2", projectType) |
                  grepl("403.3", projectType) |
                  grepl("202.3", projectType) |
                  grepl("303.2", projectType) |
                  grepl("403.2a", projectType) |
                  grepl("203.4", projectType) |
                  grepl("401.1a", projectType) |
                  grepl("402.4", projectType) |
                  grepl("403.1a", projectType) |
                  grepl("207.1a", projectType) |
                  grepl("405.1a", projectType) |
                  grepl("206.1a", projectType) |
                  grepl("403.4a", projectType) |
                  grepl("403.3a", projectType) |
                  grepl("206.2a", projectType) |
                  grepl("205.7a", projectType)
                ) %>%
  filter(programArea=="HMGP") %>%
  filter(programFy>=2010) %>%
  #filter(!is.na(county)) %>%
  mutate(hurricane_count_approval=str_count(disaster_approval_type, "Hurricane"),
         hurricane_count_approval_year=str_count(disaster_approval_year_type, "Hurricane"),
         hurricane_count_closed=str_count(disaster_closed_type, "Hurricane"))

# LET'S COUNT BY COUNTY?

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


saveRDS(counties_only, "data/clean/mega_df_counties.RDS")

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

county_data <- read_csv("data/clean/county_combined.csv")

county_data <- county_data %>%
  mutate(pov_quantile= ntile(pctpov, 4))

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

mega_df_counties <- mega_df %>%
  left_join(county_data)

#1. Disasters happening while counties are waiting.
#- what am i asking?
#- counties with the longest wait times?
#- counties with the most disasters during their wait time
#-

#2. What percentage of this money is unspent and
#3. Equity issues in how it's distributed




# fires
fires <- filter(mega_df_counties,
                grepl("300.2", projectType) |
                  grepl("205.2", projectType) |
                  grepl("205.1", projectType) |
                  grepl("302.3", projectType) |#&
                  grepl("300.8", projectType) |
                  grepl("304.2", projectType) |
                  grepl("304.1", projectType) |
                  grepl("400.3", projectType) |
                  grepl("402.2", projectType) |
                  grepl("403.8", projectType) |
                  grepl("303.3", projectType)
) %>%
  filter(programArea=="HMGP") %>%
  filter(programFy>=2010) %>%
  #filter(!is.na(county)) %>%
  mutate(fire_count_approval=str_count(disaster_approval_type, "Fire"),
         fire_count_approval_year=str_count(disaster_approval_year_type, "Fire"),
         fire_count_closed=str_count(disaster_closed_type, "Fire"))


# hurricane

hurricanes <- filter(mega_df_counties,
                     grepl("301.1", projectType) |
                       grepl("403.1", projectType) |
                       grepl("405.1", projectType) |
                       grepl("202.1", projectType) |#&
                       grepl("403.4", projectType) |
                       grepl("401.1", projectType) |
                       grepl("206.2", projectType) |
                       grepl("201.1", projectType) |
                       grepl("402.1", projectType) |
                       grepl("203.3", projectType) |
                       grepl("202.1", projectType) |
                       grepl("403.5", projectType) |
                       grepl("205.8", projectType) |
                       grepl("204.3", projectType) |
                       grepl("500.2a", projectType) |
                       grepl("201.3", projectType) |
                       grepl("203.1", projectType) |
                       grepl("403.2", projectType) |
                       grepl("404.1", projectType) |
                       grepl("206.1", projectType) |
                       grepl("205.7", projectType) |
                       grepl("303.1", projectType) |
                       grepl("500.3", projectType) |
                       grepl("500.3", projectType) |
                       grepl("204.4", projectType) |
                       grepl("200.2", projectType) |
                       grepl("201.2", projectType) |
                       grepl("200.3", projectType) |
                       grepl("500.1", projectType) |
                       grepl("202.4", projectType) |
                       grepl("203.2", projectType) |
                       grepl("500.2", projectType) |
                       grepl("403.3", projectType) |
                       grepl("202.3", projectType) |
                       grepl("303.2", projectType) |
                       grepl("403.2a", projectType) |
                       grepl("203.4", projectType) |
                       grepl("401.1a", projectType) |
                       grepl("402.4", projectType) |
                       grepl("403.1a", projectType) |
                       grepl("207.1a", projectType) |
                       grepl("405.1a", projectType) |
                       grepl("206.1a", projectType) |
                       grepl("403.4a", projectType) |
                       grepl("403.3a", projectType) |
                       grepl("206.2a", projectType) |
                       grepl("205.7a", projectType)
) %>%
  filter(programArea=="HMGP") %>%
  filter(programFy>=2010) %>%
  #filter(!is.na(county)) %>%
  mutate(hurricane_count_approval=str_count(disaster_approval_type, "Hurricane"),
         hurricane_count_approval_year=str_count(disaster_approval_year_type, "Hurricane"),
         hurricane_count_closed=str_count(disaster_closed_type, "Hurricane"))

write_csv(fires, "data/clean/fires.csv",na="")
write_csv(hurricanes, "data/clean/hurricanes.csv",na="")

counties_only2 <- counties_only %>% left_join(county_data)

county_names <- county_race_majority_annual %>%
  select(GEOID, state, county) %>%
  unique()
counties_only2 <- counties_only2 %>%
  left_join(county_names)
write_csv(counties_only2, "data/clean/counties_only.csv", na="")


nev <- filter(mega_df, county=="Nevada" & state=="California") %>%
  filter(programFy>2010)


nev %>%
  mutate(proj_name_id=paste(projectIdentifier, gsub(".*: ", "", projectType), "\n $", prettyNum(round(projectAmount), big.mark=","))) %>%
  mutate(dateClosed2=case_when(
    is.na(dateClosed) ~ mdy_hms("10-01-2021 00:00:00"),
    TRUE ~ dateClosed
  )) %>%
ggplot() +
  geom_segment(
    aes(x=declarationDate,
        y=proj_name_id,
        xend = dateClosed2,
        yend=proj_name_id),
        color="gray50") +
  geom_point(aes(x=declarationDate, y=proj_name_id), color="dark blue") +
  geom_point(aes(x=dateApproved, y=proj_name_id), color="dark green") +
  geom_point(aes(x=dateClosed, y=proj_name_id), color="dark red") +
  labs(title=paste0("Hazard Mitigation Projects in ", nev$NAME))

geoids <- mega_df$GEOID %>% unique()

for (x in 1:length(geoids)) {
  nev <- filter(mega_df, GEOID==geoids[x])

  for (i in 1:nrow(nev)) {

    # dis_array1 <- nev$disaster_approval[i]
    # dis_array_type1 <- nev$disaster_approval_type[i]
    #
    # dis_array2 <- nev$disaster_closed[i]
    # dis_array_type2 <- nev$disaster_closed_type[i]
    #
    dis_array <- nev$disaster_wide[i]
    dis_array_type <- nev$disaster_wide_type[i]

    # if (is.na(dis_array1)) {
    #   dis_array <- dis_array2
    #   dis_array_type <- dis_array_type2
    # } else if (is.na(dis_array2)) {
    #   dis_array <- dis_array1
    #   dis_array_type <- dis_array_type1
    # } else {
    #   dis_array <- paste0(dis_array1, ", ", dis_array2)
    #   dis_array_type <- paste0(dis_array_type1, ", ", dis_array_type2)
    # }


    dis_array <- unlist(strsplit(dis_array, ", "))
    dis_array_type <- unlist(strsplit(dis_array_type, ", "))

    df_array <- data.frame(dis_array, dis_array_type)
    colnames(df_array) <- c("femaDeclarationString", "disaster_type")
    df_array$GEOID <- nev$GEOID[i]
    df_array$county <- nev$county[i]
    df_array$state <- nev$state[i]
    df_array$declarationDate <- nev$declarationDate[i]
    df_array$dateApproved <- nev$dateApproved[i]
    df_array$dateClosed <- nev$dateClosed[i]
    df_array$NAME <- paste0(nev$county[i], " County, ", nev$state[i])
    df_array$projectIdentifier <- nev$projectIdentifier[i]
    df_array$projectType <- nev$projectType[i]
    df_array$projectAmount <- nev$projectAmount[i]

    if (!exists("big_df")) {
    big_df <- df_array
    } else {
      big_df <- rbind(big_df, df_array)
    }

  }
  print(x)
}


disasters_narrow <- disasters %>%
  select(femaDeclarationString, disaster_date=declarationDate)


big_df <- big_df %>%
  left_join(disasters_narrow)

write_csv(big_df, "data/clean/tall_counties_states.csv")
write_csv(big_df %>% filter(county!="Statewide"), "data/clean/tall_counties_only.csv")

big_df <- big_df %>%
  filter(county!="Statewide")

big_df2 <- big_df %>%
  filter(year(declarationDate)>2010)

write_csv(big_df2, "data/clean/tall_counties_only_2011.csv", na="")
saveRDS(big_df2, "data/clean/tall_counties_only_2011.RDS")

big_df %>%
  mutate(proj_name_id=paste(projectIdentifier, gsub(".*: ", "", projectType), "\n $", prettyNum(round(projectAmount), big.mark=","))) %>%
  mutate(dateClosed2=case_when(
    is.na(dateClosed) ~ mdy_hms("10-01-2021 00:00:00"),
    TRUE ~ dateClosed
  )) %>%
  ggplot() +
  geom_segment(
    aes(x=declarationDate,
        y=fct_reorder(proj_name_id, desc(declarationDate)),
        xend = dateClosed2,
        yend=fct_reorder(proj_name_id, desc(declarationDate))),
    color="gray50") +
  geom_point(aes(x=declarationDate, y=proj_name_id), color="dark blue") +
  geom_point(aes(x=dateApproved, y=proj_name_id), color="dark green") +
  geom_point(aes(x=dateClosed, y=proj_name_id), color="dark red") +
  geom_point(aes(x=disaster_date, y=proj_name_id), color="orange") +
  labs(title=paste0("Hazard Mitigation Projects in ", big_df$NAME),
       x="", y="") +
  theme_minimal() +
  annotate("point", x = as.POSIXct("2016-02-01"), y = 1.5, colour = "dark blue", size = 2) +
  annotate("point", x = as.POSIXct("2017-03-01"), y = 1.5, colour = "dark green", size = 2) +
  annotate("point", x = as.POSIXct("2018-04-01"), y = 1.5, colour = "dark red", size = 2) +
  annotate("point", x = as.POSIXct("2019-04-01"), y = 1.5, colour = "orange", size = 2) +

  annotate("text", x = as.POSIXct("2016-08-01"), y = 1.5, label = "Declaration date", size=3, colour="dark blue") +
  annotate("text", x = as.POSIXct("2017-09-01"), y = 1.5, label = "Project approved", size=3, colour="dark green") +
  annotate("text", x = as.POSIXct("2018-09-20"), y = 1.5, label = "Project closed", size=3, colour="dark red") +
  annotate("text", x = as.POSIXct("2019-07-20"), y = 1.5, label = "Disaster", size=3, colour="orange") #+

