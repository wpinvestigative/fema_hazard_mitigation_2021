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

mega_df2b <- mega_df2 %>%
  select(disaster_approval_year, disaster_approval_year_type)

mega_df3b <- mega_df3 %>%
  select(disaster_closed, disaster_closed_type)

mega_df4b <- mega_df4 %>%
  select(disaster_wide, disaster_wide_type)


mega_df <- cbind(mega_df1, mega_df2b)
mega_df <- cbind(mega_df, mega_df3b)
mega_df <- cbind(mega_df, mega_df4b)

mega_df <- mega_df %>%
  mutate(months_approval=interval_approval %/% months(1)) %>%
  mutate(months_approval_year=interval_approval_year %/% months(1)) %>%
  mutate(months_closed=interval_closed %/% months(1)) %>%
  mutate(months_disaster=interval_disaster %/% months(1))

mega_df4 <- mega_df %>%
  left_join(county_race_majority_annual %>% select(-state, -county))

saveRDS(mega_df4, "data/clean/mega_df_pt2.RDS")


# which counties get no mitigation project money after 2010

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

saveRDS(no_mitigation, "data/clean/no_mitigation.RDS")

no_mitigation <- readRDS("data/clean/no_mitigation.RDS")



