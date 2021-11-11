# install packages if missing
packages <- c("tidyverse", "shiny", "shinyWidgets", "lubridate", "shinythemes", "DT")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "https://cran.us.r-project.org")
}


library(tidyverse)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(shinythemes)
library(DT)


# mega_df <- readRDS("data/clean/mega_df_pt1.RDS") %>%
#   filter(programArea=="HMGP") %>%
#   select(state, county,
#          projectIdentifier, projectType, status,
#          projectAmount, declarationDate, dateApproved, dateClosed,
#          disaster_wide, disaster_wide_type) %>%
#   filter(year(declarationDate)>2010)
#
# saveRDS(mega_df, "data/clean/mega_df_shiny.RDS")

#mega_df <- readRDS("data/clean/mega_df_shiny.RDS")
#mega_df <- readRDS("mega_df_shiny.RDS")

#county_data <- readRDS("data/clean/tall_counties_only_2011.RDS") %>%
county_data <- readRDS("data/clean/tall_counties_only_2011.RDS") %>%

  #filter(state!="Puerto Rico") %>%
  filter(state!="Guam") %>%
  filter(state!="Northern Mariana Islands") %>%
  filter(state!="Virgin Islands of the U.S.") %>%
  filter(state!="American Samoa")

state_names <- county_data %>%
  pull(state) %>% unique() %>%
  sort()

shinyServer(function(input, output) {


  output$state_selector <- renderUI({

    pickerInput(
      inputId = "state",
      label = "State:",
      choices = state_names,
      options = list(`live-search`=TRUE),
      selected ="California")

  })

  output$county_selector <- renderUI({
    req(input$state)

    available <- county_data[county_data$state == input$state, "county"] %>% unique() %>% sort()

    #available <- filter(available, !is.na(county)) %>%
    #  arrange(county) %>%
    #  unique()

    pickerInput(
      inputId = "county",
      label = "County:",
      choices = available,
      options = list(`live-search`=TRUE),
      selected = available[1]
    )

  })


  #county_df <- ""
  makeReactiveBinding("county_df")

  county_df <- reactive({

    county_df <- county_data %>%
      filter(state==input$state) %>%
      filter(county==input$county)
  })


  #county_df <- ""
#  makeReactiveBinding("table_df")

 # table_df <- reactive({

#    table_df <- mega_df %>%
#      filter(state==input$state) %>%
#      filter(county==input$county)
#  })




  output$top_chart <- renderPlot({

    county_df <- county_df()


    county_df %>%
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
      labs(title=paste0("Hazard Mitigation Projects in ", county_df$NAME),
           x="", y="",
           caption="FEMA") +
      theme_minimal() +
      annotate("point", x = as.POSIXct("2016-02-01"), y = 1.5, colour = "dark blue", size = 2) +
      annotate("point", x = as.POSIXct("2017-03-01"), y = 1.5, colour = "dark green", size = 2) +
      annotate("point", x = as.POSIXct("2018-04-01"), y = 1.5, colour = "dark red", size = 2) +
      annotate("point", x = as.POSIXct("2019-04-01"), y = 1.5, colour = "orange", size = 2) +

      annotate("text", x = as.POSIXct("2016-08-01"), y = 1.5, label = "Declaration date", size=3, colour="dark blue") +
      annotate("text", x = as.POSIXct("2017-09-01"), y = 1.5, label = "Project approved", size=3, colour="dark green") +
      annotate("text", x = as.POSIXct("2018-09-20"), y = 1.5, label = "Project closed", size=3, colour="dark red") +
      annotate("text", x = as.POSIXct("2019-07-20"), y = 1.5, label = "Disaster", size=3, colour="orange") #+
  })

  #output$top_table <- renderDataTable(
    #table_df=table_df()
  #  mega_df %>%
  #    filter(state==input$state) %>%
  #    filter(county==input$county)
  #)

})
