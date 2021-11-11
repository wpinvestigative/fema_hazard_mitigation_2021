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


shinyUI(fluidPage(theme=shinytheme("flatly"),

                  # Inserting the Washington Post Investigative logo
                  list(tags$head(HTML('<link rel="icon", href="https://avatars3.githubusercontent.com/u/29076131?s=30&v=4",
                                      type="image/png" />'))),
                  div(style="padding: 1px 0px; width: '100%'",
                      titlePanel(
                        title="", windowTitle="FEMA Hazard mitigation project county explorer"
                      )
                  ),
                  navbarPage(
                    title=div(HTML("<img src='https://avatars3.githubusercontent.com/u/29076131?s=30&v=4' hspace='5'>"), "FEMA Hazard mitigation project county explorer"),
                    tabPanel("County",
                             sidebarLayout(
                               sidebarPanel(
                                 htmlOutput("state_selector"),
                                 htmlOutput("county_selector"),
                                 p("Disasters that occurred in counties while waiting for FEMA Hazard Mitigation Projects to finish."),
                                 #p("The Washington Post reviewed MLB footage and analyzed nearly 2 million pitches from data provided by Baseball Prospectus since 2017 when spin rates first started being tracked reliably. Details on The Post’s methodology and data can be found on GitHub.")
                               ),

                               mainPanel(

                                 plotOutput("top_chart", height="500px")#,
                                 #dataTableOutput("top_table")
                               ))
                    )

                  )

))

