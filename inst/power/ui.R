# /******************************************************************************
# * Copyright (C) Mathematica Policy Research, Inc. 
# * This code cannot be copied, distributed or used without the express written permission
# * of Mathematica Policy Research, Inc. 
# *******************************************************************************/

## ui.R ##
library(shinydashboard)
library(MDES)
library(shinyBS)
header <- dashboardHeader(title = "Power dashboard")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Calculator", tabName = "Calculator", icon = icon("th")),
  menuItem("Help", tabName = "Help", icon = icon("th")),
  shinyURL::shinyURL.ui()
  ))

body <- dashboardBody(tabItems(
  # First tab content
  tabItem(tabName = "Calculator",
          fluidRow(
            box(title = "Assumptions", width = 3, solidHeader = TRUE, 
                status = "primary", 
                flowLayout(selectInput(
                  inputId = "Design", 
                  label = label_with_help(label = "Design:", id = "design_popover", size = h4),
                  c(
                    "Random Assigment" = "Random Assigment",
                    "Matched Comparison Group" = "Matched Comparison Group",
                    "DID" = "DID"
                  ),
                  selected = "Random Assigment"
                ),
                bsPopover(id = "design_popover",
                          title = "Designs",
                          content = "This dasboard can help you calculate Minimum Detectable Effect Sizes (MDESs) and Minimum Detectable Effects (MDEs) for three basic evaluation designs",
                          placement = "right",
                          trigger = "focus",
                          options = list(container = "body")
                          ),
                selectInput(
                  inputId = "sig.lvl", 
                  label = label_with_help(label = "Level of Significance:", id = "sig_lvl_popover", size = h4),
                  c(
                    "0.01" = 0.01,
                    "0.05" = 0.05,
                    "0.10" = 0.10
                  ),
                  selected = 0.05
                  ),
                
                bsPopover(id = "sig_lvl_popover",
                          title = "Type I error",
                          content = "Probability of incorrectly concluding that there is an impact when there is none",
                          placement = "right",
                          trigger = "focus",
                          options = list(container = "body")
                ),
                
                
                selectInput(
                  inputId = "power", 
                  label = label_with_help(label = "Power:", id = "power_popover", size = h4),
                  c(
                    "0.90" = 0.90,
                    "0.80" = 0.80,
                    "0.70" = 0.70
                  ),
                  selected = 0.80
                ),
                bsPopover(id = "power_popover",
                          title = "Power level",
                          content = "Probability of failing to detect an impact that truly exists",
                          placement = "right",
                          trigger = "focus",
                          options = list(container = "body")
                )
                
                )
                ),
            box(title = "Parameters", width = 3, solidHeader = TRUE, status = "primary",
                flowLayout(
                  numericInput(
                    "N", "Sample Size:", min = 2, step = 1, value = 5000
                  ),
                  
                  selectInput(
                    "Clusters", "Unit of Intervention:",
                    c("Group" = TRUE,
                      "Individual" = FALSE),
                    selected = FALSE
                  ),
                  
                  uiOutput("g"),
                  
                  sliderInput(
                    "p", "Probability of assignment to the treatment:", min = 0.001, max = 0.999, step =
                      0.01, value = 0.5
                  ),
                  
                  selectInput(
                    "outcome", "Outcome:",
                    c("Binary" = "binary",
                      "Continuous" = "continuous"),
                    selected = "continuous"
                  ),
                  
                  numericInput("num", label = uiOutput("txt.outcome"), value = 0.5, step = 0.01),
                  
                  uiOutput("rho"),
                  
                  uiOutput("dynUI")
                  
                )),
            box(title = "Minimum Detectable Impact", width = 6, 
                solidHeader = TRUE, status = "success", h3(textOutput("text1"))),
            
            h4(p("This dashboard was developed by Mathematica Policy Research for the Office of Educational Technology."),
               br(),
               p(HTML(paste0("This dashboard is an early prototype; please send us an ",
                             "<a href = \"mailto:EdTechRCE@mathematica-mpr.com\"> email </a>",
                             " with your feedback.")))
            ),
            h5(p('(C) Mathematica Policy Research, Inc.'))
          )),

  # Second tab content
  tabItem(tabName = "Help",
          h2("Help tab content"))
))

dashboardPage(skin = "red", header, sidebar, body)
