

library(shiny)
library(shinythemes)
library(highcharter)

  
shinyUI(navbarPage("SOC it to me: A career recommender app", theme = shinytheme("cosmo"),
  
  tabPanel("Main",
    fluidPage(includeCSS("styles.css"), fluidRow(
      
      titlePanel("Start here:"),
      column(3, 
        wellPanel(style = "font-size:10pt", 
                  tags$style(type = "text/css", 
                             "
                              .irs-slider {width: 13px; height: 13px; top: 22px;} "),

        selectInput("CIPArea", "First, select general area of your degree", c(Area = "", names(CIP))),
        uiOutput("selectedCIP"),
        
        hr(),
        selectInput("selectedGender", "(Optional) Your gender", c(Select = "", "Male", "Female")),
        helpText("Note: Making a gender selection will bias results towards gender conformity."),
        
        hr(),
        helpText("Use the slider to indicate your personal interest in the following domains."),
        
        fluidRow(
          column(6,
          sliderInput("inRealistic", "Realistic", 0, min = 0, max = 7, val = 0, step= 0.5),
          sliderInput("inInvestigative", "Investigative", 0, min = 0, max = 7, val = 0, step= 0.5),
          sliderInput("inEnterprising",  "Enterprising", 0, min = 0, max = 7, val = 0, step= 0.5)
          ),
          column(6,
          sliderInput("inArtistic", "Artistic", 0, min = 0, max = 7, val = 0, step= 0.5),
          sliderInput("inSocial", "Social", 0, min = 0, max = 7, val = 0, step= 0.5),
          sliderInput("inConventional", "Conventional", 0, min = 0, max = 7, val = 0, step= 0.5)
          )),
        
        selectInput("selectedCompensation", "You'd rather have your job:", 
                    c("Fit interests as much as possible.", 
                      "Make more money even if interests match less well on these dimensions.")),
        
        hr(),
        actionButton("go", "Work it!")
        )
      ),
      
      column(9, fluidRow(
          
        column(5, h3("Results"), 
          uiOutput("Results"),
          hr(), h3("Job growth outlook"), 
          tableOutput("ProjectionTab"),
          helpText("Projections 2012-2022 are available for non-military occupations. 
                   Source: http://www.projectionscentral.com/"),
          hr(), h3("National Annual Pay $ Figures"),
          tableOutput("Compensation")
        ),
        
      
        column(7, 
            highchartOutput("InterestRadar"), p(),
          fluidRow(
            column(6, highchartOutput("GenderChart")),
            column(6, highchartOutput("GenerationChart"))
          )
        )
        
      )) 
    ))
  )
))
