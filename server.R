

library(shiny)
library(data.table)
library(highcharter)


load("data.RData")
load("OI.RData")
load("projections.RData")
load("natincome.RData")


shinyServer(function(input, output) {
  
  
  output$selectedCIP <- renderUI({ 
  if (input$CIPArea == "") return()
    selectInput("selectedCIP", label = NULL, choices = c(Degree = "", CIP[[input$CIPArea]]))
  })
  
  getJobs <- eventReactive(input$go, { 
    validate(need(input$selectedCIP, "Need degree input..."))
    group <- ifelse(input$selectedGender == "", "All", paste0(input$selectedGender, "s"))
    results <- data[.(input$selectedCIP, group), ][order(-N)][1:10]
    userintdata <- c(input$inRealistic, input$inInvestigative, input$inArtistic,
                     input$inSocial, input$inEnterprising, input$inConventional)
    intscores <- as.numeric(sapply(results$OccTitle, 
                                   function(x) dist(rbind(userintdata, OccIntData[[x]]))))
    intscores[is.na(intscores)] <- mean(intscores, na.rm = T)
    if (input$selectedCompensation == "Fit interests as much as possible.") {
      incomeboost <- rep(0,10) } else {
      incomeboost <- natincome[.(results$OccTitle), A_MEDIAN]
      incomeboost[is.na(incomeboost)] <- 0
      incomeboost <- incomeboost / 10000
    }
    score <- intscores - incomeboost
    results$OccTitle <- results$OccTitle[order(score)]
    results$OccTitle
  })
  
  
  output$Results <- renderUI({
    selectInput("Jobs", "Suggested jobs to explore",
                choices = getJobs(), selected = "")
    
  })
  
  getProjection <- eventReactive(input$Jobs, {
    projections[projections$Occupation == input$Jobs, 
                       c("Area", "PercentChange", "AverageAnnualOpenings")]
  })  
  
  
  output$ProjectionTab <- renderTable({
    tab <- getProjection()
    tab <- head(tab[order(tab$AverageAnnualOpenings, decreasing = T), ], 13)
    tab
    }, include.rownames = F, caption = "National and top 12 states data for job openings.
                                        Anything >20 % can be considered one of the fastest growing jobs.")
  
  
  
  output$Compensation <- renderTable({
    validate(need(input$Jobs, ""))
    natincome[.(input$Jobs), ]
    
  }, include.rownames = F, 
     caption = "Note: Jobs showing $187,200 means equal to or greater than $187,200.")
  
  
  
  byGender <- eventReactive(input$Jobs, {
    round(data[.(input$selectedCIP, c("Females", "Males"), input$Jobs), N/sum(N)], 2)
  })
  
  
  output$GenderChart <- renderHighchart({
    validate(need(input$Jobs, ""))
    gender <- byGender()
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_title(text = "Gender Breakdown (%)") %>% 
      hc_add_series(name = "Females",
                    data = gender[1]) %>%
      hc_add_series(name = "Males",
                    data = gender[2]) %>%
    hc_add_theme(hc_theme_538())
  })   
  
  
  byGeneration <- eventReactive(input$Jobs, {
     generation <- round(data[.(input$selectedCIP, c("Gen Y (1982-2000)", 
                                                     "Gen X (1965-1981)", 
                                                     "Baby Boomer (1946-1964)"), 
                 input$Jobs), N/sum(N)], 2)
  })
  
  output$GenerationChart <- renderHighchart({
    validate(need(input$Jobs, ""))
    generation <- byGeneration()
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_title(text = "Generation Breakdown (%)") %>% 
      hc_add_series(name = "Gen Y (1982-2000)",
                    data = generation[1]) %>%
      hc_add_series(name = "Gen X (1965-1981)",
                    data = generation[2]) %>%
      hc_add_series(name = "Baby Boomer (1946-1964)",
                    data = generation[3]) %>%
      hc_add_theme(hc_theme_538())
  })
  
  getInts <- eventReactive(input$Jobs, {
    c(input$inRealistic, input$inInvestigative, input$inArtistic,
      input$inSocial, input$inEnterprising, input$inConventional)
  })
  
  output$InterestRadar <- renderHighchart({
    validate(need(input$Jobs, ""))
    intdata <- OccIntData[[input$Jobs]]
    
    if (is.null(intdata)) {
    
      highchart() %>% 
      hc_title(text = "Interest Comparison Chart") %>% 
      hc_subtitle(text = " *Data not available for this occupation.") %>%
      hc_add_theme(hc_theme_538())
      
    } else {
      highchart() %>% 
        hc_chart(polar = TRUE, type = "area") %>% 
        hc_title(text = "Interest Comparison Chart") %>% 
        hc_xAxis(categories = c("Realistic", "Investigative", "Artistic", "Social", 
                                "Enterprising", "Conventional"),
                 tickmarkPlacement = 'on', lineWidth = 0,
                 labels = list(style = list(fontSize = "18px"))) %>% 
        hc_yAxis(lineWidth = 0, min = 0, max =7) %>% 
        hc_series(
          list(
            name = paste("Average Interest Profile for ", input$Jobs),
            data = intdata,
            pointPlacement = 'on'),
          list(
            name = "Your Interest Profile",
            data = getInts(),
            pointPlacement = 'on')) %>% 
        hc_credits(enabled = TRUE, text = "Source: http://www.onetcenter.org/", 
                   href = "http://www.onetcenter.org/") %>%
        hc_add_theme(hc_theme_538()) 
    }
  })

})   

