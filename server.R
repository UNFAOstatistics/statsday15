library(dplyr)
library(FAOSTAT)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$Region)
  )
  
  
  output$group <- renderUI({
    groupTable <- FAOmetaTable[[1]]
    groupNames <- groupTable[["groupName"]]
    opts <- selectInput("gc_name", "Which Group are you looking for:",choices = groupNames, selected=groupNames[2])
    list(opts)
  })
  
  output$domain <- renderUI({
    domainTable <- FAOmetaTable[[2]]
    gc <- groupTable[groupTable$groupName == input$gc_name,]$groupCode
#     x <- groupNames[1]
#     gc <- groupTable[groupTable$groupName == x,]$groupCode
    subdomainTable <- domainTable[domainTable$groupCode == gc,]
    domainNames <- subdomainTable[["domainName"]]
    opts <- selectInput("domain_name", "Which Domain are you looking for:",
                        choices = domainNames, selected=domainNames[1])
    list(opts)
  })
  
  output$indOrAgg <- renderUI({
    ind_or_agg <- data.frame(name = c("(0) Individual item (e.g. Apples, Wheat)",
                                      "(1) Aggregated item (e.g. Total cereals, Total meat"),
                             code = c(0,1),
                             stringsAsFactors = FALSE
                             )
    values <- ind_or_agg$name
    opts <- selectInput("ind_or_agg", "Are you looking for individual item or aggregated item:",
                        choices = values, selected=values[1])
    list(opts)
  })
  
  output$item <- renderUI({
    
    itemTable <- FAOmetaTable[[3]]
    itemAggTable <- FAOmetaTable[[4]]
    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    agg <- ind_or_agg[ind_or_agg$name == input$ind_or_agg,]$code
#     x <- values[2]
#     agg <- ind_or_agg[ind_or_agg$name == x,]$code
    #if (agg == 1) {
    #  subitemTable = itemAggTable[itemAggTable$domainCode == dc,]
    #} else {
      subitemTable = itemTable[itemTable$domainCode == dc,]
    #}
    values <- subitemTable$itemName
    opts <- selectInput("item_name", "Which Item are you looking for?",
                        choices = values, selected=values[1])
    list(opts)
  })
  
  output$element <- renderUI({
    
    elementTable <- FAOmetaTable[[5]]
    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    subelementTable = elementTable[elementTable$domainCode == dc,]
    values <- as.character(subelementTable$elementName)
    opts <- selectInput("element_name", "Which Element are you looking for?",
                        choices = values, selected=values[1])
    list(opts)
  })
  
  fao_data <- reactive({

    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    ec <- elementTable[elementTable$elementName == input$element_name & elementTable$domainCode == dc,]$elementCode
    ic <- itemTable[itemTable$itemName == input$item_name & itemTable$domainCode == dc,]$itemCode

    dat <- getFAOtoSYB(domainCode = dc, 
                       elementCode = ec,
                       itemCode = ic)
     dat[["entity"]]
  })
  
  output$yearRange <- renderUI({
    
    data <- fao_data()
    maxim <- max(data$Year)
    minim <- min(data$Year)
    opts <- sliderInput("year_range", "Select year range", min = minim, max = maxim, value = c(minim,maxim))
    list(opts)
  })
  
  fao_data_filtered <- reactive({
    
    data <- fao_data()
    data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>% 
      arrange(FAOST_CODE)
  })
  

  output$mytable = renderDataTable({
    fao_data_filtered()
  },options = list(pageLength = 10))
  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$domain_name,input$element_name,input$item_name,input$year_range[1],input$year_range[2],'.csv', sep="-") }, 
    content = function(file) {
      write.csv(fao_data_filtered(), file, row.names = FALSE)
    }
  )
  
  output$timeSeries <- reactivePlot(function() {
    
    plot_data <- fao_data_filtered()
    names(plot_data) <- c("FAOST_CODE","Year","value")
    plot_data <- merge(plot_data,reg,by="FAOST_CODE")
    
    p <- ggplot(plot_data, aes(x=Year, y=value,group=FAOST_CODE,color=Region))
    p <- p + geom_point() + geom_line()
    p <- p + geom_text(data=plot_data[plot_data$Year ==input$year_range[2],],
                       aes(x=Year,y=value,label=Country))
    p <- p + theme_minimal() + 
      theme(legend.position = "top") + 
      theme(text = element_text(family = "Open Sans", size= 12)) +
      theme(legend.title = element_text(size = 12, face = "bold")) +
      theme(axis.text= element_text(size = 10)) +
      theme(axis.title = element_text(size = 12, face = "bold")) +
      theme(legend.text= element_text(size = 12)) +
      theme(strip.text = element_text(size = 14, face="bold")) +
      guides(colour = guide_legend(override.aes = list(size=4)))
    print(p)
  })
  




})