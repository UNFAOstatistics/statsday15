library(dplyr)
library(ggplot2)

shinyServer(function(input, output, session) {
  

  output$group <- renderUI({
    groupNames <- groupTable[["groupName"]]
    opts <- selectInput("gc_name", "Which Group are you looking for:",choices = groupNames, selected=groupNames[2])
    list(opts)
  })
  
  output$domain <- renderUI({
    
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
    
    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
#    agg <- ind_or_agg[ind_or_agg$name == input$ind_or_agg,]$code
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
    opts <- sliderInput("year_range", "Select year range", min = minim, max = maxim, value = c(minim,maxim), step = 1)
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
  
#### Bivariate stuff


output$var_x_item <- renderUI({
  itemNames <- itemTable[["itemName"]]
  opts <- selectizeInput('varXitem', "Which item are you looking for:", choices = itemNames, selected=itemNames[1], multiple = FALSE)
  list(opts)
})

output$var_x_element <- renderUI({
  domainCode <- itemTable[itemTable[["itemName"]] == input$varXitem, ]$domainCode
  elementNames <- as.character(elementTable[elementTable[["domainCode"]] %in% domainCode, ]$elementName)
  opts <- selectizeInput('varXelement', "Which element are you looking for:", choices = elementNames, multiple = FALSE)
  list(opts)
})


output$var_y_item <- renderUI({
  itemNames <- itemTable[["itemName"]]
  opts <- selectizeInput('varYitem', "Which item are you looking for:", choices = itemNames, selected=itemNames[3], multiple = FALSE)
  list(opts)
})

output$var_y_element <- renderUI({
  domainCode <- itemTable[itemTable[["itemName"]] == input$varYitem, ]$domainCode
  elementNames <- as.character(elementTable[elementTable[["domainCode"]] %in% domainCode, ]$elementName)
  opts <- selectizeInput('varYelement', "Which element are you looking for:", choices = elementNames, multiple = FALSE)
  list(opts)
})


bivar_data <- reactive({
  
  domainCodeX <- itemTable[itemTable[["itemName"]] == input$varXitem, ]$domainCode
  domainCodeY <- itemTable[itemTable[["itemName"]] == input$varYitem, ]$domainCode
  
  dcX <- domainCodeX
  ecX <- elementTable[elementTable$elementName == input$varXelement & elementTable$domainCode == dcX,]$elementCode
  icX <- itemTable[itemTable[["itemName"]] == input$varXitem, ]$itemCode
  
  dcY <- domainCodeY
  ecY <- elementTable[elementTable$elementName == input$varYelement & elementTable$domainCode == dcY,]$elementCode
  icY <- itemTable[itemTable[["itemName"]] == input$varYitem, ]$itemCode
  
  
  Xvar <- getFAOtoSYB(domainCode = dcX, 
                     elementCode = ecX,
                     itemCode = icX)
  X <- Xvar[["entity"]]
  
  Yvar <- getFAOtoSYB(domainCode = dcY, 
                      elementCode = ecY,
                      itemCode = icY)
  Y <- Yvar[["entity"]]

  XY <- inner_join(X, Y, by = c("FAOST_CODE","Year"))
  na.omit(XY) 
    
})


output$bivar_year <- renderUI({
  year_ss <- bivar_data()
  unique(year_ss$Year)
  opts <- sliderInput("bivarYear", "", min = min(year_ss$Year), max = max(year_ss$Year), value = max(year_ss$Year), step = 1)
  list(opts)
})





output$single_scatter <- reactivePlot(function() {
  
  plot_data <- bivar_data()
  plot_data <- plot_data[plot_data$Year == input$bivarYear,]
  names(plot_data) <- c("FAOST_CODE","Year","valueX","valueY")
  plot_data <- merge(plot_data,reg,by="FAOST_CODE")
  
  p <- ggplot(plot_data, aes(x=valueX, y=valueY,group=1,color=Region,label=Country))
  p <- p + geom_point()
  p <- p + geom_text()
  p <- p + theme_minimal() + 
    theme(legend.position = "top") + 
    theme(text = element_text(family = "Open Sans", size= 12)) +
    theme(legend.title = element_text(size = 12, face = "bold")) +
    theme(axis.text= element_text(size = 10)) +
    theme(axis.title = element_text(size = 12, face = "bold")) +
    theme(legend.text= element_text(size = 12)) +
    theme(strip.text = element_text(size = 14, face="bold")) +
    guides(colour = guide_legend(override.aes = list(size=4)))
  p <- p + labs(x=input$varXitem,y=input$varYitem)
  p <- p + geom_smooth(method = "loess")
  print(p)
})


output$multi_scatter <- reactivePlot(function() {
  
  plot_data <- bivar_data()
  names(plot_data) <- c("FAOST_CODE","Year","valueX","valueY")
  plot_data <- merge(plot_data,reg,by="FAOST_CODE")
  
  p <- ggplot(plot_data, aes(x=valueX, y=valueY, group=1,color=Region,label=Country))
  p <- p + geom_point()
  p <- p + geom_text(size=2.5)
  p <- p + theme_minimal() + 
    theme(legend.position = "top") + 
    theme(text = element_text(family = "Open Sans", size= 12)) +
    theme(legend.title = element_text(size = 12, face = "bold")) +
    theme(axis.text= element_text(size = 10)) +
    theme(axis.title = element_text(size = 12, face = "bold")) +
    theme(legend.text= element_text(size = 12)) +
    theme(strip.text = element_text(size = 14, face="bold")) +
    guides(colour = guide_legend(override.aes = list(size=4)))
  p <- p + labs(x=input$varXitem,y=input$varyitem)
  p <- p + facet_wrap(~Year)
  p <- p + geom_smooth(method = "loess")
  print(p)
})


})