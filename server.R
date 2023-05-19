#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)

## Initial Data
airbnb <- fread("Data/Aemf1.csv")

airbnb <- airbnb %>% select(City, Price, Day, `Room Type`, `Person Capacity`,
                            Superhost, `Cleanliness Rating`, 
                            `Guest Satisfaction`, Bedrooms, `City Center (km)`,
                            `Metro Distance (km)`, `Attraction Index`,
                            `Normalised Attraction Index`,
                            `Restraunt Index`, `Normalised Restraunt Index`) %>%
  rename(Room_Type = `Room Type`, 
         Person_Capacity = `Person Capacity`,
         Cleanliness_Rating = `Cleanliness Rating`,
         Guest_Satisfaction_Rating = `Guest Satisfaction`,
         City_Center_Distance = `City Center (km)`,
         Metro_Distance = `Metro Distance (km)`,
         Attraction_Index = `Attraction Index`,
         Normalized_Attraction_Index = `Normalised Attraction Index`,
         Restaurant_Index = `Restraunt Index`,
         Normalized_Restaurant_Index = `Normalised Restraunt Index`) %>%
  relocate(City, Price, Superhost, Day, Room_Type, Bedrooms, Person_Capacity,
           Guest_Satisfaction_Rating, Cleanliness_Rating, City_Center_Distance,
           Metro_Distance, Attraction_Index, Normalized_Attraction_Index, 
           Restaurant_Index, Normalized_Restaurant_Index)

avg_val <- airbnb %>% group_by(City) %>% 
  summarize(`Median Price` = round(median(Price), digit = 0),
            `Prop Superhost` = round(mean(`Superhost`), digit = 3),
            `Median Bedrooms` = median(Bedrooms),
            `Median Person Capacity` = median(`Person_Capacity`),
            `Median Clean Rating` = median(`Cleanliness_Rating`),
            `Median Satisfaction` = median(`Guest_Satisfaction_Rating`),
            `Median City Center Dist` = round(median(`City_Center_Distance`), 
                                              digit = 3),
            `Median Metro Dist` = round(median(`Metro_Distance`),
                                        digit = 3),
            `Median Attraction Index` = round(median(`Attraction_Index`),
                                              digit = 3),
            `Median Restaurant Index` = round(median(`Restaurant_Index`), 
                                              digit = 3))

df_days <- airbnb %>% select(City, Day) %>% group_by(City) %>% count(Day) %>%
  mutate(`Prop Weekend` = round(n/sum(n), digit = 3)) %>% filter(Day == "Weekend") %>% 
  select(City, `Prop Weekend`)

df_rooms <- airbnb %>% select(City, Room_Type) %>% group_by(City) %>% count(Room_Type) %>%
  mutate(Prop = round(n/sum(n), digit = 3)) %>% select(City, Room_Type, Prop) %>%
  pivot_wider(names_from = Room_Type, values_from = Prop)

city_avg <-df_days %>% left_join(df_rooms, by = "City") %>% 
  left_join(avg_val, by = "City")


## Start Server

shinyServer(function(input, output, session){
  
  ## Page 1
  
  output$EuropeImage <- renderImage({
    return(list(src = "./Data/IntroEurope.png", contentType = "image/png", alt = "Alignment"))
  }, deleteFile = FALSE)
  
  output$introText <- renderUI({
    HTML("This is visualization of Airbnb in Europe. Selecting 9 cities in 
    Europe: Amsterdam, Athens, Barcelona, Berlin, Budapest, Lisbon, Paris, 
    Rome, and Vienna. This website should help in giving general aspects
    of the selected cities in Europe, such as price and different metrics of 
    cities and Airbnbs.
    <br>
    <br>
    Refer to my github page (https://github.com/DMHwang/DataViz) for code.
    <br>
    <br>
    Created By: Paul Hwang")
  })
  
  
  ## Page 2
  
  output$medExplain <- renderText({
    text <- "Here, we have a city-wide median for the following metrics of cities.
    Note that we are using median instead of mean due to outliers. To look into 
    more detail, go to the City tab."
  })
  
  updateSelectInput(session, "columnSel", choices = colnames(city_avg)[2:15])
  
  output$avgdata <- renderDT({
    datatable(
      city_avg %>% select(c("City", input$columnSel)),
      extensions = "FixedColumns",
      options = list(
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 2)
      )
    )
  })
  
  ## page 3 1d
  
  updateSelectInput(session, "citysel", choices = unique(airbnb$City))
  
  cityData <- reactive({
    df_city <- airbnb
  })
  
  updateSelectInput(session, "colSel", choices = colnames(airbnb_ams)[2:15])
  
  output$cityDescription <- renderText({
    text <- "This section shows individual traits of a single city, as well as 
    some simple visualization. Here, you will be able to observe general trends
    of a chosen city."
  })
  
  
  output$city1dPlot <- renderPlotly({
    vec_city1d <- cityData() %>% pull(input$colSel)
    final <- plot_ly()
    if(is.numeric(vec_city1d)){
      final <- plot_ly(data = cityData()  %>% filter(City == input$citysel), 
                       hoverinfo = "none") %>% 
        add_histogram(x = ~get(input$colSel))
    } else{
      temp_data <- cityData()  %>% filter(City == input$citysel) %>% 
        select(input$colSel) %>% count(get(input$colSel)) %>% 
        rename(chosen_factor = "get(input$colSel)")
      final <- plot_ly(data = temp_data, type = "bar",
                       x = ~chosen_factor, y = ~n)
    }
    final %>% layout(yaxis = list(title = "Count"),
                     xaxis = list(title = input$colSel))
  })
  
  ## page 4 price info
  
  updateSelectInput(session, "citysel2", choices = unique(airbnb$City))
  
  output$priceDescription <- renderUI({
    HTML("This section will show the behaviors of price based on selected 
    sets of variables. Manipulate the values here to find relationship of 
    price with other factors.
    <br>
    <br>
    One interesting find is that price is barely correlated with 
    factors not directly related to the room itself. Though price seemed to go 
    up as the room size and satisfaction rating went up, price seemed to 
    be weakly, if at all, correlated with distance to city center or 
    attraction ratings.")
  })
  
  updateSelectInput(session, "numColSel", 
                    choices = colnames(airbnb %>% select_if(is.numeric)),
                    selected = "Attraction_Index")
  
  updateSelectInput(session, "numColSel2", 
                    choices = colnames(airbnb %>% select_if(is.numeric)),
                    selected = "Restaurant_Index")
  
  
  output$price2d <- renderPlotly({
    plot_ly(data = cityData()  %>% filter(City == input$citysel2), 
            type = "scatter", mode = "markers",
            y = ~Price, x = ~get(input$numColSel),
            hovertemplate = paste("Price: %{y:.2f}<br>",
                                  input$numColSel, " : %{x:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>% 
      layout(xaxis = list(title = input$numColSel))
  })
  
  output$price2dens <- renderPlotly({
    plot_ly(data = cityData() %>% filter(City == input$citysel2), 
            type = "histogram2dcontour",
            x = ~get(input$numColSel),
            y = ~Price,
            hovertemplate = paste("Price: %{y:.2f}<br>",
                                  input$numColSel, " : %{x:.2f}<br>",
                                  "z: %{z:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>%
      layout(xaxis = list(title = input$numColSel),
             yaxis = list(title = "Price"))
  })
  
  output$price3d <- renderPlotly({
    plot_ly(data = cityData() %>% filter(City == input$citysel2),
            type = "scatter3d", mode = "markers",
            z = ~Price, x = ~get(input$numColSel), 
            y = ~get(input$numColSel2), opacity = 0.5,
            color = ~Price,
            marker = list(size = 2),
            hovertemplate = paste("Price: %{z:.2f}<br>",
                                  input$numColSel, " : %{x:.2f}<br>",
                                  input$numColSel2, ": %{y:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>%
      layout(scene = list(xaxis = list(title = input$numColSel),
                          yaxis = list(title = input$numColSel2)))
  })
  
  output$price3den <- renderPlotly({
    plot_ly(data = cityData() %>% filter(City == input$citysel2), 
            type = "histogram2dcontour",
            x = ~get(input$numColSel),
            y = ~get(input$numColSel2),
            hovertemplate = paste(input$numColSel2, " : %{y:.2f}<br>",
                                  input$numColSel, " : %{x:.2f}<br>",
                                  "z: %{z:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>%
      layout(xaxis = list(title = input$numColSel),
             yaxis = list(title = input$numColSel2))
  })
  
  ## page 5
  
  updateSelectInput(session, "citysel3", choices = unique(airbnb$City))
  
  output$attractDescription <- renderUI({
    HTML("This section will show the behaviors of attarction index based on selected 
         sets of variables. Manipulate the values here to find relationship of 
         price with other factors.
         <br>
         <br>
         Contrary to price, we can see that attraction index is strongly  
         correlated with a good number of factors. Any factors that are not price, 
         the number of bedrooms, or people capacity have some correlation with 
         attraction index. For example, closer the Airbnb is to city center, 
         the higher the attraction index is.")
  })
  
  updateSelectInput(session, "numColSel3", 
                    choices = colnames(airbnb %>% select_if(is.numeric)),
                    selected = "City_Center_Distance")
  
  updateSelectInput(session, "numColSel4", 
                    choices = colnames(airbnb %>% select_if(is.numeric)),
                    selected = "Metro_Distance")
  
  
  output$attract2d <- renderPlotly({
    plot_ly(data = cityData()  %>% filter(City == input$citysel3), 
            type = "scatter", mode = "markers",
            y = ~Attraction_Index, x = ~get(input$numColSel3),
            hovertemplate = paste("Attraction_Index: %{y:.2f}<br>",
                                  input$numColSel3, " : %{x:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>% 
      layout(xaxis = list(title = input$numColSel3))
  })
  
  output$attract2den <- renderPlotly({
    plot_ly(data = cityData() %>% filter(City == input$citysel3), 
            type = "histogram2dcontour",
            x = ~get(input$numColSel3),
            y = ~Attraction_Index,
            hovertemplate = paste("Attraction Index: %{y:.2f}<br>",
                                  input$numColSel3, " : %{x:.2f}<br>",
                                  "z: %{z:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>%
      layout(xaxis = list(title = input$numColSel3))
  })
  
  output$attract3d <- renderPlotly({
    plot_ly(data = cityData() %>% filter(City == input$citysel3),
            type = "scatter3d", mode = "markers",
            z = ~Attraction_Index, x = ~get(input$numColSel3), 
            y = ~get(input$numColSel4), opacity = 0.5,
            color = ~Attraction_Index,
            marker = list(size = 2),
            hovertemplate = paste("Attraction Index: %{z:.2f}<br>",
                                  input$numColSel3, " : %{x:.2f}<br>",
                                  input$numColSel4, " : %{y:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>%
      layout(scene = list(xaxis = list(title = input$numColSel3),
                          yaxis = list(title = input$numColSel4)))
  })
  
  output$attract3den <- renderPlotly({
    plot_ly(data = cityData() %>% filter(City == input$citysel3), 
            type = "histogram2dcontour",
            x = ~get(input$numColSel3),
            y = ~get(input$numColSel4),
            hovertemplate = paste(input$numColSel4, " : %{y:.2f}<br>",
                                  input$numColSel3, " : %{x:.2f}<br>",
                                  "z: %{z:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>%
      layout(xaxis = list(title = input$numColSel3),
             yaxis = list(title = input$numColSel4))
  })
  
  ## appendix
  
  output$df_full <- renderDT({
    datatable(
      cityData(),
      extensions = "FixedColumns",
      options = list(
        scrollX = TRUE
      )
    )
  })
  
  updateSelectInput(session, "numCol", 
                    choices = colnames(airbnb %>% select_if(is.numeric)))
  
  updateSelectInput(session, "numCol2", 
                    choices = colnames(airbnb %>% select_if(is.numeric)))
  
  updateSelectInput(session, "numCol3", 
                    choices = colnames(airbnb %>% select_if(is.numeric)))
  
  output$scatter2d <- renderPlotly({
    plot_ly(data = cityData(), 
            type = "scatter", mode = "markers",
            x = ~get(input$numCol), y = ~get(input$numCol2),
            color = ~City, opacity = 0.4, colors = "Set1",
            hovertemplate = paste(input$numCol, " : %{x:.2f}<br>",
                                  input$numCol2, ": %{y:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>% 
      layout(xaxis = list(title = input$numCol),
             yaxis = list(title = input$numCol2))
  })
  
  output$densityall <- renderPlotly({
    plot_ly(data = cityData(), 
            type = "histogram2dcontour",
            x = ~get(input$numCol),
            y = ~get(input$numCol2),
            hovertemplate = paste(input$numCol2, " : %{y:.2f}<br>",
                                  input$numCol, " : %{x:.2f}<br>",
                                  "z: %{z:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>%
      layout(xaxis = list(title = input$numCol),
             yaxis = list(title = input$numCol2))
  })
  
  output$scatter3d <- renderPlotly({
    plot_ly(data = cityData(),
            type = "scatter3d", mode = "markers",
            x = ~get(input$numCol), 
            y = ~get(input$numCol2),
            z = ~get(input$numCol3),
            opacity = 0.2,
            color = ~City,
            colors = "Set1",
            marker = list(size = 2),
            hovertemplate = paste(input$numCol3, " : %{z:.2f}<br>",
                                  input$numCol, " : %{x:.2f}<br>",
                                  input$numCol2, ": %{y:.2f}",
                                  "<extra></extra>"),
            hoverinfo = "hovertemplate") %>%
      layout(scene = list(xaxis = list(title = input$numCol),
                          yaxis = list(title = input$numCol2),
                          zaxis = list(title = input$numCol3)))
  })
  
  
  
  
  
  
})