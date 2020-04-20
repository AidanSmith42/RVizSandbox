

server <- function(input, output, session){

  observeEvent(input$bargraph,{
    if(input$package == 'ggplot'){
    code <- "ggplot(mpg) + geom_bar(aes(class))"
    g <-  ggplot(mpg, aes(class)) + geom_bar()
    if("dodge" %in% input$aesthetics){
      code <- "ggplot(mpg) + geom_bar(aes(class), position=position_dodge())"
      g <-  ggplot(mpg, aes(class)) + geom_bar(position=position_dodge())
    }
    
    if("fill" %in% input$aesthetics){
      code <- "ggplot(mpg) + geom_bar(aes(class, fill=drv"
      if("dodge" %in% input$aesthetics){
        code <- paste0(code, "), position=position_dodge())")
        g <- ggplot(mpg) + geom_bar(aes(class, fill=drv), position=position_dodge())
      }
      else{ code<- paste0(code, "))")
      g <- ggplot(mpg) + geom_bar(aes(class, fill=drv))
      }
    }
    if("flip" %in% input$aesthetics){
      if("fill" %in% input$aesthetics){
        code <- "ggplot(mpg) + geom_bar(aes(class, fill = drv), position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme(legend.position = 'top')"
        g <- ggplot(mpg) +
          geom_bar(aes(class, fill = drv), position = position_stack(reverse = TRUE)) +
          coord_flip() +
          theme(legend.position = "top")
      }
      else{
        g <- ggplot(mpg) +
          geom_bar(aes(class), position = position_stack(reverse = TRUE)) +
          coord_flip() +
          theme(legend.position = "top")
        code <- "ggplot(mpg) +
          geom_bar(aes(class), position = position_stack(reverse = TRUE)) +
          coord_flip() +
          theme(legend.position = 'top')"
      }
    }
    }
    else{
      code <- "plot_ly(mpg %>% group_by(class) %>% summarise(count = n()), x=~class, y=~count, type='bar')"
      p <- plot_ly(mpg %>% group_by(class) %>% summarise(count = n()), x=~class, y=~count, type='bar')
    }
    output$code <- renderText(code)
    if(input$package == 'ggplot'){
      
    output$plot2 <- renderPlotly(NULL)
    output$plot <- renderPlot(g)
    }
    else{ output$plot2 <- renderPlotly(p)
    output$plot <- renderPlot(NULL)
    }
    set.seed(12)
    output$Table <- renderDT(datatable(head(sample(mpg, size = 10)), options=list(dom='t')))
  })
  
  observeEvent(input$scatter,{
    if(input$package == 'ggplot'){
    code <- "ggplot(data=iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length))"
    g <- ggplot(data=iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length))
    
    if("color" %in% input$scatteraesthetics){
      code <- "ggplot(data=iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, color=Species))"
      g <- ggplot(data=iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, color=Species))
      if("shape" %in% input$scatteraesthetics){
        code <- "ggplot(data=iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, color=Species, shape = Species))"
        g <- ggplot(data=iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, color=Species,  shape=Species)) + theme_bw()
      }
    }
    else if("shape" %in% input$scatteraesthetics){
      code <- "ggplot(data=iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, shape = Species))"
      g <- ggplot(data=iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, shape=Species)) + theme_bw()
    }
    
    if("jitter" %in% input$scatteraesthetics){
      code <- "ggplot(data=iris) + geom_jitter(aes(x=Species, y=Sepal.Length))"
      g <- ggplot(data=iris) + geom_jitter(aes(x=Species, y=Sepal.Length)) + theme_bw()
      if("color" %in% input$scatteraesthetics){
        code <- "ggplot(data=iris) + geom_jitter(aes(x=Sepal.Width, y=Sepal.Length, color=Species))"
        g <- ggplot(data=iris) + geom_jitter(aes(x=Sepal.Width, y=Sepal.Length, color=Species))
        if("shape" %in% input$scatteraesthetics){
          code <- "ggplot(data=iris) + geom_jitter(aes(x=Sepal.Width, y=Sepal.Length, color=Species, shape = Species))"
          g <- ggplot(data=iris) + geom_jitter(aes(x=Sepal.Width, y=Sepal.Length, color=Species,  shape=Species)) + theme_bw()
        }
      }
    }
  }
  else{
    p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
    code <- " plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)"
  }
    
    if(input$package == 'ggplot'){
      
      output$plot2 <- renderPlotly(NULL)
      output$plot <- renderPlot(g)
    }
    else{ 
      output$plot2 <- renderPlotly(p)
      output$plot <- renderPlot(NULL)
    }
    output$code <- renderText(code)

    output$Table <- renderDT(datatable(head(iris), options=list(dom='t')))
  })
  
  observeEvent(input$boxPlot,{
    if(input$package == 'ggplot'){
    code <- "ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy))"
    g <- ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy))
    
    if("color" %in% input$boxaesthetics){
    g <- ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy), fill = "white", colour = "#3366FF")
    code <- 'ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy), fill = "white", colour = "#3366FF")'
    if("outlier color" %in% input$boxaesthetics){
      g <- ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy),fill = "white", colour = "#3366FF", outlier.colour = "red")
      code <- 'ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy),fill = "white", colour = "#3366FF", outlier.colour = "red")'
      if("jitter" %in% input$boxaesthetics){
        g <- ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy),fill = "white", colour = "#3366FF", outlier.colour = "red") + geom_jitter(aes(x=class, y=hwy))
        code <- 'ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy),fill = "white", colour = "#3366FF", outlier.colour = "red") + geom_jitter(aes(x=class, y=hwy))'
        }
    }
    else if("jitter" %in% input$boxaesthetics){
      g <- ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy),fill = "white", colour = "#3366FF") + geom_jitter(aes(x=class, y=hwy))
      code <- 'ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy),fill = "white", colour = "#3366FF") + geom_jitter(aes(x=class, y=hwy))'
    }

    }
    
    else if("outlier color" %in% input$boxaesthetics){
    g <- ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy), outlier.colour = "red")
      code <- 'ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy), outlier.colour = "red")'
    }
    else if("jitter" %in% input$boxaesthetics){
      g <- ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy)) + geom_jitter(aes(x=class, y=hwy))
      code <- 'ggplot(data=mpg) + geom_boxplot(aes(x=class, y=hwy)) + geom_jitter(aes(x=class, y=hwy))'
    }
    }
    else {
      p <- plot_ly(data=mpg, y=~hwy, color=~class, type="box")
      code <- "plot_ly(data=mpg, y=~hwy, color=~class, type='box')"
      
    }
    
    if(input$package == 'ggplot'){
      
      output$plot2 <- renderPlotly(NULL)
      output$plot <- renderPlot(g)
    }
    else{ 
      output$plot2 <- renderPlotly(p)
      output$plot <- renderPlot(NULL)
    }
    
    output$code <- renderText(code)
    output$Table <- renderDT(datatable(head(mpg), options=list(dom='t')))
    
  })
  
  observeEvent(input$linegraph,{
    if(input$package == 'ggplot'){
    g <- ggplot(economics, aes(date, unemploy)) + geom_line()
    code <- 'ggplot(economics, aes(date, unemploy)) + geom_line()'
    
    if("color" %in% input$lineaesthetics){
     g <-  ggplot(economics, aes(date, unemploy)) + geom_line(colour = "red")
     code <- 'ggplot(economics, aes(date, unemploy)) + geom_line(colour = "red")'
     if("linetype" %in% input$lineaesthetics){
       g <-  ggplot(economics, aes(date, unemploy)) + geom_line(colour = "red", linetype=2)
       code <-  'ggplot(economics, aes(date, unemploy)) + geom_line(colour = "red", linetype=2)'
     }
     if("ribbon"%in% input$lineaesthetics){
       g <-  ggplot(economics, aes(date, unemploy)) + geom_ribbon(aes(ymin=unemploy-sd(unemploy), ymax=unemploy+sd(unemploy)), fill="grey70") + geom_line(colour = "red")
       code <- 'ggplot(economics, aes(date, unemploy)) + geom_ribbon(aes(ymin=unemploy-sd(unemploy), ymax=unemploy+sd(unemploy)), fill="grey70") + geom_line(colour = "red")'
     }
    }
    else if("ribbon"%in% input$lineaesthetics){
        g <-  ggplot(economics, aes(date, unemploy)) + geom_ribbon(aes(ymin=unemploy-sd(unemploy), ymax=unemploy+sd(unemploy)), fill="grey70") + geom_line(colour = "red")
        code <- 'ggplot(economics, aes(date, unemploy)) + geom_ribbon(aes(ymin=unemploy-sd(unemploy), ymax=unemploy+sd(unemploy)), fill="grey70") + geom_line(colour = "red")'
      }
    }
    else {
      code <- "plot_ly(economics, x=~date, y=~unemploy, type='scatter', mode='lines')"
      p <- plot_ly(economics, x=~date, y=~unemploy, type='scatter', mode='lines')
      
    }
    
    if(input$package == 'ggplot'){
      
      output$plot2 <- renderPlotly(NULL)
      output$plot <- renderPlot(g)
    }
    else{ 
      output$plot2 <- renderPlotly(p)
      output$plot <- renderPlot(NULL)
    }
    output$code <- renderText(code)
    output$Table <- renderDT(datatable(head(economics), options=list(dom='t')))
  })
  
  value <- reactiveVal(0)
  
  observeEvent(input$cheatsheet,{
    
    if(as.numeric(value()) == 0){
     output$sheet <- renderUI(HTML('<object width="1200" height="850" data="data-visualization-2.1.pdf"></object>'))
     newValue <- value() - 1
     value(newValue)
    }
    else{
      output$sheet <- renderUI(NULL)
      newValue <- value() + 1
      value(newValue)
    }
  })
  
  output$imageHTML <-renderUI(HTML("<hr>
                     <p style='text-align:center;'>
                                           <span style='float:center;'><img src='logo.png' width='120px'></span>
                                           </p>
                                           "))

  observeEvent(input$package,{
    
    
    if(input$package == 'ggplot') output$imageHTML <- renderUI(HTML("<hr><p style='text-align:center;'>
    <span style='float:center;'><img src='logo.png' width='120px'></span>
                              </p>
                              "))
    else output$imageHTML <- renderUI(HTML("<hr><p style='text-align:center;'>
    <span style='float:center;'><img src='logo3.png' width='120px'></span>
                                           </p>
                                           "))
    
  })
  

  
}


