# LICENCIA ----------------------------------------------------------------

 
 lic <- "
 
 The FreeType Project LICENSE
                    ----------------------------

                            2006-Jan-27

                    Copyright 1996-2002, 2006 by
          David Turner, Robert Wilhelm, and Werner Lemberg



Introduction
============

  The FreeType  Project is distributed in  several archive packages;
  some of them may contain, in addition to the FreeType font engine,
  various tools and  contributions which rely on, or  relate to, the
  FreeType Project.

  This  license applies  to all  files found  in such  packages, and
  which do not  fall under their own explicit  license.  The license
  affects  thus  the  FreeType   font  engine,  the  test  programs,
  documentation and makefiles, at the very least.

  This  license   was  inspired  by  the  BSD,   Artistic,  and  IJG
  (Independent JPEG  Group) licenses, which  all encourage inclusion
  and  use of  free  software in  commercial  and freeware  products
  alike.  As a consequence, its main points are that:

    o We don't promise that this software works. However, we will be
      interested in any kind of bug reports. (`as is' distribution)

    o You can  use this software for whatever you  want, in parts or
      full form, without having to pay us. (`royalty-free' usage)

    o You may not pretend that  you wrote this software.  If you use
      it, or  only parts of it,  in a program,  you must acknowledge
      somewhere  in  your  documentation  that  you  have  used  the
      FreeType code. (`credits')

  We  specifically  permit  and  encourage  the  inclusion  of  this
  software, with  or without modifications,  in commercial products.
  We  disclaim  all warranties  covering  The  FreeType Project  and
  assume no liability related to The FreeType Project.


  Finally,  many  people  asked  us  for  a  preferred  form  for  a
  credit/disclaimer to use in compliance with this license.  We thus
  encourage you to use the following text:

 Portions of this software are copyright © <year> The FreeType
 Project (www.freetype.org).  All rights reserved.

  Please replace <year> with the value from the FreeType version you
  actually use.


Legal Terms
===========

0. Definitions
--------------

  Throughout this license,  the terms `package', `FreeType Project',
  and  `FreeType  archive' refer  to  the  set  of files  originally
  distributed  by the  authors  (David Turner,  Robert Wilhelm,  and
  Werner Lemberg) as the `FreeType Project', be they named as alpha,
  beta or final release.

  `You' refers to  the licensee, or person using  the project, where
  `using' is a generic term including compiling the project's source
  code as  well as linking it  to form a  `program' or `executable'.
  This  program is  referred to  as  `a program  using the  FreeType
  engine'.

  This  license applies  to all  files distributed  in  the original
  FreeType  Project,   including  all  source   code,  binaries  and
  documentation,  unless  otherwise  stated   in  the  file  in  its
  original, unmodified form as  distributed in the original archive.
  If you are  unsure whether or not a particular  file is covered by
  this license, you must contact us to verify this.

  The FreeType  Project is copyright (C) 1996-2000  by David Turner,
  Robert Wilhelm, and Werner Lemberg.  All rights reserved except as
  specified below.

1. No Warranty
--------------

  THE FREETYPE PROJECT  IS PROVIDED `AS IS' WITHOUT  WARRANTY OF ANY
  KIND, EITHER  EXPRESS OR IMPLIED,  INCLUDING, BUT NOT  LIMITED TO,
  WARRANTIES  OF  MERCHANTABILITY   AND  FITNESS  FOR  A  PARTICULAR
  PURPOSE.  IN NO EVENT WILL ANY OF THE AUTHORS OR COPYRIGHT HOLDERS
  BE LIABLE  FOR ANY DAMAGES CAUSED  BY THE USE OR  THE INABILITY TO
  USE, OF THE FREETYPE PROJECT.

2. Redistribution
-----------------

  This  license  grants  a  worldwide, royalty-free,  perpetual  and
  irrevocable right  and license to use,  execute, perform, compile,
  display,  copy,   create  derivative  works   of,  distribute  and
  sublicense the  FreeType Project (in  both source and  object code
  forms)  and  derivative works  thereof  for  any  purpose; and  to
  authorize others  to exercise  some or all  of the  rights granted
  herein, subject to the following conditions:

    o Redistribution of  source code  must retain this  license file
      (`FTL.TXT') unaltered; any  additions, deletions or changes to
      the original  files must be clearly  indicated in accompanying
      documentation.   The  copyright   notices  of  the  unaltered,
      original  files must  be  preserved in  all  copies of  source
      files.

    o Redistribution in binary form must provide a  disclaimer  that
      states  that  the software is based in part of the work of the
      FreeType Team,  in  the  distribution  documentation.  We also
      encourage you to put an URL to the FreeType web page  in  your
      documentation, though this isn't mandatory.

  These conditions  apply to any  software derived from or  based on
  the FreeType Project,  not just the unmodified files.   If you use
  our work, you  must acknowledge us.  However, no  fee need be paid
  to us.

3. Advertising
--------------

  Neither the  FreeType authors and  contributors nor you  shall use
  the name of the  other for commercial, advertising, or promotional
  purposes without specific prior written permission.

  We suggest,  but do not require, that  you use one or  more of the
  following phrases to refer  to this software in your documentation
  or advertising  materials: `FreeType Project',  `FreeType Engine',
  `FreeType library', or `FreeType Distribution'.

  As  you have  not signed  this license,  you are  not  required to
  accept  it.   However,  as  the FreeType  Project  is  copyrighted
  material, only  this license, or  another one contracted  with the
  authors, grants you  the right to use, distribute,  and modify it.
  Therefore,  by  using,  distributing,  or modifying  the  FreeType
  Project, you indicate that you understand and accept all the terms
  of this license.

4. Contacts
-----------

  There are two mailing lists related to FreeType:

    o freetype@nongnu.org

      Discusses general use and applications of FreeType, as well as
      future and  wanted additions to the  library and distribution.
      If  you are looking  for support,  start in  this list  if you
      haven't found anything to help you in the documentation.

    o freetype-devel@nongnu.org

      Discusses bugs,  as well  as engine internals,  design issues,
      specific licenses, porting, etc.

  Our home page can be found at

    http://www.freetype.org

 "


# LIBRERIAS ---------------------------------------------------------------


library(shinydashboard)
library(plotly)

library(tidyquant)
library(PerformanceAnalytics)
library(TTR)

library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
library(scales)
library(DT)



 

# || FRONTEND || ----------------------------------------------------------------

 
 


# CABECERA ----------------------------------------------------------------


header <- dashboardHeader(title = span(tagList(icon("chart-line"), "Never Lose Money")),
                          
                          dropdownMenu(
                            type = "messages",
                            messageItem(
                              from = "Leonardo C",
                              message = "¡Bienvenido! Click  para más Info",
                              href = "https://www.linkedin.com/in/leo-cerv/"
                            ))
)



# SIDEBAR -----------------------------------------------------------------


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Forecast", tabName =  "for"),
    menuItem("Análisis", tabName =  "est"),
    menuItem("Indicadores", tabName =  "ind"),
    menuItem("Datos", tabName = "dat"))
  
  )



# BODY --------------------------------------------------------------------


body <- dashboardBody(
  
#### * Style ####  
  tags$head(
    tags$style( 
      HTML('
      h3 {
        font-family: Courier New;
      }
      ')
    )
  ),
  
  
#### * Forecast ####  
  tabItems(
    
    tabItem(tabName = "for",
            
            fluidRow(      
              box(
                title = "Activo 1",
                width = 6,
                textInput("act1", "Selección:"),
                actionButton("buscar", "Buscar")),
              
              box(
                title = "Activo 2",
                width = 6,
                textInput("act2", "Selección:"),
                actionButton("buscar2", "Buscar"))
              
              
            ),
            
            fluidRow(      
              box(
                title = "ACTIVO 1: Forecast a 30 dias",
                width = 12,
                plotlyOutput("fore1"))
            ),
            
            fluidRow(      
              box(
                title = "ACTIVO 2: Forecast a 30 dias",
                width = 12,
                plotlyOutput("fore2"))
            ),
            
            
    ),
    
    
    
    #### * Análisis #### 
    tabItem(tabName = "est",
            
            fluidRow(      
              tabBox(width = 12,
                     
                     tabPanel(title = "Promedio", 
                             
                              plotlyOutput("prom1")),
                     
                     tabPanel(title = "Desviación", 
                              
                              plotlyOutput("sd1")),
                     
                     tabPanel(title = "Boxplot", 
                              
                              plotlyOutput("box1")),
                     
                     tabPanel(title = "Retorno", 
                            
                              plotlyOutput("ret1"))
            )),
            
            
            fluidRow(      
              valueBoxOutput("val1"),
              valueBoxOutput("val2")
              ),
            
    ),
    
    
    
    
    
    #### * Indicadores #### 
    tabItem(tabName = "ind",
            
            fluidRow(      
              tabBox(width = 12,
                     
                     tabPanel(title = "RSI", 
                              sliderInput("obs", "Selecciona periodo:",
                                          min = 1, max = 90, value = 14
                              ),
                              plotlyOutput("rsi")),
                     
                     tabPanel(title = "MACD",
                              sliderInput("fast", "Fast:",
                                          min = 1, max = 90, value = 12
                              ),
                              sliderInput("slow", "Slow:",
                                          min = 1, max = 90, value = 26
                              ),
                              sliderInput("signal", "Signal:",
                                          min = 1, max = 90, value = 9
                              ),
                              plotlyOutput("mcd")),
                     
                     tabPanel(title = "Volumen",
                              plotlyOutput("vol")),
                     
                     
                     
                     tabPanel(title = "SMA",
                              
                              sliderInput("sma1", "SMA 1:",
                                          min = 1, max = 90, value = 9
                              ),
                              sliderInput("sma2", "SMA 2:",
                                          min = 1, max = 200, value = 50
                              ),
                              plotOutput("sma")),
                     
                     
                     
                     tabPanel(title = "EMA",
                              
                              sliderInput("ema1", "EMA 1:",
                                          min = 1, max = 90, value = 9
                              ),
                              sliderInput("ema2", "EMA 2:",
                                          min = 1, max = 200, value = 50
                              ),
                              
                              plotOutput("ema")),
                     
                     
                     tabPanel(title = "Candels",
                              plotlyOutput("candel"))
                     
              )
            ),
            
            fluidRow(      
              tabBox(width = 12,
                     
                     tabPanel(title = "RSI", 
                              sliderInput("obs2", "Selecciona periodo:",
                                          min = 1, max = 90, value = 14
                              ),
                              plotlyOutput("rsi2")),
                     
                     tabPanel(title = "MACD",
                              sliderInput("fast2", "Fast:",
                                          min = 1, max = 90, value = 12
                              ),
                              sliderInput("slow2", "Slow:",
                                          min = 1, max = 90, value = 26
                              ),
                              sliderInput("signal2", "Signal:",
                                          min = 1, max = 90, value = 9
                              ),
                              plotlyOutput("mcd2")),
                     
                     tabPanel(title = "Volumen",
                              plotlyOutput("vol2")),
                     
                     
                     
                     tabPanel(title = "SMA",
                              
                              sliderInput("sma1", "SMA 1:",
                                          min = 1, max = 90, value = 9
                              ),
                              sliderInput("sma2", "SMA 2:",
                                          min = 1, max = 200, value = 50
                              ),
                              plotOutput("sma2")),
                     
                     
                     
                     tabPanel(title = "EMA",
                              
                              sliderInput("ema1", "EMA 1:",
                                          min = 1, max = 90, value = 9
                              ),
                              sliderInput("ema2", "EMA 2:",
                                          min = 1, max = 200, value = 50
                              ),
                              
                              plotOutput("ema2")),
                     
                     
                     tabPanel(title = "Candels",
                              plotlyOutput("candel2"))
                     
                     
              ))
             ),
    
    
    #### * Datos ####
    tabItem(tabName = "dat",
            
            fluidRow(      
              box(
                width = 12,
              
                dataTableOutput("tabla1"))),
            
            fluidRow(      
              box(
                width = 12,
                
                dataTableOutput("tabla2")))
            
            )
))







# || UI || ----------------------------------------------------------------


ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin = "yellow",
                    title = "Hold")




# || BACKEND || -----------------------------------------------------------------


server <- function(input, output) {
  
  
# EXTRACCIÓN ---------------------------------------------------  
  
  
 
#### Primer activo ####
activo_react_1 <- eventReactive(input$buscar, {
    
    activo_1 <- getSymbols(input$act1, auto.assign = F,)
    activo_1 %>% tail(360)
    
  })



#### Segundo activo ####
activo_react_2 <- eventReactive(input$buscar2, {
  
  activo_2 <- getSymbols(input$act2, auto.assign = F)
  activo_2 %>% tail(360)
  
})





# MODELO PREDICTIVO -------------------------------------------------------


#### Primer activo ####


output$fore1 <-  renderPlotly({
    
 
 # 1 -. Limpieza de la data
  
  # transformación a dataframe
  ACTIVO_TS <- as.data.frame(activo_react_1())
  
  # index a columna y suprimir NA
  ACTIVO_TS <- ACTIVO_TS %>% 
    rownames_to_column(var = "Date") %>% 
    drop_na()
  
  # Formateo a tibble, columna fecha y nombre columna "Close"
  ACTIVO_TS <- as_tibble(ACTIVO_TS)
  ACTIVO_TS$Date <- as.Date(ACTIVO_TS$Date)
  colnames(ACTIVO_TS)[2] <- "Close"
    
    
    
    
  # 2 -. Modelo predictivos

  # Segmentación de la data testeo - validación
    splits <- time_series_split(
      ACTIVO_TS,
      assess     = "30 days",
      cumulative = TRUE
    )
    
    
    
    # Definición de los modelos
    
    # AUTO ARIMA
    model_arima <- arima_reg() %>%
      set_engine("auto_arima") %>%
      fit(Close ~ Date, training(splits))
    
    model_arima
    
    # Prophet 
    model_prophet <- prophet_reg(
      seasonality_yearly = TRUE
    ) %>%
      set_engine("prophet") %>%
      fit(Close ~ Date, training(splits))
    
    model_prophet
    
    # GLM
    model_glmnet <- linear_reg(penalty = 0.01) %>%
      set_engine("glmnet") %>%
      fit(
        Close ~ wday(Date, label = TRUE)
        + month(Date, label = TRUE)
        + as.numeric(Dates),
        training(splits)
      )
    
    model_glmnet
    
    
    
    # Despliegue de los resultados
    
    # Modeltime
    model_tbl <- modeltime_table(
      model_arima,
      model_prophet,
      model_glmnet.
    )
    
    # Calibrate
    calib_tbl <- model_tbl %>%
      modeltime_calibrate(testing(splits))
    
    
    
    # Forecast Future 
    future_forecast_tbl <- calib_tbl %>%
      modeltime_refit(ACTIVO_TS) %>%
      modeltime_forecast(
        h           = "30 days",
        actual_data = ACTIVO_TS
      )
    
    future_forecast_tbl %>%
      plot_modeltime_forecast(title = "")
    
  })
  
  
  


#### Segundo activo ####


output$fore2 <-  renderPlotly({
  
  
  # 1 -. Limpieza de la data
  
  # transformación a dataframe
  ACTIVO_TS <- as.data.frame(activo_react_2())
  
  # index a columna y suprimir NA
  ACTIVO_TS <- ACTIVO_TS %>% 
    rownames_to_column(var = "Date") %>% 
    drop_na()
  
  # Formateo a tibble, columna fecha y nombre columna "Close"
  ACTIVO_TS <- as_tibble(ACTIVO_TS)
  ACTIVO_TS$Date <- as.Date(ACTIVO_TS$Date)
  colnames(ACTIVO_TS)[2] <- "Close"
  
  
  # 2 -. Modelo predictivos
  
  # Segmentación de la data testeo - validación
  splits <- time_series_split(
    ACTIVO_TS,
    assess     = "30 days",
    cumulative = TRUE
  )
  
  
  # Definición de los modelos
  
  # AUTO ARIMA
  model_arima <- arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(Close ~ Date, training(splits))
  
  model_arima
  
  # Prophet 
  model_prophet <- prophet_reg(
    seasonality_yearly = TRUE
  ) %>%
    set_engine("prophet") %>%
    fit(Close ~ Date, training(splits))
  
  model_prophet
  
  # GLM
  model_glmnet <- linear_reg(penalty = 0.01) %>%
    set_engine("glmnet") %>%
    fit(
      Close ~ wday(Date, label = TRUE)
      + month(Date, label = TRUE)
      + as.numeric(Date),
      training(split)
    )
  
  model_glmnet
  
  
  # Despliegue de los resultados
  
  # Modeltime
  model_tbl <- modeltime_table(
    model_arima,
    model_prophet,
    model_glmnet,
  )
  
  # Calibrate
  calib_tbl <- model_tbl %>%
    modeltime_calibrate(testing(splits))
  
  
  
  # Forecast Future 
  future_forecast_tbl <- calib_tbl %>%
    modeltime_refit(TS) %>%
    modeltime_forecast(
      h           = "30 days",
      actual_data = ACTIVO_TS
    )
  
  future_forecast_tbl %>%
    plot_modeltime_forecast(.title = "")
  
  
})




# ANALISIS  ----------------------------------------------------------------


#### * Prom 1 #####

output$prom1 <- renderPlotly({
  

  
  BTC_R <- activo_react_1()
  BTC_R <- CalculateReturns(BTC) %>% na.omit()
  BTC_TS <- as.data.frame(BTC_R)
  
  
  BTC_TS <- BTC_TS %>% 
    rownames_to_column(var = "Date")
  
  BTC_TS <- as_tibble(BTC_TS)
  BTC_TS$Date <- as.Date(BTC_TS$Date)
  colnames(BTC_TS)[dim(BTC_TS)[2]] <- "Activo1"
  
  
  BTC_CLEAN <- BTC_TS %>% 
    select(Activo1) %>% 
    tail(360) %>% 
    drop_na() %>% 
    pivot_longer(cols = "Activo1",
                 names_to = "Activo",
                 values_to = "Precio") %>% 
    mutate(Precio = Precio * 100)
  
  

  

  BTC_ADA_R <- activo_react_2()  
  BTC_ADA_R <- CalculateReturns(BTC_ADA_R) %>% na.omit()  
  ADA_TS <- as.data.frame(BTC_ADA_R)
  
  ADA_TS <- ADA_TS %>% 
    rownames_to_column(var = "Date")
  
  ADA_TS <- as_tibble(ADA_TS)
  ADA_TS$Date <- as.Date(ADA_TS$Date)
  colnames(ADA_TS)[dim(ADA_TS)[2]] <- "Activo2" #TIKER
  
  
  ADA_CLEAN <- ADA_TS %>% 
    select(Activo2) %>% 
    tail(360) %>% 
    drop_na() %>% 
    pivot_longer(cols = "Activo2",
                 names_to = "Activo",
                 values_to = "Precio") %>% 
    mutate(Precio = Precio * 100)
  
  
  BD_ANALISIS <- rbind(ADA_CLEAN, BTC_CLEAN)
  BD_ANALISIS <-  BD_ANALISIS %>% as_tibble()
  
  
  prom_pty <- BD_ANALISIS %>% 
    group_by(Activo) %>% 
    summarise(Promedio = mean(Precio)) %>% 
    
    ggplot()+
    aes(Activo, Promedio) + 
    geom_point(size = 4, shape = 18, color = "orange") %>% 
    facet_grid(~ Activo, scales = "free_y") %>% 
    ylab(NULL) %>% 
    xlab("Retorno diario")
  
  ggplotly(prom_pty)
  
  
})




#### * SD 1 #####

output$sd1 <- renderPlotly({
  
  
  BTC_R <- activo_react_1()
  BTC_R <- CalculateReturns(BTC) %>% na.omit()
  BTC_TS <- as.data.frame(BTC_R)
  
  
  BTC_TS <- BTC_TS %>% 
    rownames_to_column(var = "Date")
  
  BTC_TS <- as_tibble(BTC_TS)
  BTC_TS$Date <- as.Date(BTC_TS$Date)
  colnames(BTC_TS)[dim(BTC_TS)[2]] <- "Activo1"
  
  
  BTC_CLEAN <- BTC_TS %>% 
    select(Activo1) %>% 
    tail(360) %>% 
    drop_na() %>% 
    pivot_longer(cols = "Activo1",
                 names_to = "Activo",
                 values_to = "Precio") %>% 
    mutate(Precio = Precio * 100)
  
  
  BTC_ADA_R <- activo_react_2()  
  BTC_ADA_R <- CalculateReturns(BTC_ADA_R) %>% na.omit()  
  ADA_TS <- as.data.frame(BTC_ADA_R)
  
  ADA_TS <- ADA_TS %>% 
    rownames_to_column(var = "Date")
  
  ADA_TS <- as_tibble(ADA_TS)
  ADA_TS$Date <- as.Date(ADA_TS$Date)
  colnames(ADA_TS)[dim(ADA_TS)[2]] <- "Activo2" #TIKER
  
  
  ADA_CLEAN <- ADA_TS %>% 
    select(Activo2) %>% 
    tail(360) %>% 
    drop_na() %>% 
    pivot_longer(cols = "Activo2",
                 names_to = "Activo",
                 values_to = "Precio") %>% 
    mutate(Precio = Precio * 100)
  
  
  BD_ANALISIS <- rbind(ADA_CLEAN, BTC_CLEAN)
  BD_ANALISIS <-  BD_ANALISIS %>% as_tibble()
  
  
  sd_pty <- BD_ANALISIS %>% 
    group_by(Activo) %>% 
    summarise(Promedio = sd(Precio)) %>% 
    
    ggplot()+
    aes(Activo, Promedio) + 
    geom_point(size = 4, shape = 18, color = "orange") +
    facet_grid(~ Activo, scales = "free_y")+
    ylab(NULL)+
    xlab("Desviación diaria")
  
  ggplotly(sd_pty)
  
  
})




#### * Boxplot 1 #####

output$box1 <- renderPlotly({
  
  
  
  BTC_R <- activo_react_1()
  BTC_R <- CalculateReturns(BTC) %>% na.omit()
  BTC_TS <- as.data.frame(BTC_R)
  
  
  BTC_TS <- BTC_TS %>% 
    rownames_to_column(var = "Date")
  
  BTC_TS <- as_tibble(BTC_TS)
  BTC_TS$Date <- as.Date(BTC_TS$Date)
  colnames(BTC_TS)[dim(BTC_TS)[2]] <- "Activo1"
  
  
  BTC_CLEAN <- BTC_TS %>% 
    select(Activo1) %>% 
    tail(360) %>% 
    drop_na() %>% 
    pivot_longer(cols = "Activo1",
                 names_to = "Activo",
                 values_to = "Precio") %>% 
    mutate(Precio = Precio * 100)
  
  
  BTC_ADA_R <- activo_react_2()  
  BTC_ADA_R <- CalculateReturns(BTC_ADA_R) %>% na.omit()  
  ADA_TS <- as.data.frame(BTC_ADA_R)
  
  ADA_TS <- ADA_TS %>% 
    rownames_to_column(var = "Date")
  
  ADA_TS <- as_tibble(ADA_TS)
  ADA_TS$Date <- as.Date(ADA_TS$Date)
  colnames(ADA_TS)[dim(ADA_TS)[2]] <- "Activo2" #TIKER
  
  
  ADA_CLEAN <- ADA_TS %>% 
    select(Activo2) %>% 
    tail(360) %>% 
    drop_na() %>% 
    pivot_longer(cols = "Activo2",
                 names_to = "Activo",
                 values_to = "Precio") %>% 
    mutate(Precio = Precio * 100)
  
  
  BD_ANALISIS <- rbind(ADA_CLEAN, BTC_CLEAN)
  BD_ANALISIS <-  BD_ANALISIS %>% as_tibble()
  
  box_pty <- BD_ANALISIS %>% 
    ggplot()+
    aes(Precio)+
    geom_boxplot(fill = "Orange")+
    facet_grid(~ Activo, scales = "free_y")+
    xlab(NULL)
  
  ggplotly(box_pty )
  
})




#### * Return 1 #####

output$ret1 <- renderPlotly({
  
  BTC_R <- activo_react_1()
  BTC_R <- CalculateReturns(BTC) %>% na.omit()
  BTC_TS <- as.data.frame(BTC_R)
  
  BTC_TS <- BTC_TS %>% 
    rownames_to_column(var = "Date")
  
  BTC_TS <- as_tibble(BTC_TS)
  BTC_TS$Date <- as.Date(BTC_TS$Date)
  colnames(BTC_TS)[dim(BTC_TS)[2]] <- "Activo1"
  
  
  BTC_CLEAN <- BTC_TS %>% 
    select(Activo1) %>% 
    tail(360) %>% 
    drop_na() %>% 
    pivot_longer(cols = "Activo1",
                 names_to = "Activo",
                 values_to = "Precio") %>% 
    mutate(Precio = Precio * 100)
  
  
  BTC_ADA_R <- activo_react_2()  
  BTC_ADA_R <- CalculateReturns(BTC_ADA_R) %>% na.omit()  
  ADA_TS <- as.data.frame(BTC_ADA_R)
  
  ADA_TS <- ADA_TS %>% 
    rownames_to_column(var = "Date")
  
  ADA_TS <- as_tibble(ADA_TS)
  ADA_TS$Date <- as.Date(ADA_TS$Date)
  colnames(ADA_TS)[dim(ADA_TS)[2]] <- "Activo2" #TIKER

  
  ADA_CLEAN <- ADA_TS %>% 
    select(Activo2) %>% 
    tail(360) %>% 
    drop_na() %>% 
    pivot_longer(cols = "Activo2",
                 names_to = "Activo",
                 values_to = "Precio") %>% 
    mutate(Precio = Precio * 100)
  
  
  BD_ANALISIS <- rbind(ADA_CLEAN, BTC_CLEAN)
  BD_ANALISIS <-  BD_ANALISIS %>% as_tibble()
  
  
  hist_pty <- BD_ANALISIS %>% 
    ggplot()+
    aes(Precio, color = Activo)+
    geom_density()+
    xlab("Ret")+
    theme_classic()
  
  ggplotly(hist_pty)
  
  
})





# TENDENCIA 1 -------------------------------------------------------------

  
#### Rsi 1 ####
output$rsi <- renderPlotly({
    
    # 1 -. Limpieza de la data
    
    # transformación a dataframe
    ACTIVO_TS <- as.data.frame(activo_react_1())
    
    # index a columna y suprimir NA
    ACTIVO_TS <- ACTIVO_TS %>% 
      rownames_to_column(var = "Date") %>% 
      drop_na()
    
    # Formateo a tibble, columna fecha y nombre columna "Close"
    ACTIVO_TS <- as_tibble(ACTIVO_TS)
    ACTIVO_TS$Date <- as.Date(ACTIVO_TS$Date)
    colnames(ACTIVO_TS)[2] <- "Close"
    
    precio <- ACTIVO_TS$Close
    
    rsi <- RSI(precio, n = input$obs)
    
    rsi_plot <- cbind(ACTIVO_TS$Date, rsi)
    rsi_plot <- as.data.frame(rsi_plot)
    rsi_plot$V1 <- as.Date(rsi_plot$V1)
    rsi_plot <- as_tibble(rsi_plot)
    rsi_plot <- rsi_plot %>% drop_na()
    
    pty_rsi <- rsi_plot %>% 
      ggplot()+
      aes(V1, rsi) +
      geom_line(color = "#f2a900") +
      geom_hline(yintercept = 20, color = "red")+
      geom_hline(yintercept = 80, color = "red")+
      geom_hline(yintercept = 50, color = "black", lty = 2)+
      theme_light()
    
    ggplotly(pty_rsi)
    
  })
  
  

#### Macd 1 ####  
output$mcd <- renderPlotly({
    
    ACT_M <- activo_react_1() 
    ACT_M <- as.data.frame(ACT_M)
    
    ACT_M <- ACT_M %>% 
      rownames_to_column(var = "Date") %>% 
      drop_na()
    
    ACT_M <- as_tibble(ACT_M)
    ACT_M$Date <- as.Date(ACT_M$Date)
    
    colnames(ACT_M)[2] <- "Close"
    precio <- ACT_M$Close
    
    
    macdi  <- MACD(precio , nFast = input$fast, nSlow = input$slow, nSig = input$signal, maType="EMA")
    macdi <- as_tibble(macdi)
    macdi <- macdi %>% drop_na()
    hasta <- dim(macdi)[1]
    
    macds <- macdi %>% mutate(id = seq(1, hasta) )
    
    
    pty_macd <- macds %>% 
      ggplot()+
      aes(id, macd)+
      geom_line(color = "#f2a900")+
      geom_line(aes(y = signal), color = "red")+
      geom_hline(yintercept = 0, color = "black", lty = 2)+
      theme_light()
    
    ggplotly(pty_macd)
    
  })
  
  
  
  
#### Vol 1 ####  
output$vol <- renderPlotly({
    
    ACT_vol <- activo_react_1() 
    ACT_vol <- as.data.frame(ACT_vol)
    
    ACT_vol <- ACT_vol %>% 
      rownames_to_column(var = "Date")
    
    ACT_vol <- ACT_vol[ , c("Date", paste0(input$act1,".Volume"))]
    ACT_vol <- as_tibble(ACT_vol)
    ACT_vol$Date <- as.Date(ACT_vol$Date)
    colnames(ACT_vol)[2] <- "Volumen"
    
    
    pty_vol <- ACT_vol %>% 
      ggplot()+
      aes(Date, Volumen)+
      geom_bar(stat = "identity", fill = "#f2a900", color = "#f2a900")+
      geom_hline(yintercept = mean(ACT_vol$Volumen, na.rm = T), lty = 2)+
      theme_light()+
      ylab(NULL)+
      xlab(NULL)
    
    ggplotly(pty_vol)
    
    
  })
  
  
#### Sma 1####  
output$sma <- renderPlot({
    
    ACT_m <- as.data.frame(activo_react_1())
  
    ACT_m <- ACT_m %>% 
      rownames_to_column(var = "Date")
    
    ACT_m <- as_tibble(ACT_m)
    ACT_m$Date <- as.Date(ACT_m$Date)
    colnames(ACT_m) <- c("Date", "open", "high", "low", "close", "volumen", "adjusted")
    
    ACT_m %>%
      ggplot(aes(x = Date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close),
                       colour_up = "darkgreen", colour_down = "darkred", 
                       fill_up  = "darkgreen", fill_down  = "darkred")+
      
      geom_ma(ma_fun = SMA, n = input$sma1, linetype = 5, size = 1.25) +
      geom_ma(ma_fun = SMA, n = input$sma2, color = "red", size = 1.25) + 
      theme_tq()
    
  })
  
  
  
#### Ema 2 ####  
output$ema <- renderPlot({
    
    ACT_m <- as.data.frame(activo_react_1())
  
    ACT_m <- ACT_m %>% 
      rownames_to_column(var = "Date")
    
    ACT_m <- as_tibble(ACT_m)
    ACT_m$Date <- as.Date(ACT_m$Date)
    
    colnames(ACT_m) <- c("Date", "open", "high", "low", "close", "volumen", "adjusted")
    
    ACT_m %>%
      ggplot(aes(x = Date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close),
                       colour_up = "darkgreen", colour_down = "darkred", 
                       fill_up  = "darkgreen", fill_down  = "darkred")+
      
      geom_ma(ma_fun = EMA, n = input$ema1, linetype = 5, size = 1.25) +
      geom_ma(ma_fun = EMA, n = input$ema2, color = "red", size = 1.25) + 
      theme_tq()
    
    
  })
  
  


#### Cand 1 #### 
  output$candel <- renderPlotly({
    
    ACT_c <- as.data.frame(activo_react_1())
    
    ACT_c <- ACT_c %>% 
      rownames_to_column(var = "Date")
    
    ACT_c <- as_tibble(ACT_c)
    ACT_c$Date <- as.Date(ACT_c$Date)
    
    colnames(ACT_c) <- c("Date", "open", "high", "low", "close", "volumen", "adjusted")
    
    i <- list(line = list(color = '#f2a900'))
    d <- list(line = list(color = '#4d4d4e'))
    
    ACT_c %>% tail(60) %>% plot_ly(x = ~Date, type="candlestick",
                                   open = ~open, close = ~close,
                                   high = ~high, low = ~low,
                                   increasing = i, decreasing = d)
    
  })
  
  
  
 

# TENDENCIA 2 -------------------------------------------------------------

  
  
  
#### Rsi 2 #### 
output$rsi2 <- renderPlotly({
  
    ACTIVO_TS2 <- as.data.frame(activo_react_2())
    
    ACTIVO_TS2 <- ACTIVO_TS2 %>% 
      rownames_to_column(var = "Date") %>% 
      drop_na()
    
    # Formateo a tibble, columna fecha y nombre columna "Close"
    ACTIVO_TS2 <- as_tibble(ACTIVO_TS2)
    ACTIVO_TS2$Date <- as.Date(ACTIVO_TS2$Date)
    colnames(ACTIVO_TS2)[2] <- "Close"
    
    precio <- ACTIVO_TS2$Close
    
    rsi <- RSI(precio, n = input$obs2)
  
    rsi_plot <- cbind(ACTIVO_TS2$Date, rsi)
    rsi_plot <- as.data.frame(rsi_plot)
    rsi_plot$V1 <- as.Date(rsi_plot$V1)
    rsi_plot <- as_tibble(rsi_plot)
    rsi_plot <- rsi_plot %>% drop_na()
    
    #plot
    pty_rsi <- rsi_plot %>% 
      ggplot()+
      aes(V1, rsi) +
      geom_line(color = "#f2a900") +
      geom_hline(yintercept = 20, color = "red")+
      geom_hline(yintercept = 80, color = "red")+
      geom_hline(yintercept = 50, color = "black", lty = 2)+
      theme_light()
    
    ggplotly(pty_rsi)
    
  })
  
  
  
  
  
  
#### Macd 2 ####   
output$mcd2 <- renderPlotly({
    
    ATC_M2 <- activo_react_2()
    ATC_M2 <- as.data.frame(ATC_M2)
    
    ATC_M2 <- ATC_M2 %>% 
      rownames_to_column(var = "Date") %>% 
      drop_na()
    
    ATC_M2 <- as_tibble(ATC_M2)
    ATC_M2$Date <- as.Date(ATC_M2$Date)
    colnames(ATC_M2)[2] <- "Close"
    precio <- ATC_M2$Close
    
    macdi  <- MACD(precio , nFast = input$fast, nSlow = input$slow, nSig = input$signal, maType="EMA" )
    macdi <- as_tibble(macdi)
    macdi <- macdi %>% drop_na()
    hasta <- dim(macdi)[1]
    macds <- macdi %>% mutate(id = seq(1, hasta) )
    
    pty_macd <- macds %>% 
      ggplot()+
      aes(id, macd)+
      geom_line(color = "#f2a900")+
      geom_line(aes(y = signal), color = "red")+
      geom_hline(yintercept = 0, color = "black", lty = 2)+
      theme_light()
    
    ggplotly(pty_macd)
    
  })
  
  
  
  
  
#### Vol 2 ####    
output$vol2 <- renderPlotly({
    
    ATC_vol2 <- activo_react_2() 
    ATC_vol2 <- as.data.frame(ATC_vol2)
    
    ATC_vol2 <- ATC_vol2 %>% 
      rownames_to_column(var = "Date") %>% drop_na()
    
    ATC_vol2 <- ATC_vol2[ ,c("Date", paste0(input$act2,".Volume"))]
    
    ATC_vol2 <- as_tibble(ATC_vol2)
    ATC_vol2$Date <- as.Date(ATC_vol2$Date)
    
    colnames(ATC_vol2)[2] <- "Volumen"
    
    
    pty_vol <- ATC_vol2 %>% 
      ggplot()+
      aes(Date, Volumen)+
      geom_bar(stat = "identity", fill = "#f2a900", color = "#f2a900")+
      geom_hline(yintercept = mean(ATC_vol2$Volumen, na.rm = T), lty = 2)+
      theme_light()+
      ylab(NULL)+
      xlab(NULL)
    
    ggplotly(pty_vol)
    
  })
  
  


#### Sma 2 ####    
output$sma2 <- renderPlot({
    
    ATC_m2 <- as.data.frame(activo_react_2())
  
    ATC_m2 <- ATC_m2 %>% 
      rownames_to_column(var = "Date")
    
    ATC_m2 <- as_tibble(ATC_m2)
    ATC_m2$Date <- as.Date(ATC_m2$Date)
    
    colnames(ATC_m2) <- c("Date", "open", "high", "low", "close", "volumen", "adjusted")
    
    ATC_m2 %>%
      ggplot(aes(x = Date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close),
                       colour_up = "darkgreen", colour_down = "darkred", 
                       fill_up  = "darkgreen", fill_down  = "darkred")+
      
      geom_ma(ma_fun = SMA, n = input$sma1, linetype = 5, size = 1.25) +
      geom_ma(ma_fun = SMA, n = input$sma2, color = "red", size = 1.25) + 
      theme_tq()
    
  })
  
  
  


#### Ema 2 ####    
output$ema2 <- renderPlot({
    
    BTC_m <- as.data.frame(activo_react_2())
    BTC_m <- BTC_m %>% 
      rownames_to_column(var = "Date")
    BTC_m <- as_tibble(BTC_m)
    BTC_m$Date <- as.Date(BTC_m$Date)
    
    colnames(BTC_m) <- c("Date", "open", "high", "low", "close", "volumen", "adjusted")
    
    BTC_m %>%
      ggplot(aes(x = Date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close),
                       colour_up = "darkgreen", colour_down = "darkred", 
                       fill_up  = "darkgreen", fill_down  = "darkred")+
      
      geom_ma(ma_fun = EMA, n = input$ema1, linetype = 5, size = 1.25) +
      geom_ma(ma_fun = EMA, n = input$ema2, color = "red", size = 1.25) + 
      theme_tq()
    
  })
  
  
  
#### Cand 2 ####  
output$candel2 <- renderPlotly({
    
    ACT_c2 <- as.data.frame(activo_react_2())
    
    ACT_c2 <- ACT_c2 %>% 
      rownames_to_column(var = "Date")
    
    ACT_c2 <- as_tibble(ACT_c2)
    ACT_c2$Date <- as.Date(ACT_c2$Date)
    colnames(ACT_c2) <- c("Date", "open", "high", "low", "close", "volumen", "adjusted")
    
    i <- list(line = list(color = '#f2a900'))
    d <- list(line = list(color = '#4d4d4e'))
    
    ACT_c2 %>% tail(60) %>% plot_ly(x = ~Date, type="candlestick",
                                   open = ~open, close = ~close,
                                   high = ~high, low = ~low,
                                   increasing = i, decreasing = d)
    
  })
  

# DATOS  -------------------------------------------------------------------


output$tabla1 <- renderDataTable({
  
  activo_react_1() %>%
    datatable(rownames = F, extensions = "Buttons",
              options = list(pageLength = 20,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv")))
  
})



output$tabla2 <- renderDataTable({
  
  activo_react_2() %>%
    datatable(rownames = F, extensions = "Buttons",
              options = list(pageLength = 20,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv")))
})



# VALUEBOX ----------------------------------------------------------------


output$val1 <- renderValueBox({
  
  Dat <- as_tibble(activo_react_1())
  colnames(Dat)[dim(Dat)[2]] <- "Close"
  
  val <- (tail(Dat$Close,1) - head(Dat$Close,1)) / head(Dat$Close,1)
  val <- val * 100
  
  valueBox(round(val,2),
           "Retorno Anual A1",
           color = "yellow",
           icon = icon("percent"))
  
})



output$val2 <- renderValueBox({
  
  Dat <- as_tibble(activo_react_2())
  colnames(Dat)[dim(Dat)[2]] <- "Close"
  
  val <- (tail(Dat$Close,1) - head(Dat$Close,1)) / head(Dat$Close,1)
  val <- val * 100
  
  
  valueBox(round(val,2),
           "Retorno Anual A2",
           color = "yellow",
           icon = icon("percent"))
  
})

}




shinyApp(ui, server)
