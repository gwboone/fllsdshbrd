# load the required packages
{library(shiny)
require(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyquant)
library(shinycssloaders)
library(readr)
library(data.table)
library(DT)
library(htmltools)
library(lubridate)
library(anytime)
library(priceR)
library(purrr)
library(rvest)
library(tidyr)
library(stringi)}

# setup & helpers
{example <- read_csv("buys.csv") # data for example table
rates <- read_csv("rates.csv")  # data for exchange rates


exchange.date <-  case_when(    # handling weekends for applying exchange rates
  lubridate::wday(Sys.Date())==1 ~ Sys.Date()-2,
  lubridate::wday(Sys.Date())==7 ~ Sys.Date()-1,
  lubridate::wday(Sys.Date())==2 ~ Sys.Date()-3,
  TRUE ~ Sys.Date()-1)

getFX("USD/CAD",                # downloading most recent USD to CAD FX
      from = exchange.date,
      to = exchange.date)

rateupdate <- as.data.table(USDCAD) # updated FX as a df  

rateupdate <- rateupdate %>% # appending most recent FX to historical FX 
  rename(date = index) %>%
  rename(CAD = USD.CAD) %>%
  mutate(date = format(date,"%m/%d/%Y"))

rates <- rbind(rates, rateupdate)
rates <- distinct(rates, date, .keep_all = T)
write_csv(rates, "rates.csv")
rates$date <- anydate(rates$date)


exchange <- as.data.frame(USDCAD) # the df that will be used for FX information

## strings for creating yahoo finance ticker profile urls
yfurl <- "https://finance.yahoo.com/quote/"
yfprfl <- "/profile?p="
}

#dataset <- diamonds

ui <- dashboardPage(title = paste0("E5CAnalytics ", format(Sys.Date(), format="%b-%d-%Y")),
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
  fluidRow(
    column(4, offset = 0,
           #h4("Portfolio Upload"),

           box(collapsible = T,
               collapsed = T,
               width = "40%",
               #height = "100",
               title = "Data Upload",
               solidHeader = T,
               status = "success",
               fileInput("csvFile","",placeholder = "upload data for analysis",
                     #width = '35%', 
                     multiple = F, accept = ".csv")

    )),
    
    hr(),
    column(11, offset = 0,
    fluidRow(
      tabBox(title = NULL, width = 9,
             tabPanel("Stock Performance", plotOutput("MyPlot") %>% withSpinner(color="#0dc5c1")),
             tabPanel("Portfolio Trend", plotOutput("PrtfTrend") %>% withSpinner(color="#0dc5c1")),
             #tabPanel("Performance Detail", dataTableOutput("PrtfDtl") %>% withSpinner(color="#0dc5c1")),
             tabPanel("Portfolio Diversity", plotOutput("PrtfPie") %>% withSpinner(color="#0dc5c1"))
             
             #box(width = 8,height = 450, plotOutput("MyPlot") %>% withSpinner(color="#0dc5c1")),
      ),   
      valueBoxOutput("Invested", width = 3),
      valueBoxOutput("MarketValue", width = 3),
      valueBoxOutput("ROI", width = 3),
      valueBoxOutput("Profit", width = 3)
      
    ),
    ),
    fluidRow(
      
    ),
  #plotOutput('plot'),
  
hr(),

column(11, offset = 0,
fluidRow(
  tabBox(title = NULL, width = 12,
         tabPanel("Stock Summary", dataTableOutput("table") %>% withSpinner(color="#0dc5c1")),       
         tabPanel("Performance Detail", dataTableOutput("PrtfDtl") %>% withSpinner(color="#0dc5c1"))
         #box(dataTableOutput("table"),width = 12, title = paste0("My ", format(exchange.date, "%B %d, %Y"), " Stock Summary")),
         #box(dataTableOutput("table2"),width = 12, title = paste0("My ", format(exchange.date, "%B %d, %Y"), " Stock Summary"))
  )
),
),
  


  ),#end rows
) #end dashboard body
)


server <- function(input, output, session) {
  buys <- eventReactive(input$csvFile, {
    data <- read.csv(input$csvFile$datapath,sep = ",", header = T)
    
    data <- data %>% 
      filter(Action == "BUY")# %>%
    #mutate(BuyPrice = if_else(Currency=="USD",BuyPrice*CADexchange[,1],BuyPrice))
    #mutate(BuyPrice = if_else(Currency=="USD",BuyPrice*exchange[,1],BuyPrice))
    
    #filter(BuyPrice > 15)
    data
  })
  
  
  
  contributions <- eventReactive(input$csvFile, {
    data <- read.csv(input$csvFile$datapath,sep = ",", header = T)
    
    data <- data %>% 
      filter(Action == "DEPOSIT")# %>%
    #filter(BuyPrice > 15)
    data
  })
  
  
  buydates <- eventReactive(input$csvFile,{
    #data <- read.csv(input$csvFile$datapath,sep = ",", header = T)      
    
    data <- buys() %>%
      filter(Action == "BUY") %>%
      mutate(Date2 = anytime::anydate(Date))
  })
  
  
  peformance.returns <- eventReactive(input$csvFile, {
    data <- buys() %>%
      group_by(Symbol, Date) %>%
      
      data   
    
  }) 
  
  tkrs <- eventReactive(input$csvFile, {
    data <- read.csv(input$csvFile$datapath,sep = ",", header = T)
    
    data <- data %>% distinct(Symbol)
    data
  })     
  
  
  returns_daily <- eventReactive(input$csvFile, {
    
    data <- tkrs() %>%
      
      tq_get(get = "stock.prices",
             from = Sys.Date()-7)
    data
  })         
  
  most_recent_returns <- eventReactive(input$csvFile, {
    
    data <- returns_daily() %>%
      group_by(Symbol) %>%
      filter(date == max(date)) %>% # selecting the most recent record from the 7 days of data returned
      select(Symbol, date, adjusted) %>%
      arrange(Symbol)# %>%
    #rename(Symbol = symbol)
    
    data
    
  })    
  
  MyPrtfl1 <- eventReactive(input$csvFile, {
    
    data <- merge(x=buys(), y=most_recent_returns(), by="Symbol")
    
    
    data
    
  }) 
  
  MyPrtfl2 <- eventReactive(input$csvFile, {
    
    data <- merge(x=MyPrtfl1(), y=rates, by="date")
    
    
    data
    
  }) 
  
  
  
  MyPrtfl <- eventReactive(input$csvFile, {
    
    data <- MyPrtfl2() %>%
      #mutate(CstBss = BuyPrice*Quantity)%>% # dollars spent to purchase stocks
      mutate(CstBss =  if_else(Currency == "USD", BuyPrice*Quantity*CAD, BuyPrice*Quantity))%>% # dollars spent to purchase stocks  
      #mutate(MktVal = (adjusted*Quantity)) %>% # use adjusted price for assessing market value
      mutate(MktVal = if_else(Currency=="USD",adjusted*Quantity*exchange[,1],adjusted*Quantity)) %>%
      mutate(MktRtn = (MktVal-CstBss)/CstBss*100) %>%
      # mutate(MktRtn = ((MktVal-(BuyPrice*Quantity))/(BuyPrice*Quantity)*100)) %>% # are we up or down from purchase price
      # mutate(BuyPrice = if_else(Currency=="USD",BuyPrice*exchange[,1],BuyPrice)) 
      mutate(PtflVal = sum(MktVal)) %>%# what is the portion of portfolio the stock makes
      mutate(adjusted = adjusted)
    
    
    data
    
  }) 
  
  
  MyDashBoard <- eventReactive(input$csvFile, {
    
    data <- MyPrtfl() %>%
      group_by(Symbol) %>%
      summarise(CstBasis = sum(CstBss),
                MktVal = sum(MktVal),
                Rtrn = (MktVal - CstBasis)/CstBasis*100,
                #Rtrn = mean(MktRtn),
      ) %>%
      mutate(PtflVal = sum(MktVal)) %>%
      mutate(Invstd = sum(CstBasis))%>%
      mutate(PrcntPtfl = MktVal/PtflVal*100) %>%
      mutate(PrcntPtfl = round(PrcntPtfl,digits=0)) %>%
      mutate(Rtrn = round(Rtrn, digits = 0)) %>%
      mutate(CstBasis = paste0("$",format(CstBasis,digits = 0,big.mark = ",", big.interval = 3))) %>%
      mutate(MktVal = paste0("$",format(MktVal,digits = 0,big.mark = ",", big.interval = 3)))
    
    
    data
    
  })     
  
  
  MyInvestment <- eventReactive(input$csvFile, {
    
    data <- sum(contributions()$BuyPrice)
    data
  }) 
  
  MyValue <- eventReactive(input$csvFile, {
    
    data <- as.numeric(MyDashBoard()[1,5])
    data
  }) 
  
  MyReturn <- eventReactive(input$csvFile, {
    
    data <- MyValue()-MyInvestment()
    data
  })    
  
  MyROI <- eventReactive(input$csvFile, {
    
    data <- (MyValue()-MyInvestment())/MyInvestment()*100
    data
  })
  
  MyPlotData <- eventReactive(input$csvFile, {
    
    data <- MyDashBoard() %>%
      mutate(FillColor = ifelse(Rtrn > 0, "olivedrab3", "tomato3"))
    data
  })
  
  
  tkrs.matrix <- eventReactive(input$csvFile, {
    
    data <- buys() %>%
      select(Symbol) %>%
      distinct() %>%
      filter(Symbol !="DEPOSIT") %>%
      as.matrix()
    data
  })
  
  
  urls <- eventReactive(input$csvFile, {
    
    data <- sprintf(paste0(yfurl ,tkrs.matrix(),yfprfl,tkrs.matrix()))
    data
  })
  
  
  sector <- eventReactive(input$csvFile, {
    
    data <- purrr::map_df(urls(), read_profile)
    data
  })
  
  
  sectors <- eventReactive(input$csvFile, {
    
    data <- bind_cols(sector(), tkrs.matrix())
    data
  })  
  
  
  
  prtf_pie <- eventReactive(input$csvFile, {
    
    data <- inner_join(sectors(), MyDashBoard(), by=c("...2" = "Symbol"))
    data
  })
  
  
  
  
  ## Function to read yahoo finance profile page
  read_profile <- function(url){
    az <- read_html(url)
    headline <- az %>%
      html_nodes("#Col1-0-Profile-Proxy .D\\(ib\\)") %>%
      html_text() %>%
      as_tibble() %>%
      unlist() %>%
      as_tibble() %>%
      slice(-1) %>%
      separate(col=value,
               into = c("Sector", "Industry"),
               sep = ":",
               extra = "drop") %>%
      select(Industry) %>%
      stri_sub(2,-9) %>%
      trimws() %>%
      as_tibble()
  }   
  
  
  
  output$MyPlot <- renderPlot({
    p <- ggplot(data=MyPlotData(),aes(x=Symbol, y=Rtrn, label = round(Rtrn)) )+
      geom_col(fill=MyPlotData()$FillColor,stat = 'identity')+
      theme_clean() +
      #theme(text = element_text(size=25),
      #      axis.text.x = element_text(angle = 90)) +
      scale_color_identity() +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      labs(x = "Ticker", y = "Percent Return") +
      ggtitle("Adjusted Returns",
              subtitle = paste0("As Of Close ",format(max(most_recent_returns()$date),"%B %d %Y")))+
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 10, angle = 90),
            axis.title = element_text(size = 15),
            axis.text.x.top = element_text(vjust = 0.5))
    
    print(p)
    
    
  })
  
  
  
  ###################  
  output$table <- renderDataTable({
    data <- MyDashBoard()  %>%
      select(c(-"PtflVal", -"Invstd"))  %>% DT::datatable(
        #autoHideNavigation = T,
        extensions = 'Buttons',
        rownames= F,
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          'Note: ', htmltools::em(paste0('Dashboard currency in Canadian Dollars. ',
                                         format(exchange.date, "%A %B %d, %Y"),
                                         " ",
                                         ' exchange rate of '),
                                  round(exchange,3),
                                  " has been applied to US exchange listings."
          )),
        options = list(searching = F,
                       pageLength = 5,
                       columnDefs = list(list(className = 'dt-right', targets = 1:4)),
                       dom = 'Bliftsp',
                       autoWidth = F,
                       buttons = c('csv'),
                       scrollX = T,
                       lengthMenu = list(c(-1, 5,10,20,50),
                                         c("All",5,10,20,50))
        ) 
      )
    
  })
  
  
  output$tableexample <- renderDataTable({
    data <- example %>% DT::datatable(
      #autoHideNavigation = T,
      extensions = 'Buttons',
      rownames= F,
      options = list(searching = F,
                     pageLength = 5,
                     #columnDefs = list(list(className = 'dt-right', targets = 2:4)),
                     dom = 'Bliftsp',
                     autoWidth = T,
                     buttons = c('csv'),
                     scrollX = F,
                     lengthMenu = list(c(-1, 3,5),
                                       c("All",3, 5))
      ) 
    )
    
    
    
  })
  
  
  
  output$Invested <- renderValueBox({
    valueBox(
      subtitle = "Invested",
      #tags$p("Invested", style = "font-size: 95%;"),
      value = tags$p(paste0("$", format(MyInvestment(),digits = 2, big.mark = ",", big.interval = 3)),style = "font-size: 95%;"),
      color = "light-blue",
      icon = icon("fas fa-donate")
    )
  })
  
  
  
  
  output$MarketValue <- renderValueBox({
    valueBox(
      #"Market Value",
      tags$p("Mkt. Value", style = "font-size: 95%;"),
      value = tags$p(paste0("$", format(MyValue(),digits = 2, big.mark = ",", big.interval = 3)),style = "font-size: 95%;"),
      #MyValue(),
      color = "light-blue", 
      icon = icon("fas fa-hand-holding-usd")
    )
  })
  
  
  
  output$ROI <- renderValueBox({
    valueBox(
      #"Market Return",
      tags$p("Mkt. Return", style = "font-size: 95%;"),
      value = tags$p(format(MyROI(),digits = 2),style = "font-size: 95%;"),
      #format(MyROI(),digits = 2),
      color = "light-blue",
      icon = icon("fas fa-percent")
    )
  })
  
  
  output$Profit <- renderValueBox({
    valueBox(
      "Gains",
      #tags$p("Gains", style = "font-size: 95%;"),
      #subtitle = tags$p("Gains", style = "font-size: 150%;"),
      value = tags$p(paste0("$", format(MyReturn(),digits = 2, big.mark = ",", big.interval = 3)),style = "font-size: 95%;"),
      #MyReturn(),
      color = "light-blue",
      icon = icon("fas fa-dollar-sign")
    )
  })
  
  ### Trend
  
  DailySum <- eventReactive(input$csvFile, {
    
    perf.data <- buys() %>%
      mutate(Date = anytime::anydate(Date)) %>%
      filter(Symbol != "DEPOSIT")%>%
      filter(Date != Sys.Date()) %>%
      select(Symbol,Date) %>%
      mutate(Date = anytime::anydate(Date)) %>%
      group_by(Symbol) %>%
      slice_min(Date, n=1)
    
    perf.data
    
    #
    stocks <- tibble()
    
    for (i in 1:nrow(perf.data))
    {
      tmpstocks <- tq_get(perf.data$Symbol[i], get = "stock.prices",
                          from = perf.data$Date[i])
      
      stocks <- rbind(stocks,tmpstocks)
      
    }
    
    stocks <- stocks %>% rename(Symbol = symbol)
    buys_trim <- buys() %>%
      filter(Action != "DEPOSIT") %>%
      select(Symbol, Quantity)
    
    
    
    data2 <- left_join(stocks, buys_trim, by="Symbol")
    
    data3 <- data2 %>%
      mutate(Value = adjusted*Quantity) %>%
      select(Symbol, date, Value)
    
    dailysum <- data3 %>%                    # Specify data frame
      group_by(date) %>%             # Specify group indicator
      summarise_at(vars(Value),              # Specify column
                   list(return = sum)) %>%
      mutate(date = anydate(date))
    dailysum
  })
  
  output$PrtfTrend <- renderPlot({
    p <- DailySum() %>% ggplot(aes(x=date,y=return, label = "Portfolio Value"))+
      
      geom_smooth(stat = "identity", color="olivedrab3", size=1.35, method = "loess") +
      labs(y = "Portfolio Value", x=NULL)+
      ggtitle("Portfolio Return Trend",
              subtitle = paste0("As Of Close ",format(Sys.Date(),"%B %d %Y")))+
      theme_clean() +
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 10, angle=45, hjust = 1),
            axis.title = element_text(size = 15),
            axis.text.x.top = element_text(vjust = 0.5))+
      scale_x_date(date_labels = "%b-%Y")
    
    
    print(p)
    
    
  })
  
  ## Perf. Detail Data.Frame
  
  PrtflDtl.stg1 <- eventReactive(input$csvFile, {
    
    data <- MyPrtfl() %>%
      select(Date, Symbol, Description, Quantity, Currency, BuyPrice,
             adjusted, CAD,  CstBss, MktVal, MktRtn) %>%
      rename(Dscpt. = Description,
             Qty = Quantity,
             Crncy = Currency,
             AdjCls = adjusted,
             BuyFX = CAD,
             BuyDate = Date) %>%
      mutate(BuyDate = anydate(BuyDate)) %>%
      
      mutate(BuyFX = format(BuyFX, digits = 4),
             BuyPrice = format_dollars(BuyPrice, 2),
             AdjCls = format_dollars(AdjCls, 2),
             CstBss = format_dollars(CstBss, 2),
             MktVal = format_dollars(MktVal, 2),
             '%Rtn' = format(MktRtn, digits = 2)) %>%
      mutate(BuyFX =  if_else(Crncy == "USD", BuyFX,NULL)) %>%
      select(-MktRtn)
    data
    
  })
  
  
  PrtflDtl.stg2 <- eventReactive(input$csvFile, {
    
    data <- inner_join(PrtflDtl.stg1(), rates, by=c("BuyDate" = "date")) %>%
      mutate(CAD = as.numeric(CAD)) %>%
      mutate(CAD = format(CAD, digits = 4)) %>%
      rename(LtsFX = CAD) %>%
      mutate(LtsFX = if_else(Crncy == "USD", LtsFX, NULL)) %>%
      select(BuyDate, Symbol, 'Dscpt.', Qty,Crncy,BuyPrice,BuyFX, LtsFX, CstBss, AdjCls, MktVal, '%Rtn') %>%
      arrange(Symbol, BuyDate)
    
    
    data
  })
  
  
  
  ####Pef. Detail table 
  output$PrtfDtl <- renderDataTable({
    data <- PrtflDtl.stg2() %>% DT::datatable(
      #autoHideNavigation = T,
      extensions = 'Buttons',
      rownames= F,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        htmltools::em("Show All entries to export full table of results as .csv")
      ),
      options = list(searching = F,
                     pageLength = 5,
                     columnDefs = list(list(className = 'dt-right', targets = 4:10)),
                     dom = 'Bliftsp',
                     autoWidth = F,
                     buttons = c('csv'),
                     scrollX = T,
                     lengthMenu = list(c(-1, 5,10,20,50),
                                       c("All",5,10,20,50))
      )) 
    
  })
  
  output$table2 <- renderDataTable({
    prtf_pie() %>%
      select(value, PrcntPtfl) %>%
      group_by(value) %>%
      summarise(PrtflPrtn = sum(PrcntPtfl)) %>%
      rename(Sector = value)
  })
  
  output$PrtfPie <- renderPlot({
    p <- prtf_pie() %>% 
      select(value, PrcntPtfl) %>%
      group_by(value) %>%
      summarise(PrtflPrtn = sum(PrcntPtfl)) %>%
      
      
      ggplot(aes(x="", y=PrtflPrtn, fill=value)) +
      geom_bar(aes(x="", y=PrtflPrtn, fill=value), stat="identity", width = 1)+
      coord_polar(theta = "y")+
      geom_text(aes(x=1.3,y = PrtflPrtn, label = paste0(round(PrtflPrtn,0),"%")), position = position_stack(vjust = 0.5), col = "white", size=5)+
      theme_void() +
      scale_fill_brewer(palette = "Dark2") 
    
    print(p)
  })  
  
  
}

shinyApp(ui, server)