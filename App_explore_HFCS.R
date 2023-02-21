#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
library(plotly)
load("data_BE_shiny.RData")
colnames(data) <-  c("Country", "TotAssets", "OwnHouse", "OtherHouse", "vehicles", "SelfEmplBus","BusWealth", 
                  "RealEstate", "FinancialAssets_noPensions", "Deposits", "MutualFunds", "Bonds", "Bus_noSelfEmpl", "Shares", "ManagedAcc", "MoneyOwed_toHH", "OtherAssets", "Pension_nonPublic", "TotAssets_noPens", "IncomeWage",
                  "IncomeSelfEmpl", "IncomeRent", "IncomeFinancial", "IncomePension", "IncomePubTransf", "IncomePrivTransf", "IncomeOther", "IncomeFriends", "IncomeMinusInterest", "IncomeTot_equivalized_HHsize", "NetWealth_noPens", "NetFinancialPosition", "NetLiquidAsset",
                  "IncomeTotal", "Income_WagePensTrans", "InteresPaid", "IncomeWPT_noInt", "rent", "mortgage", "WPT_disp", "Income_disp")
data <- data %>% mutate(TotAssets = rowSums(across(c("FinancialAssets_noPensions", "RealEstate", "Pension_nonPublic"))),
                        TotAssets_noPens = rowSums(across(c("FinancialAssets_noPensions", "RealEstate"))) )

share_b <- function(data, ntile, type) {
  total <- sum(data[,type], na.rm=TRUE)
  data <- data %>% group_by(.data[[ntile]]) %>% mutate(pshare =  sum(.data[[type]], na.rm=TRUE)/total,
                                                       psum= sum(.data[[type]], na.rm=TRUE),
                                                       pmean = mean(.data[[type]], na.rm=TRUE))
  
  return(data)
}
deciles_maker <- function(data, vars, n) {
  for(i in vars) {
    data <- data %>% mutate(x = ntile(.data[[i]], n = n) )
    data <- rename(data, !!paste0(i, "_", n, "_tile") := x)
  }
  return(data)
}

theme_bakou <- function(font = "serif" , fs_title = 18, fs_subtitle= 18, fs_ax_leg = 17) {
  
  theme_classic() + theme(
    axis.line = element_blank(),
    axis.text = element_text(family = font, size = fs_ax_leg),
    axis.text.x = element_blank(),
    axis.title.x = element_text(family = font, size = fs_ax_leg),
    axis.ticks.x = element_blank(),
    legend.text = element_text(family = font,size=fs_ax_leg),
    legend.title = element_text(family = font,size=fs_ax_leg),
    legend.position = "none",
    strip.background = element_blank(),
    plot.title = element_text( family = font, size = fs_title, face = 'bold'), 
    plot.subtitle = element_text(family = font, size=fs_subtitle, face="italic", margin=margin(b=-30), hjust = 0.5 ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),    
    
  )
}
labs_bakou <- function(x=NULL, y=NULL, title, subtitle, tag = NULL) {
  labs(x= x ,y=y, title = title, subtitle = subtitle, tag = tag)  
  
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Visualiseer zelf de Belgische ongelijkheid !"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout( 
    sidebarPanel(       selectInput("ongelijkheid", "Verdeling van wat?", choices = c("IncomeTotal","Shares", "Income_WagePensTrans","FinancialAssets_noPensions","MutualFunds","Deposits","IncomeFinancial", "OwnHouse", "OtherHouse","TotAssets","NetFinancialPosition", "vehicles", "SelfEmplBus","BusWealth", 
                                                                                      "RealEstate",    "Bonds", "Bus_noSelfEmpl",  "ManagedAcc", "MoneyOwed_toHH", "OtherAssets", "Pension_nonPublic", "TotAssets_noPens", "IncomeWage",
                                                                                      "IncomeSelfEmpl", "IncomeRent",  "IncomePension", "IncomePubTransf", "IncomePrivTransf", "IncomeOther", "IncomeFriends", "IncomeMinusInterest_eq", "NetWealth_noPens",  "NetLiquidAsset",
                                                                                        "WPT_disp", "Income_disp")),
                        selectInput("tiel", "Over percentielen of decielen van de bevolking?", choices = c( 100,10)) ,
                        selectInput("rangschikking", "Rangschikking gebaseerd op wat?", choices = c("IncomeTotal", "IncomeMinusInterest_eq", "Income_disp",  "TotAssets", "FinancialAssets_noPensions", "Shares", "OwnHouse","OtherHouse", "IncomeWage", "Income_WagePensTrans", "IncomeWPT_noInt", "RealEstate",  "Deposits", "WPT_disp")),
                        selectInput("kleur", "Welk kleurtje moet de grafiek hebben", choices = c("#FF7579","#c61361", "#25a88e", "#757AFF", "#2d3142", "gray45"  )) 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("in percenten", plotOutput("plot1"  ), downloadButton("procenten", "Download", style= "color: #25a88e;background-color: transparent; border-color: transparent;"))  , 
        tabPanel("in euros",  plotOutput("plot2"  ), downloadButton("euros", "Download", style= "color: #25a88e;background-color: transparent; border-color: transparent;") )   , 
        tabPanel("gemiddelden per groep",  plotOutput("plot3"), downloadButton("gemiddeld", "Download", style= "color: #25a88e;background-color: transparent; border-color: transparent;")))
    )
    

    
    
    )
  

    ) 
    


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  x <- reactive( {
    
    paste0(input$rangschikking, "_", input$tiel, "_tile")
  })
  
  y <- reactive( {
    fifelse(input$tiel == 10, "deciel","percentiel")
  }) 
  
  data2 <- reactive( {
    
    deciles_maker(data %>% arrange(FinancialAssets_noPensions, IncomeTotal), vars = as.character(input$rangschikking), as.numeric(input$tiel))
    
  })
  pshares <- reactive( {
    x <- x()
    data2 <- data2()
    share_b(data2, as.character(x), as.character(input$ongelijkheid)) %>% select(x, pshare, psum, pmean) %>% distinct()
  })  
  
  
  p1 <- function() {
    pshares <- pshares()
    data3 <- data.frame(c( 
      x1 <- pshares[,1],
      x2 <- pshares[,2],
      x3 <- pshares[,3]
      ))
    colnames(data3) <- c(input$rangschikking, "pshare", "psum")
    data3 %>% group_by(data3[,1]) %>% ggplot(aes(x=data3[,1],y= data3[,2])) + geom_col(fill= input$kleur) + coord_cartesian(ylim=c(0, max(data3[,2] + 0.001)) ) + scale_y_continuous(labels= scales::percent) + theme_bakou() + labs_bakou(x= paste("van kleinste",input$rangschikking, "-",y(), "naar grootste"), y=NULL, title=NULL, subtitle=paste("aandeel van elk", input$rangschikking, "-", y(),  "\nin het totale", input$ongelijkheid)) + labs(caption = " Gemaakt door: Bakou Mertens in R Shiny, \n Data van: Eurosystem HFCS (wave 2020), \n Website: https://beconblog.quarto.pub/ \n")
  } 
  p2 <- function() {
    pshares <- pshares()
    data3 <- data.frame(c( 
      x1 <- pshares[,1],
      x2 <- pshares[,2],
      x3 <- pshares[,3]
    ))
    colnames(data3) <- c(input$rangschikking, "pshare", "psum")
    data3 %>% group_by(data3[,1]) %>% ggplot(aes(x=data3[,1],y= data3[,3])) + geom_col(fill= input$kleur) + coord_cartesian(ylim=c(0, max(data3[,3] + 10)) ) + scale_y_continuous(labels = scales::dollar_format(scale=0.000001, prefix = "€", suffix=" Milj." )) +theme_bakou() + labs_bakou(x= paste("van kleinste",input$rangschikking, "-",y(), "naar grootste"), y=NULL, title=NULL, subtitle=paste("Som van alle" , input$ongelijkheid, "per\n", input$rangschikking, "-", y())) + labs(caption = " Gemaakt door: Bakou Mertens in R Shiny, \n Data van: Eurosystem HFCS (wave 2020), \n Website: https://beconblog.quarto.pub/ \n")
    
  } 
  p3 <- function() {
    pshares <- pshares()
    data3 <- data.frame(c( 
      x1 <- pshares[,1],
      x3 <- pshares[,4]
    ))
    colnames(data3) <- c(input$rangschikking, "pmean")
    data3 %>% group_by(data3[,1]) %>% ggplot(aes(x=data3[,1],y= data3[,2])) + geom_col(fill= input$kleur) + coord_cartesian(ylim=c(0, max(data3[,2] + 10)) ) + scale_y_continuous(labels = scales::dollar_format(scale=0.001, prefix = "€", suffix=" \nduizend" )) +theme_bakou() + labs_bakou(x= paste("van kleinste",input$rangschikking, "-", y(), "naar grootste"), y=NULL, title=NULL, subtitle=paste("Gemiddelde" , input$ongelijkheid, "binnen elk\n", input$rangschikking, "-", y())) + labs(caption = " Gemaakt door: Bakou Mertens in R Shiny, \n Data van: Eurosystem HFCS (wave 2020), \n Website: https://beconblog.quarto.pub/ \n")
    
  }

    
    
    
    
    
    
    
    
  
  
  output$plot1 <- renderPlot({
    p1() 
    
  })
  
  output$procenten <- downloadHandler(
    filename = function() {
      paste("procenten", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file=file)
      plot(p1())
      dev.off()
    }
  )
  output$plot2 <- renderPlot({
    p2() 
    
  })
  
  output$euros <- downloadHandler(
    filename = function() {
      paste("euros", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file=file)
      plot(p2())
      dev.off()
    }
  )
  output$plot3 <- renderPlot({
    p3() 
    
  })
  output$gemiddeld <- downloadHandler(
    filename = function() {
      paste("gemiddeld", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file=file)
      plot(p3())
      dev.off()
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
