library(shiny)
data(euro)
cbind(euro)
ceuro <- euro
shinyServer(function(input, output) {
    convATS <- reactive({
        inputUnits <- input$sliderEuros
        ATS <- inputUnits * euro["ATS"]
    })
    
    convBEF <- reactive({
        inputUnits <- input$sliderEuros
        BEF <- inputUnits * euro["BEF"]
    })
    convDEM <- reactive({
        inputUnits <- input$sliderEuros
        DEM <- inputUnits * euro["DEM"]
    })
    convESP <- reactive({
        inputUnits <- input$sliderEuros
        ESP <- inputUnits * euro["ESP"]
    })
    convFIM <- reactive({
        inputUnits <- input$sliderEuros
        FIM <- inputUnits * euro["FIM"]
    })
    convFRF <- reactive({
        inputUnits <- input$sliderEuros
        FRF <- inputUnits * euro["FRF"]
    })
    convIEP <- reactive({
        inputUnits <- input$sliderEuros
        IEP <- inputUnits * euro["IEP"]
    })
##    convIEP <- reactive({
##        inputUnits <- input$sliderEuros
##        IEP <- inputUnits * euro["IEP"]
##    })
    convITL <- reactive({
        inputUnits <- input$sliderEuros
        ITL <- inputUnits * euro["ITL"]
    })
    convLUF <- reactive({
        inputUnits <- input$sliderEuros
        LUF <- inputUnits * euro["LUF"]
    })
    convNLG <- reactive({
        inputUnits <- input$sliderEuros
        NLG <- inputUnits * euro["NLG"]
    })
    convPTE <- reactive({
        inputUnits <- input$sliderEuros
        PTE <- inputUnits * euro["PTE"]
    })
    
    peuro1 <- reactive({
        ceuro <- euro
        ceuro[1] <- convATS()
        ceuro[2] <- convBEF()
        ceuro[3] <- convDEM()
        ceuro[4] <- convESP()
        ceuro[5] <- convFIM()
        ceuro[6] <- convFRF()
        ceuro[7] <- convIEP()
        ceuro[8] <- convITL()
        ceuro[9] <- convLUF()
        ceuro[10]<- convNLG()
        ceuro[11]<- convPTE()
        xeuro <- ceuro[1:11]
    })
    
    output$plot1 <- renderPlot({
         InputUnits <- input$sliderEuros
         titletext <- " Conversion Rate Between"
         titletext2 <- "Euro and EMU Currencies"
         titleunits <- InputUnits
         title <- paste(titletext,InputUnits,titletext2)
         xlaba <- "EMU Units per"
         xlabb <- "Euros"
         xlabab <- paste(xlaba,InputUnits,xlabb)
         ceurolim <- ceuro[8] * InputUnits + .5
         dotchart(peuro1(), xlim = c(0,ceurolim),
         xlab=xlabab,
         ylab="EMU Currencies",
         col.main="blue",bg="orange",
         main = title)
    
    })
    output$curr1 <- renderText({
        paste("Euro to ATS Austrian Schilling rate:",convATS())
##        convATS()
    })
    output$curr2 <- renderText({
        paste("Euro to BEF Belgian Franc rate:",convBEF())
##        convBEF()
    })
    output$curr3 <- renderText({
        paste("Euro to DEM German Mark rate:",convDEM())
##        convDEM()
    })
    output$curr4 <- renderText({
        paste("Euro to ESP Spanish Peseta rate:",convESP())
##        convESP()
    })
    output$curr5 <- renderText({
        paste("Euro to FIM Finnish Markka rate:",convFIM())
##        convFIM()
    })
    output$curr6 <- renderText({
        paste("Euro to FRF French Franc rate:",convFRF())
##       convFRF()
    })
    output$curr7 <- renderText({
          paste("Euro to IEP Irish Punt rate:",convIEP())
##        convIEP()
    })
    
    output$curr8 <- renderText({
          paste("Euro to ITL Italian Lira rate:",convITL())
##        convITL()
    })
    output$curr9 <- renderText({
          paste("Euro to LUF Luxembourg Franc rate:",convLUF())
##        convLUF()
    })
    output$curr10 <- renderText({
        paste("Euro to NLG Dutch Guilder rate:",convNLG())
##        convNLG()
    })
    output$curr11 <- renderText({
        paste("Euro to PTE Portuguese Escudo rate:",convPTE())
##      convPTE()
    })
  
    ATSeuro <- reactive({
        inputUnits <- input$sliderEuros
        ATSeu <- inputUnits / euro["ATS"]
    })
    
    BEFeuro <- reactive({
        inputUnits <- input$sliderEuros
        BEFeu <- inputUnits / euro["BEF"]
    })
    DEMeuro <- reactive({
        inputUnits <- input$sliderEuros
        DEMeu <- inputUnits / euro["DEM"]
    })
    ESPeuro <- reactive({
        inputUnits <- input$sliderEuros
        ESPeu <- inputUnits / euro["ESP"]
    })
    FIMeuro <- reactive({
        inputUnits <- input$sliderEuros
        FIMeu <- inputUnits / euro["FIM"]
    })
    FRFeuro <- reactive({
        inputUnits <- input$sliderEuros
        FRFeu <- inputUnits / euro["FRF"]
    })
    IEPeuro <- reactive({
        inputUnits <- input$sliderEuros
        IEPeu <- inputUnits / euro["IEP"]
        })
    ITLeuro <- reactive({
        inputUnits <- input$sliderEuros
        ITLeu <- inputUnits / euro["ITL"]
    })
    LUFeuro <- reactive({
        inputUnits <- input$sliderEuros
        LUFeu <- inputUnits / euro["LUF"]
    })
    NLGeuro <- reactive({
        inputUnits <- input$sliderEuros
        NLGeu <- inputUnits / euro["NLG"]
    })
    PTEeuro <- reactive({
        inputUnits <- input$sliderEuros
        PTEeu <- inputUnits / euro["PTE"]
    })  
    
    peuro2 <- reactive({
        ceuro <- euro
        ceuro[1] <- ATSeuro()
        ceuro[2] <- BEFeuro()
        ceuro[3] <- DEMeuro()
        ceuro[4] <- ESPeuro()
        ceuro[5] <- FIMeuro()
        ceuro[6] <- FRFeuro()
        ceuro[7] <- IEPeuro()
        ceuro[8] <- ITLeuro()
        ceuro[9] <- LUFeuro()
        ceuro[10]<- NLGeuro()
        ceuro[11]<- PTEeuro()
        xeuro <- ceuro[1:11]
    })
    
    
    output$plot2 <- renderPlot({
        InputUnits <- input$sliderEuros
        titletext <- "Conversion Rate Between"
        titletext2 <- "EMU Units and Euros"
        titleunits <- InputUnits
        title <- paste(titletext,InputUnits,titletext2)
        xlab1 <- "EMU Currencies ("
        xlab2 <- "Units ="
        xlab3 <- ")"
        xlabel <- paste(xlab1,xlab2,InputUnits,xlab3)
        ceurolim <- ceuro[8] * InputUnits + .5
        barplot(peuro2(),
        col="blue", ylab=("Euros"),xlab=xlabel,
        main = title)
    })
    peuro3 <- reactive ({
    round(peuro2(),2)
    })
    peuro4 <- reactive ({
        round(peuro2(),4)
    })
    peuro5 <- reactive ({
        round(peuro2(),5)
    })
    output$euro1 <- renderText({
        paste("ATS Austrian Schilling to Euro rate:",peuro3()[1])
        ##        convATS()
    })
    output$euro2 <- renderText({
        paste("BEF Belgian Franc to Euro rate:",peuro3()[2])
        ##        convBEF()
    })
    output$euro3 <- renderText({
        paste("DEM German Mark to Euro:",peuro3()[3])
        ##        convDEM()
    })
    output$euro4 <- renderText({
        paste("ESP Spanish Peseta to Euro rate:",peuro3()[4])
        ##        convESP()
    })
    output$euro5 <- renderText({
        paste("FIM Finnish Markka:",peuro3()[5])
        ##        convFIM()
    })
    output$euro6 <- renderText({
        paste("FRF French Franc to Euro rate:",peuro3()[6])
        ##       convFRF()
    })
    output$euro7 <- renderText({
        paste("IEP Irish Punt to Euro rate:",peuro3()[7])
        ##        convIEP()
    })
    
    output$euro8 <- renderText({
        paste("ITL Italian Lira to Euro rate:",peuro5()[8])
        ##        convITL()
    })
    output$euro9 <- renderText({
        paste("LUF Luxembourg Franc to Euro rate:",peuro3()[9])
        ##        convLUF()
    })
    output$euro10 <- renderText({
        paste("NLG Dutch Guilder to Euro rate:",peuro3()[10])
        ##        convNLG()
    })
    output$euro11 <- renderText({
        paste("PTE Portuguese Escudo to Euro rate:",peuro4()[11])
        ##      convPTE()
    })
    
##    output$plot2 <- renderPlot({
##        InputUnits <- input$sliderEuros
##        titletext <- " Euros per " Converted to European Monetary Union (EMU) Currencies"
##        titleunits <- InputUnits
##        title <- paste(InputUnits,titletext)
##        ceurolim <- ceuro[8] * InputUnits + .5
##        dotchart(peuro2(), xlim = c(0,ceurolim),
##        xlab="EMU Currency Units",ylab="EMU Currencies",
##        col.main="blue",bg="orange",
##        main = title)
##    })    
peuro4
output$doc1 <- renderText({
    "To convert Euros to the various EMU currencies:"
})
output$doc2 <- renderText({
    "1. Select the EURO TO EMU tab"
})
output$doc3 <- renderText({
    "2. Use the slider widget on the left to choose the number of units to convert."
})
output$doc4 <- renderText({
    "3. Click 'Submit'"
})

output$doc5 <- renderText({
    "Output:" 
})
output$doc5a <- renderText({
    "1. A dotchart depicting the rate of exchange from Euros to the various EMU
    currencies based on the number of units selected (units = Euro units)." 
})
output$doc5b <- renderText({
    "2. A list of the rate of exchange from Euros to the various EMU currencies
    based on the number of units selected (units = Euro units.)" 
})
output$doc6 <- renderText({
    "To convert the various EMU currencies to Euros:"
})
output$doc7 <- renderText({
    "1. Select the EMU TO EURO tab"
})
output$doc8 <- renderText({
    "2. Use the slider widget on the left to choose the number of units to convert."
})
output$doc9 <- renderText({
    "3. Click 'Submit'"
})
output$doc10 <- renderText({
    "Output:" 
})
output$doc10a <- renderText({
    "1. A histogram depicting the rates of exchange from the various EMU
    currencies to Euros based on the number of units selected (units = EMU
    currency units)."
    
})
output$doc10b <- renderText({
    "2. A list of the rates of exchange from the various EMU currencies to Euros
    based on the number of units selected (units = EMU currency units)."
})
    
})
    