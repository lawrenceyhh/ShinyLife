library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
require(DMwR)

se <- read.csv(url("https://drive.google.com/uc?export=download&id=1h5GKPHtblt0jHmtasvcxVJL5m04-ajBm"))
se <- knnImputation(se)

se$Fatigue <- se$Fatigue %>% round() %>% as.factor()
se$Recommend <- se$Recommend %>% round() %>% as.factor()
se$GrowthHelp <- se$GrowthHelp %>% round() %>% as.factor()
se$CVHelp <- se$CVHelp %>% round() %>% as.factor()
edu <- sort(se$Education)

shinyServer(
  function(input, output, session){

    ###The 1st Tab
    Gamma <- reactive({
      Q1 <- input$Q1 %>% as.numeric()
      Q2 <- input$Q2 %>% as.numeric()
      Q3 <- input$Q3 %>% as.numeric()
      Q4 <- input$Q4 %>% as.numeric()
      Q5 <- input$Q5 %>% as.numeric()
      Q6 <- input$Q6 %>% as.numeric()
      Q7 <- input$Q7 %>% as.numeric()
      Q8 <- input$Q8 %>% as.numeric()
      Q9 <- input$Q9 %>% as.numeric()
      Q10 <- input$Q10 %>% as.numeric()
      Q11 <- input$Q11 %>% as.numeric()
      Q12 <- input$Q12 %>% as.numeric()
      Q13 <- input$Q13 %>% as.numeric()
      Q14 <- input$Q14 %>% as.numeric()
      Q15 <- input$Q15 %>% as.numeric()
      Qsum <- Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q11+Q12+Q13+Q14+Q15
      r <- exp(Qsum)/(1+exp(Qsum))
    })

    ###The 2nd Tab
    #2-1 Input & BarChart & LifeIndex
    observeEvent(
      input$aIndustry,
      updateSelectizeInput(session, "aCompany",  "",
                           choices = se$Company [se$Industry == input$aIndustry])
    )

    Gdup <- reactive({
      req(input$aIndustry, input$aCompany, input$Education, input$Hour, input$Fatigue)
      sb <- se %>% lm(GrowthRate ~ Industry + Company + Education + Hour + Fatigue, .)
      sd <- data.frame(Industry = input$aIndustry, Company = input$aCompany, Education = input$Education,
                       Hour = input$Hour, Fatigue = as.factor(input$Fatigue))
      predG <- predict(sb, sd)
    })
    P1dup <- reactive({
      req(input$aIndustry, input$aCompany, input$Education, input$Year, input$Hour, input$Fatigue,
          input$Recommend, input$GrowthHelp, input$CVHelp)
      base <- se %>% lm(SalaryY ~ Industry + Company + Education + Year + Hour + Fatigue + Recommend
                        + GrowthHelp + CVHelp, .)
      i <- data.frame(Industry = input$aIndustry, Company = input$aCompany, Education = input$Education,
                      Year = input$Year, Hour = input$Hour, Fatigue = as.factor(input$Fatigue),
                      Recommend = as.factor(input$Recommend), GrowthHelp = as.factor(input$GrowthHelp),
                      CVHelp = as.factor(input$CVHelp))
      pred <- predict(base, i)
    })
    Sumdup <- reactive({
      req(input$WorkYear)
      g <- Gdup()
      i <- 2.73
      r <- (1+g/100)/(1+i/100)
      Pt <- Gamma()*P1dup()*(1-r^(input$WorkYear))/(1-r)*10000
      Pt <- Pt %>% ceiling()
    })

    output$aCompare <- renderPlot({
      req(Sumdup(),input$WorkYear)
      meanind <- 10*mean(subset(se$SalaryY, se$Industry == input$aIndustry))
      meanedu <- 10*mean(subset(se$SalaryY, se$Education == input$Education))
      meanoverall <- 10*mean(se$SalaryY)
      com <- c(Sumdup()/1000,meanind*(input$WorkYear),meanedu*(input$WorkYear),meanoverall*(input$WorkYear))
      name <- c("Your Salary", "Ind. Avg.", "Edu. Avg.", "All Emp. Avg.")
      barplot(com, main = "Comparison", las = 1, names.arg = name, col=c("steelblue3","springgreen3","yellow1","tomato1"),
              ylab = "Total Salary(in thousands)")
    })

    LifeIndex <- reactive({
      req(input$aCompany, input$WorkYear)
      est <- (Sumdup() / (input$WorkYear*10000))
      highersubset <- subset(se$SalaryY, se$SalaryY>est)
      PR <- (1 - (length(highersubset) / length(se$SalaryY)))*100
      PR <- round(PR,1)
    })

    output$aTotalSalary <- renderText({
      Sumdup <- Sumdup() %>% format(., big.mark=",", scientific=FALSE)
      paste("Estimated Total Salary:", "<font color=\"#FF0000\"><b>", Sumdup, "($ NTD)", "</b></font>")
    })
    output$aLifeIndex <- renderText({
      paste("Your Life-Value Index =", "<font color=\"#FF0000\"><b>", LifeIndex(), "/100", "</b></font>")
    })
    output$TS <- renderValueBox({
      valueBox(paste(Sumdup(),"($ NTD)"), "Estimated Total Salary",
               color = "orange", icon("money"))
    })
    output$LI <- renderValueBox({
      valueBox(paste(LifeIndex(), "%"), "Estimated Life Index",
               color = "maroon", icon("chart-line"))
    })
    #2-2 Filter Data
    output$aSalaryTable <- renderTable({
      se %>% filter(input$aIndustry == se$Industry & input$aCompany == se$Company)
    })
    #2-3 Distribution & PR
    output$adensityind <- renderPlot({
      req(Sumdup())
      subset1 <- subset(se$SalaryY, se$Industry == input$aIndustry)
      abc <- density(subset1)
      point <- (Sumdup() / (input$WorkYear*10000))
      plot(abc,
           main = "Current Industry",
           xlab = "Salary(in millions)",
           ylab = "Density"
      )
      abline(v= point)
    })
    output$adensityedu <- renderPlot({
      req(Sumdup())
      subset1 <- subset(se$SalaryY, se$Education == input$Education)
      abc <- density(subset1)
      point <- (Sumdup() / (input$aWorkYear*10000))
      plot(abc,
           main = "Current Education Level",
           xlab = "Salary(in millions)",
           ylab = "Density"
      )
      abline(v= point)
    })

    IndPRdup <- reactive({
      req(input$aIndustry,input$WorkYear)
      est <- (Sumdup() / (input$WorkYear*10000))
      subset1 <- subset(se$SalaryY, se$Industry == input$aIndustry)
      highersubset <- subset(se$SalaryY, se$Industry == input$aIndustry & se$SalaryY>est)
      PR <- (1 - (length(highersubset) / length(subset1)))*100
      PR <- round(PR,1)
    })
    EduPRdup <- reactive({
      req(input$Education,input$WorkYear)
      est <- (Sumdup() / (input$WorkYear*10000))
      subset1 <- subset(se$SalaryY, se$Education == input$Education)
      highersubset <- subset(se$SalaryY, se$Education == input$Education & se$SalaryY>est)
      PR <- (1 - (length(highersubset) / length(subset1)))*100
      PR <- round(PR,1)
    })
    output$aPRInd <- renderText({
      paste("#Your Estimated Salary is higher than ", "<font color=\"#FF0000\"><b>", IndPRdup(),"%", "</b></font>", "of people in the same industry.")

    })
    output$aPREdu <- renderText({
      paste("#Your Estimated Salary is higher than ", "<font color=\"#FF0000\"><b>", EduPRdup(),"%", "</b></font>", "of people with the same education level as you.")
    })
    #2-4 Recommendation
    bestcomp <- reactive({
      req(input$aIndustry)
      subset1 <- se[which(se$Industry==input$aIndustry),]
      asdf <- subset1 %>%
        group_by(Company) %>%
        summarize(avg=mean(SalaryY))
      ordered <- asdf[order(-asdf$avg),]
      bestcomp <- lapply(ordered[1,1], as.character)
    })

    bestsal <- reactive({
      req(input$aIndustry)
      subset1 <- se[which(se$Industry==input$aIndustry),]
      asdf <- subset1 %>%
        group_by(Company) %>%
        summarize(avg=mean(SalaryY))
      ordered <- asdf[order(-asdf$avg),]
      bestcomp <- lapply(ordered[1,2], as.character)
    })

    output$bestcomp <- renderText({
      paste("Best Company in Current Industry: ", "<font color=\"blue\"><b>", bestcomp(), "</b></font>")
    })

    output$bestsal <- renderText({
      paste("Salary in month: ", "<font color=\"blue\"><b>", bestsal(), "</b></font>")
    })

    output$bcom <- renderInfoBox({
      infoBox(title = "Recommend Company",
              value = bestcomp(),
              subtitle = "The company that suits you the most",
              fill = T, color = "blue", icon("building"))
    })
    output$bsal <- renderInfoBox({
      infoBox(title = "Salary",
              value = bestsal(),
              subtitle = "Highest possible salary (/month)",
              fill = T, color = "green", icon("dollar-sign"))
    })

    ###The 3rd Tab
    #3-1 Input & BarChart & TotalSalary
    observeEvent(
      input$aIndustry,
      updateSelectizeInput(session, "Industry", "",
                           selected = input$aIndustry)
    )
    observeEvent(
      input$aCompany,
      updateSelectizeInput(session, "Company", "",
                           selected = input$aCompany)
    )
    observeEvent(
      input$Industry,
      updateSelectizeInput(session, "Company",  "",
                           choices = se$Company [se$Industry == input$Industry])
    )
    observeEvent(
      input$Industry2,
      updateSelectizeInput(session, "Company2",  "",
                           choices = se$Company [se$Industry == input$Industry2
                                                 & se$Company != input$Company])
    )
    observeEvent(
      input$Industry3,
      updateSelectizeInput(session, "Company3",  "",
                           choices = se$Company [se$Industry == input$Industry3
                                                 & se$Company != input$Company
                                                 & se$Company != input$Company2])
    )

    P1 <- reactive({
      req(input$Industry, input$Company, input$Education, input$Year, input$Hour, input$Fatigue,
          input$Recommend, input$GrowthHelp, input$CVHelp)
      base <- se %>% lm(SalaryY ~ Industry + Company + Education + Year + Hour + Fatigue + Recommend
                        + GrowthHelp + CVHelp, .)
      i <- data.frame(Industry = input$Industry, Company = input$Company, Education = input$Education,
                      Year = input$Year, Hour = input$Hour, Fatigue = as.factor(input$Fatigue),
                      Recommend = as.factor(input$Recommend), GrowthHelp = as.factor(input$GrowthHelp),
                      CVHelp = as.factor(input$CVHelp))
      pred <- predict(base, i)
    })
    P12 <- reactive({
      req(input$Industry2, input$Company2, input$Education, input$Year, input$Hour, input$Fatigue,
          input$Recommend, input$GrowthHelp, input$CVHelp)
      base <- se %>% lm(SalaryY ~ Industry + Company + Education + Year + Hour + Fatigue + Recommend
                        + GrowthHelp + CVHelp, .)
      i <- data.frame(Industry = input$Industry2, Company = input$Company2, Education = input$Education,
                      Year = input$Year, Hour = input$Hour, Fatigue = as.factor(input$Fatigue),
                      Recommend = as.factor(input$Recommend), GrowthHelp = as.factor(input$GrowthHelp),
                      CVHelp = as.factor(input$CVHelp))
      pred <- predict(base, i)
    })
    P13 <- reactive({
      req(input$Industry3, input$Company3, input$Education, input$Year, input$Hour, input$Fatigue,
          input$Recommend, input$GrowthHelp, input$CVHelp)
      base <- se %>% lm(SalaryY ~ Industry + Company + Education + Year + Hour + Fatigue + Recommend
                        + GrowthHelp + CVHelp, .)
      i <- data.frame(Industry = input$Industry3, Company = input$Company3, Education = input$Education,
                      Year = input$Year, Hour = input$Hour, Fatigue = as.factor(input$Fatigue),
                      Recommend = as.factor(input$Recommend), GrowthHelp = as.factor(input$GrowthHelp),
                      CVHelp = as.factor(input$CVHelp))
      pred <- predict(base, i)
    })

    G <- reactive({
      req(input$Industry, input$Company, input$Education, input$Hour, input$Fatigue)
      sb <- se %>% lm(GrowthRate ~ Industry + Company + Hour + Fatigue + Education, .)
      sd <- data.frame(Industry = input$Industry, Company = input$Company, Education = input$Education,
                       Hour = input$Hour, Fatigue = as.factor(input$Fatigue))
      predG <- predict(sb, sd)
    })
    G2 <- reactive({
      req(input$Industry2, input$Company2, input$Education, input$Hour, input$Fatigue)
      sb <- se %>% lm(GrowthRate ~ Industry + Company + Hour + Fatigue + Education, .)
      sd <- data.frame(Industry = input$Industry2, Company = input$Company2, Education = input$Education,
                       Hour = input$Hour, Fatigue = as.factor(input$Fatigue))
      predG <- predict(sb, sd)
    })
    G3 <- reactive({
      req(input$Industry3, input$Company3, input$Education, input$Hour, input$Fatigue)
      sb <- se %>% lm(GrowthRate ~ Industry + Company + Hour + Fatigue + Education, .)
      sd <- data.frame(Industry = input$Industry3, Company = input$Company3, Education = input$Education,
                       Hour = input$Hour, Fatigue = as.factor(input$Fatigue))
      predG <- predict(sb, sd)
    })
    Sum <- reactive({
      req(input$WorkYear)
      g <- G()
      i <- 2.73
      r <- (1+g/100)/(1+i/100)
      Pt <- Gamma()*P1()*(1-r^(input$WorkYear))/(1-r)*10000
      Pt <- Pt %>% ceiling()
    })
    Sum2 <- reactive({
      req(input$WorkYear)
      g <- G2()
      i <- 2.73
      r <- (1+g/100)/(1+i/100)
      Pt <- Gamma()*P12()*(1-r^(input$WorkYear))/(1-r)*10000
      Pt <- Pt %>% ceiling()
    })
    Sum3 <- reactive({
      req(input$WorkYear)
      g <- G3()
      i <- 2.73
      r <- (1+g/100)/(1+i/100)
      Pt <- Gamma()*P13()*(1-r^(input$WorkYear))/(1-r)*10000
      Pt <- Pt %>% ceiling()
    })

    output$Compare <- renderPlot({
      req(Sum(), Sum2(), Sum3())
      com <- c(Sum()/10000, Sum2()/10000, Sum3()/10000)
      name <- c(input$Company, input$Company2, input$Company3)
      barplot(com, main = "Comparison", names.arg = name, col=c("royalblue", "springgreen3", "orange"))
    })

    output$TotalSalary <- renderText({
      Sum <- Sum() %>% format(., big.mark=",", scientific=FALSE)
      paste("First Company:", "<font color=\"#0066ff\"><b>", Sum,"($ NTD)", "</b></font>")
    })
    output$TotalSalary2 <- renderText({
      Sum2 <- Sum2() %>% format(., big.mark=",", scientific=FALSE)
      paste("Second Company:", "<font color=\"#009933\"><b>", Sum2,"($ NTD)", "</b></font>")
    })
    output$TotalSalary3 <- renderText({
      Sum3 <- Sum3() %>% format(., big.mark=",", scientific=FALSE)
      paste("Third Company:", "<font color=\"#ff3300\"><b>", Sum3, "($ NTD)", "</b></font>")
    })
    LifeIndex1 <- reactive({
      req(input$Company, input$WorkYear)
      est <- (Sum() / (input$WorkYear*10000))
      highersubset <- subset(se$SalaryY, se$SalaryY>est)
      PR <- (1 - (length(highersubset) / length(se$SalaryY)))*100
      PR <- round(PR,1)
    })
    LifeIndex2 <- reactive({
      req(input$Company2, input$WorkYear)
      est <- (Sum2() / (input$WorkYear*10000))
      highersubset <- subset(se$SalaryY, se$SalaryY>est)
      PR <- (1 - (length(highersubset) / length(se$SalaryY)))*100
      PR <- round(PR,1)
    })
    LifeIndex3 <- reactive({
      req(input$Company3, input$WorkYear)
      est <- (Sum3() / (input$WorkYear*10000))
      highersubset <- subset(se$SalaryY, se$SalaryY>est)
      PR <- (1 - (length(highersubset) / length(se$SalaryY)))*100
      PR <- round(PR,1)
    })
    output$TS1 <- renderValueBox({
      valueBox(paste(Sum(),"($ NTD)"), "Salary 1",
               color = "blue", icon("money"))
    })
    output$LI1 <- renderValueBox({
      valueBox(paste(LifeIndex1(),"%"), "Life Index 1",
               color = "light-blue", icon("chart-line"))
    })
    output$TS2 <- renderValueBox({
      valueBox(paste(Sum2(),"($ NTD)"), "Salary 2",
               color = "green", icon("money"))
    })
    output$LI2 <- renderValueBox({
      valueBox(paste(LifeIndex2(),"%"), "Life Index 2",
               color = "olive", icon("chart-line"))
    })
    output$TS3 <- renderValueBox({
      valueBox(paste(Sum3(),"($ NTD)"), "Salary 3",
               color = "orange", icon("money"))
    })
    output$LI3 <- renderValueBox({
      valueBox(paste(LifeIndex3(),"%"), "Life Index 3",
               color = "yellow", icon("chart-line"))
    })
    #3-2 Filter Data
    output$SalaryTable <- renderTable({
      se %>% filter(input$Industry == se$Industry & input$Company == se$Company)
    })
    output$SalaryTable2 <- renderTable({
      se %>% filter(input$Industry2 == se$Industry & input$Company2 == se$Company)
    })
    output$SalaryTable3 <- renderTable({
      se %>% filter(input$Industry3 == se$Industry & input$Company3 == se$Company)
    })
    #3-3 Distribution & PR
    Indust <- reactive({
      if(input$choice == 1){
        Ind <- input$Industry
      } else if(input$choice == 2){
        Ind <- input$Industry2
      } else if(input$choice == 3){
        Ind <- input$Industry3
      }
    })
    IndSal <- reactive({
      if(input$choice == 1){
        Sal <- Sum()
      } else if(input$choice == 2){
        Sal2 <- Sum2()
      } else if(input$choice == 3){
        Sal3 <- Sum3()
      }
    })
    output$densityind <- renderPlot({
      req(IndSal())
      subset1 <- subset(se$SalaryY, se$Industry == Indust())
      abc <- density(subset1)
      point <- (IndSal() / (input$WorkYear*10000))
      plot(abc,
           main = "Current Industry",
           xlab = "Salary(in millions)",
           ylab = "Density"
      )
      abline(v= point)
    })
    output$densityedu <- renderPlot({
      req(IndSal())
      subset1 <- subset(se$SalaryY, se$Education == input$Education)
      abc <- density(subset1)
      point <- (IndSal() / (input$WorkYear*10000))
      plot(abc,
           main = "Current Education Level",
           xlab = "Salary(in millions)",
           ylab = "Density"
      )
      abline(v= point)
    })

    IndPR <- reactive({
      req(Indust())
      est <- (IndSal() / (input$WorkYear*10000))
      subset1 <- subset(se$SalaryY, se$Industry == Indust())
      highersubset <- subset(se$SalaryY, se$Industry == Indust() & se$SalaryY>est)
      PR <- (1 - (length(highersubset) / length(subset1)))*100
      PR <- round(PR,1)
    })
    EduPR <- reactive({
      req(input$Education)
      est <- (IndSal() / (input$WorkYear*10000))
      subset1 <- subset(se$SalaryY, se$Education == input$Education)
      highersubset <- subset(se$SalaryY, se$Education == input$Education & se$SalaryY>est)
      PR <- (1 - (length(highersubset) / length(subset1)))*100
      PR <- round(PR,1)
    })

    output$PRInd <- renderText({
      paste("#Your Estimated Salary is higher than ", "<font color=\"#FF0000\"><b>", IndPR(),"%", "</b></font>", "of people in the same industry.")
    })
    output$PREdu <- renderText({
      paste("#Your Estimated Salary is higher than ", "<font color=\"#FF0000\"><b>", EduPR(),"%", "</b></font>", "of people with the same education level as you.")
    })

    ###The 4th Tab
    output$mpgPlot <- renderPlot({
      par(mfrow=c(2,1), mar = c(5, 40, 2, 1))
      boxplot(se$SalaryY~se$Industry,
              col = "#74AADB", pch = 19, horizontal = T, las = 2, main = "Industry Comparison",
              xlab = "Salary per year (10,000)")
      boxplot(se$SalaryY~se$Education,
              col = "#75AADB", pch = 19 , horizontal = T, las = 2, main = "Education Level Comparison",
              xlab = "Salary per year (10,000)")}, height = 850, width = 1000)

    ###The 5th Tab
    output$data <- renderTable({
      se
    })

  }
)
