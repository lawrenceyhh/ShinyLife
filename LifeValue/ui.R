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

shinyUI(
  dashboardPage(
    dashboardHeader(title = "ShinyLife"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Demission & Satisfaction", tabName = "dem", icon = icon("question")),
        menuItem("Job & Personal Life-Value", icon = icon("chalkboard-teacher"), startExpanded = TRUE,
                 menuSubItem("Life-Value", tabName = "lv", icon = icon("chart-bar")),
                 menuSubItem("Data Sample", tabName = "data", icon = icon("align-left"))
        ),
        menuItem("Comparision", icon = icon("balance-scale"),
                 menuSubItem("Company Comparision", tabName = "com", icon = icon("city")),
                 menuSubItem("Data Sample", tabName = "data2", icon = icon("align-left"))
        ),
        menuItem("Salary Plot", tabName = "plot", icon = icon("table")),
        menuItem("Raw Data", tabName = "alldata", icon = icon("database")),
        numericInput("WorkYear", "Years you plan to work:",
                     value = 1, min = 1, max = 46)
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dem",
                h3("On my job, this is what I feel about..."),
                fluidRow(
                  box(
                    selectInput("Q1", "1.	Being able to keep busy all the time",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q2", "2.	The chance to work alone on the job",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q3", "3.	The chance to do different things from time to time",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q4", "4.	The chance to be 'somebody' in the community",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q5", "5.	The way my boss handles his/her workers",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q6", "6.	The competence of my supervisor in making decisions",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q7", "7.	Being able to do things that don't go against my conscience",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q8", "8.	The way my job provides for steady employment",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px')
                  ),
                  box(
                    selectInput("Q9", "9.	The chance to do things for other people",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q10", "10.	The chance to tell people what to do",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q11", "11.	The chance to do something that makes use of my abilities",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q12", "12.	The way company policies are put into practice",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q13", "13.	My pay and the amount of work I do",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q14", "14.	The chances for advancement on this job",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    selectInput("Q15", "15.	The freedom to use my own judgment",
                                c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                    br(),
                    h5("#The questionnaire is adapted from the 'Manual for the Minnesota Satisfaction Questionnaire. Weiss, D. J., Dawis, R. V., & England, G. W. (1967).'")
                  )
                )
        ),
        tabItem(tabName = "lv",
                h2("Job & Personal Life-Value"),
                fluidRow(
                  box(
                    selectizeInput("aIndustry", "The industry you want to get in:", se$Industry),
                    selectizeInput("aCompany", "The company you want to get in:", se$Company),
                    selectInput("Education", "Your education level:", edu),
                    width = 3, height = 300),
                  box(
                    selectInput("Year", "Your current working experience: [years]",
                                c("0~1", "1~2", "2~3", "4~5", "5~7", "7~10", "10~15", "15~20", "20~30", "30+")),
                    selectInput("Hour", "The weekly working hours you can accept: [Hours/week]",
                                c("30-", "30~40", "40~45", "45~50", "50~55", "55~60", "60~70", "70~80",
                                  "80~90", "90~100", "120+")),
                    sliderInput("Fatigue", "Your tolerance to fatigue at work:",
                                min = 1, max = 8, value = 5),
                    width = 3, height = 300),
                  box(
                    sliderInput("Recommend", "How important do you think the recommendations of
                                the current staff are:",
                                min = 1, max = 5, value = 3),
                    sliderInput("GrowthHelp", "The job's importance to your future career growth",
                                min = 1, max = 5, value = 3),
                    sliderInput("CVHelp","The job's importance to your resume:",
                                min = 1, max = 5, value = 3),
                    width = 6, height = 300)
                ),
                fluidRow(
                  tabBox(
                    tabPanel("Salary Comparison",
                             plotOutput("aCompare"),
                             hr(),
                             h3(htmlOutput("aTotalSalary")),
                             h3(htmlOutput("aLifeIndex"))
                    ),
                    tabPanel("Summary",
                             fluidRow(valueBoxOutput("TS", width = 10)),
                             fluidRow(valueBoxOutput("LI", width = 10)),
                             fluidRow(infoBoxOutput("bcom", width = 10)),
                             fluidRow(infoBoxOutput("bsal", width = 10))
                    )
                  ),
                  tabBox(
                    side = "right",
                    tabPanel("Industry Distribution",
                             plotOutput("adensityind"), h4(htmlOutput("aPRInd"))
                    ),
                    tabPanel("Education Lv. Distribution",
                             plotOutput("adensityedu"), h4(htmlOutput("aPREdu"))
                    )
                  )
                )
        ),
        tabItem(tabName = "data",
                h3("Employee Salaries"),
                tableOutput("aSalaryTable")
        ),
        tabItem(tabName = "com",
                h2("Companies Comparison"),
                fluidRow(
                  box(title = "First Company", background = "blue",
                      selectizeInput("Industry", "The first industry:", se$Industry),
                      selectizeInput("Company", "The first company:", se$Company),
                      width = 4, height = 230),
                  box(title = "Second Company", background = "green",
                      selectizeInput("Industry2", "The second industry:", se$Industry),
                      selectizeInput("Company2", "The second company:", se$Company),
                      width = 4, height = 230),
                  box(title = "Third Company", background = "orange",
                      selectizeInput("Industry3", "The thrid industry:", se$Industry),
                      selectizeInput("Company3", "The thrid company:", se$Company),
                      width = 4, height = 230)
                ),
                fluidRow(
                  column(width = 6, offset = 6,
                         radioButtons("choice", "",
                                      c("First Company" = 1, "Second Company" = 2, "Third Company" = 3),
                                      inline = TRUE)
                  )),
                fluidRow(
                  tabBox(
                    tabPanel("Salary Comparison", plotOutput("Compare"),
                             hr(),
                             h3("Estimated Salary:"),
                             h4(htmlOutput("TotalSalary"), htmlOutput("TotalSalary2"), htmlOutput("TotalSalary3"))
                    ),
                    tabPanel("Summary",
                             fluidRow(valueBoxOutput("TS1", width = 8), valueBoxOutput("LI1", width = 4)),
                             fluidRow(valueBoxOutput("TS2", width = 8), valueBoxOutput("LI2", width = 4)),
                             fluidRow(valueBoxOutput("TS3", width = 8), valueBoxOutput("LI3", width = 4))
                    )
                  ),
                  tabBox(
                    side = "right",
                    tabPanel("Industry Distribution",
                             plotOutput("densityind"), h4(htmlOutput("PRInd"))
                    ),
                    tabPanel("Education Lv. Distribution",
                             plotOutput("densityedu"), h4(htmlOutput("PREdu"))
                    )
                  )
                )
        ),
        tabItem(tabName = "data2",
                h3("The first company"),
                tableOutput("SalaryTable"),
                hr(),
                h3("The second company"),
                tableOutput("SalaryTable2"),
                hr(),
                h3("The third company"),
                tableOutput("SalaryTable3")
        ),
        tabItem(tabName = "plot",
                h4("SalaryPlot"),
                fluidRow(column(width = 7, height = 850, align = "center", plotOutput("mpgPlot")))
        ),
        tabItem(tabName = "alldata",
                h4("RawData"),
                tableOutput("data")
        )
      )
    )
  )
)
