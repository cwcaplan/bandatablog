shinyUI(pageWithSidebar(
    headerPanel(""),
    sidebarPanel(
        #verbatimTextOutput("chartChoice"),
        h4('Look At A Specific Year'),
        sliderInput('year', '', value = 2000,
                    min = 1984, max = 2015, step = 1,),
        selectInput("top5ChartPick", "Pick a Chart:", 
                    choices=list("Billboard 200"=1, "Country"=2, "R&B/Hip-Hop"=3),
                    selected=1),
        h4('Most Weeks at #1:'),
        verbatimTextOutput("top5"),
        h4('albums on both lists:'),
        verbatimTextOutput("winners"),
        h4('chart-toppers that the critics snubbed:'),
        verbatimTextOutput("losers")
    ),
    mainPanel(
        h3('Sales vs. Critics, Since 1984'),
        checkboxGroupInput('chartChoice', "Compare Critic's List To:", 
                           c("Billboard 200"="1", "Country"="2", "R&B/Hip-Hop" = "3"),
                           selected="1", inline=T),
        plotOutput("percPlot"),
        plotOutput("numPlotC"),
        plotOutput("numPlotT"),
        plotOutput("weeksPlotAvg"),
        plotOutput("weeksPlot"),
        plotOutput("RanGarTayPlot")
        #dataTableOutput("albums")
    )
))