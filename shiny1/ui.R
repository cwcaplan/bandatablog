shinyUI(pageWithSidebar(
    headerPanel("Look At A Year"),
    sidebarPanel(
        sliderInput('year', 'Which Year?', value = 2000, 
                    min = 1984, max = 2015, step = 1,),
        h4('albums on both lists:'),
        verbatimTextOutput("winners"),
        h4('chart-toppers that the critics snubbed:'),
        verbatimTextOutput("losers")
    ),
    mainPanel(
        h3('Sales vs. Critics, Since 1984'),
        plotOutput("percPlot")
        #dataTableOutput("albums")
    )
))