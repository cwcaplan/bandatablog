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
        #h5('Avg Weeks at #1:'),
        textOutput("avg"),
        h4('Most Weeks at #1:'),
        verbatimTextOutput("top5"),
        h4('albums on both lists:'),
        verbatimTextOutput("winners"),
        h4('chart-toppers that the critics snubbed:'),
        verbatimTextOutput("losers")
    ),
    mainPanel(
        h3('Vox Populi vs. Cognoscenti, Since 1984'),
        h5("Comparing how many records that reached #1 on the Billboard albums charts 
            also appeared in the Top 40 of that year's Pazz And Jop Poll.  Pazz And Jop 
            is an aggregated Best Albums of the Year list from several hundred critics.  
            It was started by Robert Christgau in 1971 and has been curated by the 
            Village Voice ever since."),
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