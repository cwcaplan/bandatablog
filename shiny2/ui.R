shinyUI(pageWithSidebar(
    headerPanel(""),
    sidebarPanel(
        #verbatimTextOutput("chartChoice"),
        h4('Look At A Specific Year'),
        sliderInput('year', label='', value = 2000,
                    min = 1984, max = 2015, step = 1),
        conditionalPanel(
            condition = "input.albSing == 1",
            selectInput("pick1", "Pick a Chart:", 
                        choices=list("Billboard 200"=1,
                                     "Country"=3,
                                     "R&B/Hip-Hop"=5))
        ),
        conditionalPanel(
            condition = "input.albSing == 2",
            selectInput("pick2", "Pick a Chart:", 
                        choices=list("Billboard Hot 100 Singles"=2,
                                     "Country Singles"=4,
                                     "R&B/Hip-Hop Singles"=6,
                                     "Mainstream Rock Singles"=7))
        ),
        conditionalPanel(
            condition = "input.albSing == 3",
            selectInput("pick3", "Pick a Chart:", 
                        choices=list("Billboard 200"=1, "Billboard Hot 100 Singles"=2,
                                     "Country"=3, "Country Singles"=4,
                                     "R&B/Hip-Hop"=5, "R&B/Hip-Hop Singles"=6,
                                     "Mainstream Rock Singles"=7))
        ),
        radioButtons('pickList', "", c("Summary"=1, "Sales"=2, "Critics"=3), inline=T),
        textOutput("avg"),
        htmlOutput("bbList"),
        htmlOutput("pjList"),
        h4('Most Weeks at #1:'),
        verbatimTextOutput("top5"),
        h4('records on both lists:'),
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
        radioButtons('commentary', "", c("Show Commentary"=1, "Hide commentary"=2), inline=T),
        textOutput("topComm"),
        radioButtons('albSing', "", c("Albums"=1, "Singles"=2, "Both"=3), 
                     inline=T, selected=3),
        checkboxGroupInput('chartChoice', "Compare Critic's List To:", 
                           c("Billboard 200"="1", "Country"="2", "R&B/Hip-Hop" = "3", 
                             "Mainstream Rock" = "4"),
                           selected="1", inline=T),
        plotOutput("percPlot"),
        textOutput("percPlotComm"),
        plotOutput("numPlotC"),
        textOutput("numPlotCComm"),
        plotOutput("numPlotT"),
        plotOutput("weeksPlotAvg"),
        plotOutput("weeksPlot"),
        plotOutput("RanGarTayPlot")
        #dataTableOutput("albums")
    )
))