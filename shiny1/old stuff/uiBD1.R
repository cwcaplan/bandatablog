shinyUI(pageWithSidebar(
    headerPanel("Illustrating inputs"),
    sidebarPanel(
        checkboxGroupInput("City", "City", 
                           c("Chicago" = "1", "New York" = "2", "St. Louis" = "3",
                             "Washington, DC" = "4", "Seattle" = "5"), 
                           selected=c("1", "2", "3", "4", "5")),
        selectInput("stat",
                    label="select a statistic",
                    choices=list("Radio Spins"=2,"Press Features"=3,"Album Sales"=4,        
                                    "Merch Sales"=5,"Ticket Sales"=6,"Show Income"=7,
                                 "Show Merch"=8, "Shows Played"=9),                  
                    selected=2),
        sliderInput('days', 'Number of Days', value = 56, 
                    min = 14, max = 1500, step = 14,)
        #selectInput("time",
        #            label="select a statistic",
        #            choices=list("Two Weeks"=1, "Two Months"=2,
        #                         "Six Months"=3,"One Year"=4,        
        #                         "Two Years"=5,"Five Years"=6),                  
        #            selected=1)
        #dateInput("date", "Date:")
    ),
    mainPanel(
        #h3('illustrating outputs'),
        #h4('You entered'),
        #verbatimTextOutput("oid2"),
        #h4('stat number'),
        #verbatimTextOutput("statnum"),
        #h4('You entered'),
        #verbatimTextOutput("odate"),
        plotOutput("cityBarPlot")
    )
))