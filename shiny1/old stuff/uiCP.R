shinyUI(
    pageWithSidebar(
        # Application title
        headerPanel(
            "Whatcha gon\' type next?",
            list(tags$head(tags$style("body {background-color: blue; }")))
        ),
        sidebarPanel(
            textInput('words', 'You typed' , value= "for the first"),
            actionButton("goButton", "Go!")
        ),
        mainPanel(
            h3('You gon\' type'),
            verbatimTextOutput("sentence"),
            verbatimTextOutput("pick")
        )
    )
)
