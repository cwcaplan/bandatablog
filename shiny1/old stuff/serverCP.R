library(stylo)

simpleGpreds <- function(pwords){
    preds <- predssupertrim
    if(length(grep(pwords[3], preds$w3))>0){
        preds <- preds[grep(pwords[3], preds$w3), ]
    }else{preds <- preds}
    if(length(grep(pwords[2], preds$w2))>0){
        preds <- preds[grep(pwords[2], preds$w2), ]
    }else{preds <- preds}
    if(length(grep(pwords[1], preds$w1))>0){
        preds <- preds[grep(pwords[1], preds$w1), ]
    }else{preds <- preds}
    output <- preds$w4[1] 
    output
}

predictor <- function(pwords){
    preds <- predssupertrim
    #word3, 2grams
    if(length(grep(pwords[3], preds$w3))>0){
        preds <- preds[preds$w3==pwords[3], ]
    }else{preds <- preds[!preds$tag==2, ]}
    ##maybe do a backoff for w3
    #word2, 3grams
    if(is.na(pwords[2])){preds <- preds[!preds$tag==3, ]
    }else if(length(grep(pwords[2], preds$w2))>0){
        preds <- preds[preds$w2==pwords[2], ]
    }else{preds <- preds[!preds$tag==3, ]}
    #word1, 4grams
    if(is.na(pwords[1])){preds <- preds[!preds$tag==4, ]
    }else if(length(grep(pwords[1], preds$w1))>0){
        preds <- preds[preds$w1==pwords[1], ]
    }else{preds <- preds[!preds$tag==4, ]}
    #remove erroneous NAs
    preds <- preds[complete.cases(preds$w4), ]
    output <- character(length=1)
    #if no match, try grep
    if(length(preds$w4)>0){output <- preds$w4[1]
    }else{
        gPred <- simpleGpreds(pwords)
        if(is.na(gPred)) {
            ifelse(grepl(".+a$|.+b$|.+c$|.+i$|.+q$|.+s$|.+v$|.+x$|.+z$", pwords[3]),
                   output <- "and",
                   output <- "the")
        }else{
            output <- gPred
        }
    }
    output
}

bigPicker <- function(words){
    words <- gsub("[^A-Za-z0-9'# -]", "", words) #what we're keeping
    words <- unlist(strsplit(words, "[ ]")) #actually make the split
    pwords <- as.character(c(words[length(words)-2], words[length(words)-2], words[length(words)]))
    answer <- predictor(pwords)
    answer
}



shinyServer(
    function(input, output) {
        pick <- reactive({bigPicker(input$words)})
        output$pick <- renderText({
            input$goButton
            isolate(pick())
            })
        output$sentence <- renderText({paste(input$words, "...", sep = "")})
    }
)

