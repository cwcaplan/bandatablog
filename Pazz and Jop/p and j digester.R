setwd("C:/Users/Seth/Documents/bandatablog/Pazz and Jop")

#year <- as.character(as.numeric(year)+1)
year <- "1994"
#load data
pj <- read.delim(paste("pazz and jop ", year, ".txt", sep=""), 
                       header=F, stringsAsFactors=F)

pj <- list()
pow <- data.frame()
pjM <- data.frame()
year <- "1974"
for (i in 1:34) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    pj[[i]] <- read.delim(paste("pazz and jop ", year, ".txt", sep=""), 
                           header=F, stringsAsFactors=F)
    #deal with asterisks
    pj[[i]] <- pj[[i]][,1:4]
    pj[[i]]$year <- year
    pjM <- rbind(pjM, PJ[[i]])
}
#write.csv(wnoM, "weekly number ones.csv", row.names=F)


### FAILED ATTEMPT TO DEBUG
pj <- list()
pow <- data.frame()
pjM <- data.frame()
year <- "1974"
for (i in 1:34) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    pj[[i]] <- read.delim(paste("pazz and jop ", year, ".txt", sep=""), 
                          header=F, stringsAsFactors=F)
    #deal with asterisks
    #doesn't work #pj[[i]] <- pj[[i]][,1:4]
    YEAR <- rep(year, nrow(pj[[i]]))
    pow <- cbind(YEAR, pj[[i]])
    pow <- pow[,1:5]
    #pj[[i]]$year <- year
    pjM <- rbind(pjM, pow)
}
#write.csv(wnoM, "weekly number ones.csv", row.names=F)
