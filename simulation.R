library(ggplot2)
library(reshape2)

# Our program parameters
popsize <- 10000
panelsize <- 5 # it was increased from 4 in 1998
nshows <- 20 # weekly show in one year

# UK Statistics, gleaned from Wikipedia and the Guardian 20150508
ukethnic <- c(87.1,12.9)
parliamentethnic <- c(590,42)
ethnicities <- c("White", "BAME")
# according to https://en.wikipedia.org/wiki/Ethnic_groups_in_the_United_Kingdom
uksexes <- c(31.029,32.153)
parliamentsexes <- c(459,191)
sexes <- c("Male","Female")


createpopulation <- function(ethnicprob,sexesprob) {
  data.frame(Ethnicity = sample(ethnicities, replace=T, prob=ethnicprob, size=popsize),
             Sex = sample(sexes, replace=T, prob=sexesprob, size=popsize))
}

createseason <- function(populous, shows=nshows) {
  subset <- populous[sample(nrow(populous),panelsize*shows), ]
  subset$number <- seq(1:nrow(subset))-1
  subset$Participant <- subset$number%%panelsize + 1
  subset$Panel <- floor(subset$number/panelsize) + 1
  subset
}

makeplot <- function(subset, shows=nshows, tknfrm="") {
  chart <- ggplot(subset, 
                  aes(x=Participant,
                      y=Panel,
                      shape=Sex,
                      fill=Ethnicity,
                      colour=Ethnicity)) + 
    geom_point(size=9) +
    #    scale_shape_manual(values=c("Male"=25,"Female"=24)) +
    scale_shape_manual(values=c("Male"="\u2642", "Female"="\u2640")) + 
    scale_colour_manual(values=c("BAME"="#c39e73","White"="#f8e0a1")) + 
    scale_fill_manual(values=c("BAME"="#c39e73","White"="#f8e0a1")) + 
    ggtitle(paste(shows,"Simulated Question Time Panels sampled from",tknfrm)) +
    theme_bw()
  chart
}

allwhitepercent <- function(subset, shows) {
allwhite <- 0
  for(i in 1:max(subset$Panel)) {
    allwhite <- allwhite + as.numeric(sum(as.numeric(subset[subset$Panel==i,]$Ethnicity))==10)
  }
100*allwhite/max(subset$Panel)
}

pop <- createpopulation(ukethnic,uksexes)
populous <- createpopulation(parliamentethnic,parliamentsexes)
uksubset <- createseason(pop,nshows)
hpsubset <- createseason(populous,nshows)

simulation <- function(population, n=1000) {
  avgwhite<-0
  avgmale <-0
  for(j in 1:n){
    allwhite <- c()
    allmale <- c()
    subset <- createseason(population)
    for(i in 1:nshows) allwhite <- c(allwhite,sum(as.numeric(subset[subset$Panel==i,]$Ethnicity)))
    for(i in 1:nshows) allmale <- c(allmale,sum(as.numeric(subset[subset$Panel==i,]$Sex)))
    avgmale <- avgmale + sum(allmale==10)/nshows
    avgwhite <- avgwhite + sum(allwhite==10)/nshows
  }
  data.frame(AllMale=avgmale/n,AllWhite=avgwhite/n)
}

compareplot <- function() {
  try <- cbind(Population=c("UK","Parliament","Question Time"),
               rbind(UK=simulation(pop,1000)*100,
                     Parliament=simulation(populous,1000)*100,
                     QuestionTime=c(0.005,0.61)*100))
  
  comparison <- ggplot(melt(try), aes(fill=Population, x=variable, y=value)) +  theme_bw() + geom_bar(stat='identity', position="dodge") + scale_y_continuous(limits=c(0,100))
  print(comparison+ labs(title="Question Time versus simulations", x="", y="percentage"))
}
