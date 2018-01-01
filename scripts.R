#This file is for testing our scripts before putting them in the shiny app
library(ggplot2)
library(plyr)
library(readxl)
donnéesQuestionnaire <- read_excel("donneesVacances.xlsx", sheet="SurveyData_20170220_15h18")

vacances <-data.frame(count=prop.table(table(vacances$`Q84 [1]`)), rep=c("Non","Oui"))
ggplot(vacances, aes(x = factor(1), y=data$count.Freq,fill=factor(data$rep)) ) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") + ylab("") + xlab("") + ggtitle("Partez-vous en vacances ? ") + guides(fill=guide_legend(title="Réponse"))+ theme(axis.ticks=element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + geom_label(aes(label=paste(round(data$count.Freq,1),"%"),x=c(1,1),y=c(0,15)),inherit.aes = TRUE, show.legend = FALSE)
vacances <-data.frame(count=prop.table(table(vacances$`Q87 [1]`)), rep=c("Non","Oui"))
bar <- ggplot(vacances, aes(x = factor(1), fill = factor(modevacances)))+ geom_bar(width = 1) + ggtitle("Partez-vous en vacances?") +
  xlab(" ") + ylab(" ") +   theme(plot.title = element_text(hjust = 0.5))

matrice<-prop.table(table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q97 [1]`),2)*100
barplot(matrice,main="Evolution de la perception sur l'hygiène de vie",ylab="Effectif",beside=TRUE, col=c("#A52422","#A4BAB7"),ylim=c(0,210), lwd=2, xlab="Note /6",las=1)
legend(x="topleft",legend=c("Avant", "Après"),cex=1,fill=c("#A52422","#A4BAB7"),bty="n")


