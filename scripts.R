#This file is for testing our scripts before putting them in the shiny app
#Imports
library(ggplot2)
library(plyr)
library(readxl)
donnéesQuestionnaire <- read_excel("donneesVacances.xlsx", sheet="SurveyData_20170220_15h18")

# Pourcentage des gens qui partent ou non en vacances
# Problème au niveau de l'affichage
vacances <-data.frame(count=prop.table(table(vacances$`Q84 [1]`)), rep=c("Non","Oui"))
ggplot(vacances, aes(x = factor(1), y=data$count.Freq,fill=factor(data$rep)) ) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") + ylab("") + xlab("") + ggtitle("Partez-vous en vacances ? ") + guides(fill=guide_legend(title="Réponse"))+ theme(axis.ticks=element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + geom_label(aes(label=paste(round(data$count.Freq,1),"%"),x=c(1,1),y=c(0,15)),inherit.aes = TRUE, show.legend = FALSE)
vacances <-data.frame(count=prop.table(table(vacances$`Q87 [1]`)), rep=c("Non","Oui"))
bar <- ggplot(vacances, aes(x = factor(1), fill = factor(modevacances)))+ geom_bar(width = 1) + ggtitle("Partez-vous en vacances?") +
  xlab(" ") + ylab(" ") +   theme(plot.title = element_text(hjust = 0.5))



# Rapport distance/situation
matrice<-prop.table( table(donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q97 [1]`),2)*100
barplot(matrice,main="Situation personelle par apport \n à la distance effectuée en vacances",beside=FALSE, col=c("#00FFFF","#00FF80","#FFFF00","#FF0000", "#FF8000", "#000000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
barplot(matrice,main="Situation personelle par apport \n à la distance effectuée en vacances",ylab="Pourcentage par enfant",beside=TRUE, col=c("#00FFFF","#00FF80","#FFFF00","#FF0000", "#FF8000", "#000000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("Autre", "En couple avec enfant(s)", "En couple sans enfant", "NRP", "Seul(e) avec enfants", " Seul(e) sans enfant "),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000", "#FF8000", "#000000"),bty="n")
matrice



# Rapport distance/enfants
matrice<-prop.table( table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q97 [1]`),2)*100
matrice[4,] <- colSums(matrice[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),] # Supprime les lignes 4,5,6
barplot(matrice,main="Pourcentage du nombre d'enfants par distance",ylab="Pourcentage par enfant",beside=FALSE, col=c( "#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
barplot(matrice,main="Pourcentage du nombre d'enfants par distance",ylab="Pourcentage par enfant",beside=TRUE, col=c( "#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),bty="n")
matrice

