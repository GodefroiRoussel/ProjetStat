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
matrice <- matrice[-(1),] # Supprime autre
matrice <- matrice[-(3),] # Supprime les non réponses
matrice[1,] <- colSums(matrice[1:2,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(2),] # Supprime les non réponses
matrice[2,] <- colSums(matrice[2:3,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(3),] # Supprime les non réponses
matrice
colnames(matrice) <- c("Moins de 100 km", "Entre 100 et 500 km", "Entre 500 et 1000 km", "Plus de 1 000 km")
barplot(matrice,main="Situation personnelle par apport \n à la distance effectuée en vacances",beside=FALSE, col=c("#00FFFF","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
barplot(matrice,main="Situation personnelle par apport \n à la distance effectuée en vacances",ylab="Pourcentage par enfant",beside=TRUE, col=c("#00FFFF","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants") 
legend(x="topleft",legend=c("En couple", "Seul(e)"),cex=1,fill=c("#00FFFF","#FF0000"),bty="n")
matrice



# Rapport distance/enfants
matrice<-prop.table( table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q97 [1]`),2)*100
matrice[4,] <- colSums(matrice[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),] # Supprime les lignes 4,5,6
colnames(matrice) <- c("Moins de 100 km", "Entre 100 et 500 km", "Entre 500 et 1000 km", "Plus de 1 000 km")
barplot(matrice,main="Nombre d'enfants par apport \n à la distance effectuée en vacances",ylab="Pourcentage par enfant",beside=FALSE, col=c( "#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
barplot(matrice,main=" Nombre d'enfants par apport \n à la distance effectuée en vacances",ylab="Pourcentage par enfant",beside=TRUE, col=c( "#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),bty="n")
matrice

# départ en vacances avec qui ?
matrice <- donnéesQuestionnaire$`Q87 [1]`
matrice <- table(matrice[matrice %in% c("1","2","3","4","5")])
matrice<-prop.table(matrice)*100
names(matrice) <- c("en couple sans enfant","en couple avec vos enfants","avec des amis","en famille (parents, beaux-parents, frères, sœurs, ...)","en solitaire")
matrice <- data.frame(matrice)
ggplot(matrice, aes(x = factor(1), y=matrice$Freq, fill=factor(matrice$Var1)) ) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") + theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) + labs(title="Avec qui partez vous en vacances ?", x="", y="", fill="")+ geom_label(
    aes(y = matrice$Freq, label = paste(round(matrice$Freq,1), " %")), 
    hjust = c(1.2,0.5,-0.8,-0.5,1.1), size = 2, show.legend = FALSE
  )

# départ en vacances Oui/Non
matrice <- donnéesQuestionnaire$`Q87 [1]`
matrice <- prop.table(table(donnéesQuestionnaire$`Q84 [1]`))
names(matrice) <- c("Non","Oui")
matrice <- data.frame(matrice)
matrice$Freq <- matrice$Freq*100
ggplot(matrice, aes(x = factor(1), y=matrice$Freq, fill=factor(matrice$Var1)) ) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") + theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) + labs(title="Partez vous en vacances ?", x="", y="", fill="")+ geom_label(
    aes(y = matrice$Freq, label = paste(round(matrice$Freq,1), " %")), 
    x = c(1,0), y=c(0,0), size = 2, show.legend = FALSE
)
