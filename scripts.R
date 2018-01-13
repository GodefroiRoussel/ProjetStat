#This file is for testing our scripts before putting them in the shiny app
#Imports
library(ggplot2)
library(plyr)
install.packages("dplyr")
library(readxl)
library(FactoMineR)
library("gplots2")
donnéesQuestionnaire <- read_excel("donneesVacances.xlsx", sheet="SurveyData_20170220_15h18")

# Pourcentage des gens qui partent ou non en vacances
# Problème au niveau de l'affichage
vacances <-data.frame(count=prop.table(table(donnéesQuestionnaire$`Q84 [1]`)), rep=c("Non","Oui"))
ggplot(vacances, aes(x = factor(1), y=data$count.Freq,fill=factor(data$rep)) ) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") + ylab("") + xlab("") + ggtitle("Partez-vous en vacances ? ") + guides(fill=guide_legend(title="Réponse"))+ theme(axis.ticks=element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + geom_label(aes(label=paste(round(data$count.Freq,1),"%"),x=c(1,1),y=c(0,15)),inherit.aes = TRUE, show.legend = FALSE)
donnéesQuestionnaire <-data.frame(count=prop.table(table(donnéesQuestionnaire$`Q87 [1]`)), rep=c("Non","Oui"))
bar <- ggplot(donnéesQuestionnaire, aes(x = factor(1), fill = factor(modevacances)))+ geom_bar(width = 1) + ggtitle("Partez-vous en vacances?") +
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

# Rapport matériel de dépannage/enfants
matrice<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q119 [1]`)
matrice[4,] <- colSums(matrice[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),] # Supprime les lignes 5,6,7
matrice <-aperm(matrice) # transposition de la matrice
rownames(matrice)
matrice[1,] <- colSums(matrice[1:21,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(2:21),] # Supprime les lignes 5 à 21
matrice <- aperm(matrice)
colnames(matrice) <- c("Oui", "Non")
matrice<-prop.table(matrice,1)*100
barplot(matrice,main="Nombre de personnes ne prévoyant pas \n de matériel de dépannage en vacances \n par rapport au nombre d'enfants",ylab="Pourcentage par enfant",beside=TRUE, col=c( "#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),bty="n")
matrice

# Rapport matériel de dépannage/enfants
matrice1<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q119 [1]`)
matrice1[4,] <- colSums(matrice1[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice1 <- matrice1[-(5:7),] # Supprime les lignes 5,6,7
matrice1 <- matrice1[,-(5:21)] # Supprime les lignes Autres
a <- array(c(0,0,0,0))
a <- aperm(a)
matrice1 <- aperm(matrice1)
matrice1 <- rbind(matrice1[1:2,],a,matrice1[3:5,])
matrice1 <- aperm(matrice1)
colnames(matrice1) <- c("Bombe anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
                       "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Pas de materiel")
rownames(matrice1) <- c("0","1","2","3+")

matrice2<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q119 [2]`)
matrice2[4,] <- colSums(matrice2[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice2 <- matrice2[-(5:7),] # Supprime les lignes 5,6,7
matrice2 <- matrice2[,-(5:30)] # Supprime les lignes Autres
a <- array(c(0,0,0,0))
a <- aperm(a)
matrice2 <- aperm(matrice2)
matrice2 <- rbind(a,matrice2,a)
matrice2 <- aperm(matrice2)
colnames(matrice2) <- c("Bombe anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
                        "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Pas de materiel")
rownames(matrice2) <- c("0","1","2","3+")

matrice3<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q119 [3]`)
matrice3[4,] <- colSums(matrice3[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice3 <- matrice3[-(5:7),] # Supprime les lignes 5,6,7
matrice3 <- matrice3[,-(4:47)] # Supprime les lignes Autres
a <- array(c(0,0,0,0))
a <- aperm(a)
matrice3 <- aperm(matrice3)
matrice3 <- rbind(a,a,matrice3,a)
matrice3 <- aperm(matrice3)
colnames(matrice3) <- c("Bombe anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
                        "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Pas de materiel")
rownames(matrice3) <- c("0","1","2","3+")

matrice4<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q119 [4]`)
matrice4[4,] <- colSums(matrice4[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice4 <- matrice4[-(5:7),] # Supprime les lignes 5,6,7
matrice4 <- matrice4[,-(3:29)] # Supprime les lignes Autres
a <- array(c(0,0,0,0))
a <- aperm(a)
matrice4 <- aperm(matrice4)
matrice4 <- rbind(a,a,a,matrice4,a)
matrice4 <- aperm(matrice4)
colnames(matrice4) <- c("Bombe anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
                        "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Pas de materiel")
rownames(matrice4) <- c("0","1","2","3+")
AFCMatrix<- matrice1+matrice2+matrice3+matrice4
colnames(AFCMatrix) <- c("a","b","c","d","e","f")

library(factoextra)
res.ca <- CA(AFCMatrix)
print(res.ca)
fviz_ca_biplot(res.ca)
barplot(matrice,main="Nombre de personnes ne prévoyant pas \n de matériel de dépannage en vacances \n par rapport au nombre d'enfants",ylab="Pourcentage par enfant",beside=TRUE, col=c( "#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),bty="n")
matrice

# Rapport vérif assurance/enfants
matrice<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q122 [1]`)
matrice[4,] <- colSums(matrice[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),] # Supprime les lignes 5,6,7
matrice<-prop.table(matrice,1)*100 #pourçentage sur les lignes
colnames(matrice) <- c("Oui", "Non")
barplot(matrice,main="Proportion des personnes vérifiants \n leur assurance 2-roues avant de partir \n par rapport au nombre d'enfants",ylab="Pourcentage par enfant",beside=TRUE, col=c( "#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),bty="n")
matrice

# Rapport révision/enfants
matrice<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q120 [1]`)
matrice[4,] <- colSums(matrice[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),] # Supprime les lignes 5,6,7
matrice<-aperm(matrice)# transposition de la matrice
matrice[1,] <- colSums(matrice[1:3,], na.rm = FALSE, dims = 1) #Regroupe les réponses en Oui ou Non
matrice<-matrice[-(2:3),] # Supprime les lignes 2,3
matrice<-aperm(matrice) # transposition de la matrice
matrice<-prop.table(matrice,1)*100 #pourçentage sur les lignes
colnames(matrice) <- c("Oui", "Non")
barplot(matrice,main="Proportion des personnes effectuant \n une révision de leur 2-roues avant leur départ \n par rapport au nombre d'enfants",ylab="Pourcentage par enfant",beside=TRUE, col=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfants", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),bty="n")
matrice

#Rapport révision/enfant pour départ sans enfants
matrice<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q87 [1]`,donnéesQuestionnaire$`Q120 [1]`)
matrice[4,,] <- colSums(matrice[4:7,,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),,] # Supprime les lignes 5,6,7
matrice <- matrice[,-(2:47),] # Supprime les lignes 5,6,7
matrice<-aperm(matrice)# transposition de la matrice
matrice[1,] <- colSums(matrice[1:3,], na.rm = FALSE, dims = 1) #Regroupe les réponses en Oui ou Non
matrice<-matrice[-(2:3),] # Supprime les lignes 2,3
matrice<-aperm(matrice) # transposition de la matrice
matrice<-prop.table(matrice,1)*100
colnames(matrice) <- c("Oui", "Non")
barplot(matrice,main="Proportion des personnes effectuant \n une révision de leur 2-roues avant leur départ \n sans enfants par rapport au nombre d'enfants",ylab="Pourcentage par enfant",beside=TRUE, col=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfants", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),bty="n")
matrice

#Rapport révision/enfant pour départ avec enfants
matrice<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q87 [1]`,donnéesQuestionnaire$`Q120 [1]`)
matrice[4,,] <- colSums(matrice[4:7,,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),,] # Supprime les lignes 5,6,7
matrice <- matrice[-1,,]
matrice <- matrice[,-(3:47),] # Supprime les lignes 3,47 
matrice <- matrice[,-1,]
matrice<-aperm(matrice)# transposition de la matrice
matrice[1,] <- colSums(matrice[1:3,], na.rm = FALSE, dims = 1) #Regroupe les réponses en Oui ou Non
matrice<-matrice[-(2:3),] # Supprime les lignes 2,3
matrice<-aperm(matrice) # transposition de la matrice
matrice<-prop.table(matrice,1)*100
colnames(matrice) <- c("Oui", "Non")
barplot(matrice,main="Proportion des personnes effectuant \n une révision de leur 2-roues avant leur départ avec \n leur enfants par rapport au nombre d'enfants",ylab="Pourcentage par enfant",beside=TRUE, col=c("#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FF80","#FFFF00","#FF0000"),bty="n")
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

# nombre d'enfants parmi les gens qui partent en vacances
matrice <- table(donnéesQuestionnaire$`Q12 [1]`, donnéesQuestionnaire$`Q84 [1]`)
matrice[4,] <- colSums(matrice[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),] # Supprime les lignes 5,6,7
matrice <- matrice[,-1]
matrice <- prop.table(matrice)*100
names(matrice) <- c("0","1","2","3+")
barplot(matrice,main="Proportion des personnes partant \n en vacances suivant leur nombre \n d'enfants",ylab="Pourcentage par enfant",beside=TRUE, col=c( "#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),bty="n")
matrice

# situation:prudence 
# TODO: supprimer la colonne NRP
matrice<- table(donnéesQuestionnaire$`Q117 [1]`, donnéesQuestionnaire$`Q11 [1]`)
rownames(matrice) <- c("moins attentif", "plus attentif", "ne change pas")
matrice <- prop.table(matrice)*100
matrice
barplot(matrice, beside = TRUE)

# situation:centre_interet 
# TODO: supprimer la colonne NRP
# Garder les personnes avec / sans enfants ? 
matrice<- table(donnéesQuestionnaire$`Q103 [1]`, donnéesQuestionnaire$`Q11 [1]`)
matrice<-matrice[1:4,c(2,3,5,6)]
rownames(matrice) <- c("Tourisme culturel", "Belles routes, paysages,", "Rassemblements, manifestations sportives","Rendre visite à des amis ou de la famille")
matrice <- prop.table(matrice,2)*100
barplot(matrice,main="Centre d'intêret en fonction de la situation",ylab="Pourcentages %",beside=TRUE, col=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),ylim=c(0,70), lwd=2, xlab="Situation",las=1)
legend(x="topright",legend=c("Tourisme culturel", "Belles routes, paysages", "Rassemblements, manifestations sportives", "Rendre visite à des amis ou de la famille"),cex=1,fill=c("#00FFFF","#00FF80","#FFFF00","#FF0000"),bty="n")

