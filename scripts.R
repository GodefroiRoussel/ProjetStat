#This file is for testing our scripts before putting them in the shiny app
#Imports
install.packages("dplyr")
install.packages(factoextra)
install.packages("gplots")
library("gplots")
library(ggplot2)
library(factoextra)
library(plyr)
library(readxl)
library(FactoMineR)
donnéesQuestionnaire <- read_excel("donneesVacances.xlsx", sheet="SurveyData_20170220_15h18")

# Pourcentage des gens qui partent ou non en vacances
# Problème au niveau de l'affichage
vacances <-data.frame(prop.table(table(donnéesQuestionnaire$`Q84 [1]`))*100, rep=c("Non","Oui"))
ggplot(vacances, aes(x = factor(1), y=vacances$Freq,fill=factor(vacances$rep)) ) + geom_bar(width = 1,stat="identity")+
  ylab("") + xlab("") + ggtitle("Partez-vous en vacances ? ") + 
  guides(fill=guide_legend(title="Réponse"))+ theme(axis.ticks=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label(aes(label=paste(round(vacances$Freq,1),"%"),x=c(1,0),y=c(0,0)),inherit.aes = TRUE, show.legend = FALSE)+
  scale_fill_manual(values=c("#4A83D9", "#E68527"))+coord_polar(theta = "y")


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
barplot(matrice,main="Situation personnelle par rapport \n à la distance effectuée en vacances",beside=FALSE, col=c("#4A83DA","#CA66BF"),ylim=c(0,100), lwd=2, xlab="Distance effectuée")
barplot(matrice,main="Situation personnelle par rapport \n à la distance effectuée en vacances",ylab="Pourcentage",beside=TRUE, col=c("#4A83DA","#CA66BF"),ylim=c(0,100), lwd=2, xlab="Distance effectuée") 
legend(x="topright",legend=c("En couple", "Seul(e)"),cex=1,fill=c("#4A83DA","#CA66BF"),bty="n")
matrice



# Rapport distance/enfants
matrice<-prop.table( table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q97 [1]`),2)*100
matrice[4,] <- colSums(matrice[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),] # Supprime les lignes 4,5,6
colnames(matrice) <- c("Moins de 100 km", "Entre 100 et 500 km", "Entre 500 et 1000 km", "Plus de 1 000 km")
barplot(matrice,main="Nombre d'enfants par rapport \n à la distance effectuée en vacances",ylab="Pourcentage",beside=FALSE, col=c( "#4A83DA","#00FF80","#E68527","#CA66BF"),ylim=c(0,70), lwd=2, xlab="Distance effectuée")
barplot(matrice,main=" Nombre d'enfants par rapport \n à la distance effectuée en vacances",ylab="Pourcentage",beside=TRUE, col=c( "#4A83DA","#00FF80","#E68527","#CA66BF"),ylim=c(0,70), lwd=2, xlab="Distance effectuée")
legend(x="topleft",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#4A83DA","#00FF80","#E68527","#CA66BF"),bty="n")
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
barplot(matrice,main="Nombre de personnes ne prévoyant pas \n de matériel de dépannage en vacances \n par rapport au nombre d'enfants",ylab="Pourcentage",beside=TRUE, col=c( "#4A83DA","#00FF80","#E68527","#CA66BF"),ylim=c(0,100), lwd=2, xlab="Réponse")
  legend(x="topright",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#4A83DA","#00FF80","#E68527","#CA66BF"),bty="n")
matrice

# AFC LORIS
# Rapport matériel de dépannage/situation familiale lors du départ (possibilité de faire avec le nb d'enfants et la situation familiale actuelle)
matrice1<-table(donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q119 [1]`)
#matrice1[4,] <- colSums(matrice1[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
#matrice1 <- matrice1[-(5:7),] # Supprime les lignes 5,6,7
matrice1 <- matrice1[-c(1,4),]
#matrice1 <- matrice1[-(6:47),]
matrice1 <- aperm(matrice1)
matrice1[1,] <- colSums(matrice1[c(1,10),])
matrice1[7,] <- colSums(matrice1[c(7,11:15,17,18,21),])
matrice1 <- matrice1[-c(5,6,8:21),] # Supprime les lignes Autres
matrice1 <- aperm(matrice1)
a <- array(c(0,0,0,0,0))
a <- aperm(a)
matrice1 <- aperm(matrice1)
matrice1 <- rbind(matrice1[1:2,],a,matrice1[3:6,])
matrice1 <- aperm(matrice1)
colnames(matrice1) <- c("Kit anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
                        "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Kit reparation crevaison pneus","Pas de materiel")
#rownames(matrice1) <- c("0","1","2","3+")

matrice2<-table(donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q119 [2]`)
#matrice2[4,] <- colSums(matrice2[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
#matrice2 <- matrice2[-(5:7),] # Supprime les lignes 5,6,7
matrice2 <- matrice2[-c(1,4),]
#matrice2 <- matrice2[-(6:47),]
matrice2 <- aperm(matrice2)
matrice2[1,] <- colSums(matrice2[c(1,12,13,14),])#colonne categorie kit anti crevaison
matrice2[8,] <- colSums(matrice2[c(8,15:27),])#colonne categorie kit repartion crevaison
matrice2 <- matrice2[-c(5:7,9:30),] # Supprime les lignes Autres
matrice2 <- aperm(matrice2)
a <- array(c(0,0,0,0,0))
a <- aperm(a)
matrice2 <- aperm(matrice2)
matrice2 <- rbind(a,matrice2[1:5,],a)
matrice2 <- aperm(matrice2)
colnames(matrice2) <- c("Kit anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
                        "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Kit reparation crevaison pneus","Pas de materiel")
#rownames(matrice2) <- c("0","1","2","3+")

matrice3<-table(donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q119 [3]`)
#matrice3[4,] <- colSums(matrice3[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
#matrice3 <- matrice3[-(5:7),] # Supprime les lignes 5,6,7
matrice3 <- matrice3[-c(1,4),]
#matrice3 <- matrice3[-(6:47),]
matrice3 <- aperm(matrice3)
a <- array(c(0,0,0,0,0))
a <- aperm(a)
matrice3 <- rbind(a,a,matrice3,a)
matrice3[1,] <- colSums(matrice3[c(1,16,17,40),])#colonne categorie kit anti crevaison
matrice3[18,] <- colSums(matrice3[c(18:35,39,41:43),])#colonne categorie kit repartion crevaison
matrice3 <- matrice3[-c(6:16,17,19:49),] # Supprime les lignes Autres
matrice3 <- aperm(matrice3)
colnames(matrice3) <- c("Kit anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
                        "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Kit reparation crevaison pneus","Pas de materiel")
#rownames(matrice3) <- c("0","1","2","3+")

matrice4<-table(donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q119 [4]`)
#matrice4[4,] <- colSums(matrice4[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
#matrice4 <- matrice4[-(5:7),] # Supprime les lignes 5,6,7
matrice4 <- matrice4[-c(1,4),]
#matrice4 <- matrice4[-(6:47),]
a <- array(c(0,0,0,0,0))
a <- aperm(a)
matrice4 <- aperm(matrice4)
matrice4 <- rbind(a,a,a,matrice4,a)
matrice4[1,] <- colSums(matrice4[(14:16),])
matrice4[14,] <- colSums(matrice4[c(16:18,25),])
matrice4 <- matrice4[-c(6:13,15:32),]# Supprime les lignes Autres
matrice4 <- aperm(matrice4)
colnames(matrice4) <- c("Kit anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
                        "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Kit reparation crevaison pneus","Pas de materiel")
rownames(matrice4) <- c("0","1","2","3+")
AFCMatrix<- matrice1+matrice2+matrice3+matrice4
#colnames(AFCMatrix) <- c("a","b","c","d","e","f","g")
rownames(AFCMatrix) <- c("0","1","2","3+")
#rownames(AFCMatrix) <- c("En couple sans enfant","En couple avec vos enfants","Avec des amis",
#                        "En famille","En solitaire")

AFCMatrix<-aperm(AFCMatrix)
library(factoextra)
chisq.test(AFCMatrix)
res.ca <- CA(AFCMatrix)
print(res.ca)
fviz_ca_biplot(res.ca)
barplot(matrice,main="Nombre de personnes ne prévoyant pas \n de matériel de dépannage en vacances \n par rapport au nombre d'enfants",ylab="Pourcentage",beside=TRUE, col=c( "#4A83DA","#00FF80","#E68527","#CA66BF"),ylim=c(0,100), lwd=2, xlab="Réponse")
legend(x="topleft",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#4A83DA","#00FF80","#E68527","#CA66BF"),bty="n")
matrice

#AFC




matriceACM<-rbind(donnéesQuestionnaire$`Q119 [1]`,donnéesQuestionnaire$`Q119 [2]`,donnéesQuestionnaire$`Q119 [3]`,
donnéesQuestionnaire$`Q119 [4]`,donnéesQuestionnaire$`Q119 [5]`,donnéesQuestionnaire$`Q119 [6]`)
matriceACM<-aperm(matriceACM)
matriceACMtrie<-matriceACM

for(i in 1:1372){
  for(j in 1:6){
    if(is.na(matriceACM[i,j]) | substring(matriceACM[i,j],1,3)=='AUT'){
        matriceACMtrie[i,j]<-0
    } else{
        if(substring(matriceACM[i,j],1,3)=='NSP'){
          matriceACMtrie[i,j]<-0
          matriceACMtrie[i,6]<-1
        }
        else{
          matriceACMtrie[i,j]<-0
          matriceACMtrie[i,as.integer(matriceACM[i,j])]<-1
        }
    }
  }
}
matriceACMtrie<-aperm(matriceACMtrie)
rownames(matriceACMtrie) <- c("Kit anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
                              "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Pas de materiel")

res.mca <- MCA(matriceACMtrie, ncp = 5, graph = TRUE)
fviz_mca_biplot(res.mca)
matriceSituation<-rbind(donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q11 [1]`)
matriceSituation<-aperm(matriceSituation)
matriceSituationTrie<-matriceSituation
for(j in 1:1372){
  matriceSituationTrie[j,1]<-0
  matriceSituationTrie[j,2]<-0
  matriceSituationTrie[j,3]<-0
  matriceSituationTrie[j,4]<-0
  matriceSituationTrie[j,5]<-0
  switch(matriceSituation[j,1], 
         "Seul(e) sans enfant"={
           matriceSituationTrie[j,1]<-1
         },
         "Seul(e) avec enfants"={
           matriceSituationTrie[j,2]<-1
         },
         "En couple sans enfant"={
           matriceSituationTrie[j,3]<-1
         },
         "En couple avec enfant(s)"={
           matriceSituationTrie[j,4]<-1
         },
         "Autre"={
           matriceSituationTrie[j,5]<-1
         }
  )
}
matriceACMtrie <- aperm(matriceACMtrie)
matriceACMtrie<-cbind(matriceACMtrie,matriceSituationTrie)
colnames(matriceACMtrie) <- c("Kit anti-crevaison", "Huile de moteur / graisse pour la chaine", 
                              "Eau distillées pour batterie",
                              "Outils divers (clefs/tournevis/bougies...",
                              "Nanomètre pour contrôler la préssion des pneus",
                              "Pas de materiel","Seul(e) sans enfant","Seul(e) avec enfants",
                              "En couple sans enfant", "En couple avec enfant(s)","Autre")
res.mca <- MCA(matriceACMtrie, quali.sup=7:11,ncp = 5, graph = TRUE)
fviz_mca_biplot(res.mca)

# Rapport vérif assurance/enfants
matrice<-table(donnéesQuestionnaire$`Q12 [1]`,donnéesQuestionnaire$`Q122 [1]`)
matrice[4,] <- colSums(matrice[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),] # Supprime les lignes 5,6,7
matrice<-prop.table(matrice,1)*100 #pourçentage sur les lignes
colnames(matrice) <- c("Oui", "Non")
barplot(matrice,main="Proportion des personnes vérifiants \n leur assurance 2-roues avant de partir \n par rapport au nombre d'enfants",ylab="Pourcentage",beside=TRUE, col=c( "#4A83DA","#00FF80","#E68527","#CA66BF"),ylim=c(0,100), lwd=2, xlab="Réponse")
legend(x="topleft",legend=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#4A83DA","#00FF80","#E68527","#CA66BF"),bty="n")
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
barplot(matrice,main="Proportion des personnes effectuant \n une révision de leur 2-roues avant leur départ \n par rapport au nombre d'enfants",ylab="Pourcentage",beside=TRUE, col=c("#4A83DA","#00FF80","#E68527","#CA66BF"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfants", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#4A83DA","#00FF80","#E68527","#CA66BF"),bty="n")
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
barplot(matrice,main="Proportion des personnes effectuant \n une révision de leur 2-roues avant leur départ \n sans enfants par rapport au nombre d'enfants",ylab="Pourcentage",beside=TRUE, col=c("#4A83DA","#00FF80","#E68527","#CA66BF"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("0 enfants", "1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#4A83DA","#00FF80","#E68527","#CA66BF"),bty="n")
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
barplot(matrice,main="Proportion des personnes effectuant \n une révision de leur 2-roues avant leur départ avec \n leur enfants par rapport au nombre d'enfants",ylab="Pourcentage",beside=TRUE, col=c("#00FF80","#E68527","#CA66BF"),ylim=c(0,100), lwd=2, xlab="Nombre d'enfants")
legend(x="topleft",legend=c("1 enfant", "2 enfants", "3 enfants et +"),cex=1,fill=c("#00FF80","#E68527","#CA66BF"),bty="n")
matrice


# départ en vacances avec qui ?
matrice <- donnéesQuestionnaire$`Q87 [1]`
matrice <- table(matrice[matrice %in% c("1","2","3","4","5")])
matrice<-prop.table(matrice)*100
names(matrice) <- c("en couple sans enfant","en couple avec vos enfants","avec des amis","en famille (parents, beaux-parents, frères, sœurs, ...)","en solitaire")
matrice <- data.frame(matrice)
ggplot(matrice, aes(x = factor(1), y=matrice$Freq, fill=factor(matrice$Var1)) ) + geom_bar(width = 1,stat="identity")+theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) + labs(title="Avec qui partez vous en vacances ?", x="", y="", fill="")+ geom_label(
    aes(y = matrice$Freq, label = paste(round(matrice$Freq,1), " %")), 
    hjust = c(1.2,0.5,-0.8,-0.5,1.1), size = 2, show.legend = FALSE
  )+ 
  scale_fill_manual(values=c("#4A83D9", "#69B940", "#CA66BF","#8C5C98","#E68527"))+coord_polar(theta = "y")

# départ en vacances Oui/Non
matrice <- donnéesQuestionnaire$`Q87 [1]`
matrice <- prop.table(table(donnéesQuestionnaire$`Q84 [1]`))
names(matrice) <- c("Non","Oui")
matrice <- data.frame(matrice)
matrice$Freq <- matrice$Freq*100
ggplot(matrice, aes(x = factor(1), y=matrice$Freq, fill=factor(matrice$Var1)) ) + geom_bar(width = 1,stat="identity")+ theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) + labs(title="Partez vous en vacances ?", x="", y="", fill="")+ geom_label(
    aes(y = matrice$Freq, label = paste(round(matrice$Freq,1), " %")), 
    x = c(1,0), y=c(0,0), size = 2, show.legend = FALSE
  )+ 
  scale_fill_manual(values=c("#4A83D9", "#69B940", "#CA66BF","#8C5C98","#E68527"))+coord_polar(theta = "y")

# nombre d'enfants parmi les gens qui partent en vacances
matrice <- table(donnéesQuestionnaire$`Q12 [1]`, donnéesQuestionnaire$`Q84 [1]`,donnéesQuestionnaire$`Q11 [1]`)
matriceSeul<- matrice[,,5] + matrice[,,6] # seul 
matriceCouple<- matrice[,,1] + matrice[,,2] # en couple

matriceSeul[4,2] <- sum(matriceSeul[4:7,2]) #Regroupe les personnes ayant 3,4,5,6 enfants
matriceSeul <- matriceSeul[-(5:7),] # Supprime les lignes 5,6,7
matriceSeul <- matriceSeul[,2]
matriceSeul


matriceCouple[4,2] <- sum(matriceCouple[4:7,2]) #Regroupe les personnes ayant 3,4,5,6 enfants
matriceCouple <- matriceCouple[-(5:7),] # Supprime les lignes 5,6,7
matriceCouple <- matriceCouple[,2]
matriceCouple


matriceCouple <- data.frame(prop.table(matriceCouple)*100)
matriceSeul <- data.frame(prop.table(matriceSeul)*100)

matriceSeul
matriceCouple

rownames(matriceCouple) <- c("0","1","2","3+")
rownames(matriceSeul) <- c("0","1","2","3+")

ggplot(matriceCouple, aes(x = factor(1), y=matriceCouple[,1], fill=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +")) )+ geom_bar(width = 1,stat="identity")+
theme(axis.text = element_blank())+
theme(panel.grid=element_blank()) +
theme(axis.ticks=element_blank()) + labs(title="Personnes en couple partant \n en vacances suivant leur nombre \n d'enfants", x="", y="", fill="Nombre d'enfants")+
geom_label(
    aes(y = matriceCouple[,1], label = paste(round(matriceCouple[,1],1), " %")), 
    x=c(1,1,0,1),y=c(0,-15,0,12), size = 2, show.legend = FALSE
)+ 
  scale_fill_manual(values=c("#4A83D9", "#69B940", "#CA66BF","#8C5C98","#E68527"))+coord_polar(theta = "y")

ggplot(matriceSeul, aes(x = factor(1), y=matriceSeul[,1], fill=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +")) )+ geom_bar(width = 1,stat="identity")+
  theme(axis.text = element_blank())+
  theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) + labs(title="Personnes seules partant \n en vacances suivant leur nombre \n d'enfants", x="", y="", fill="Nombre d'enfants")+
  geom_label(
    aes(y = matriceSeul[,1], label = paste(round(matriceSeul[,1],1), " %")), 
    x=c(0,1,1,1),y=c(0,17,8,-2), size = 2, show.legend = FALSE
  )+ 
  scale_fill_manual(values=c("#4A83D9", "#69B940", "#CA66BF","#8C5C98","#E68527"))+coord_polar(theta = "y")


# situation:prudence 
# TODO: supprimer la colonne NRP
matrice<- table(donnéesQuestionnaire$`Q117 [1]`, donnéesQuestionnaire$`Q11 [1]`)
rownames(matrice) <- c("moins attentif", "plus attentif", "ne change pas")
matrice<-matrice[,-c(1,4)]
matrice <- prop.table(matrice)*100
matrice
barplot(matrice,main="Prudence en fonction de la situation familiale",ylab="Pourcentages %",beside=TRUE, col=c("#4A83D9", "#69B940", "#CA66BF"),ylim=c(0,35), lwd=2, xlab="Situation",las=1)
legend(x="topright",legend=rownames(matrice),cex=1,fill=c("#4A83D9", "#69B940", "#CA66BF"),bty="n")

# situation:centre_interet 
# TODO: supprimer la colonne NRP
# Garder les personnes avec / sans enfants ? 
matrice<- table(donnéesQuestionnaire$`Q103 [1]`, donnéesQuestionnaire$`Q11 [1]`)
matrice<-matrice[1:4,c(2,3,5,6)]
rownames(matrice) <- c("Tourisme culturel", "Belles routes, paysages,", "Rassemblements, manifestations sportives","Rendre visite à des amis ou de la famille")
matrice <- prop.table(matrice,2)*100
barplot(matrice,main="Centre d'intêret en fonction de la situation",ylab="Pourcentages %",beside=TRUE, col=c("#4A83D9", "#69B940", "#CA66BF","#8C5C98"),ylim=c(0,70), lwd=2, xlab="Situation",las=1)
legend(x="topright",legend=c("Tourisme culturel", "Belles routes, paysages", "Rassemblements, manifestations sportives", "Rendre visite à des amis ou de la famille"),cex=1,fill=c("#4A83D9", "#69B940", "#CA66BF","#8C5C98"),bty="n")

# départ en vacances Oui/Non en fonction de s'ils ont des enfants
matrice <- table(donnéesQuestionnaire$`Q11 [1]`,donnéesQuestionnaire$`Q84 [1]`)
matrice <- matrice[-c(1,4),] # Supprime les lignes 1,4 (Correspond à : Autre et NPR)
matrice[1,] <- colSums(matrice[1:2,], na.rm = FALSE, dims = 1) #Regroupe les personnes étant en couple
matrice[3,] <- colSums(matrice[3:4,], na.rm = FALSE, dims = 1) #Regroupe les personnes étant seul(e)
matrice <- matrice[-c(2,4),] # Supprime les lignes 2,4 (Correspond en couple sans enfant et Seul sans enfant)
matrice <- prop.table(matrice)*100 #table de proportion
rownames(matrice) <- c("En couple","Seul(e)")
colnames(matrice) <- c("Non","Oui")
matrice <- data.frame(matrice)
matrice$Freq<-round(matrice$Freq,1)
matrice <- matrice[3:4,]

ggplot(matrice, aes(x = factor(1), y=matrice$Freq, fill=factor(matrice$Var1)) ) + geom_bar(width = 1,stat="identity") + theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) + labs(title="Situation des personnes partant en vacances ?", x="", y="", fill="Situation")+ geom_label(
    aes(y = matrice$Freq, label = paste(round(matrice$Freq,1), " %")), 
    hjust = c(0,1), size = 3, show.legend = FALSE
  )+ 
  scale_fill_manual(values=c("#4A83D9", "#E68527"))+coord_polar(theta = "y")


# nombre d'enfants parmi les gens qui partent en vacances
matrice <- table(donnéesQuestionnaire$`Q12 [1]`, donnéesQuestionnaire$`Q84 [1]`)
matrice[4,] <- colSums(matrice[4:7,], na.rm = FALSE, dims = 1) #Regroupe les personnes ayant 3,4,5,6 enfants
matrice <- matrice[-(5:7),] # Supprime les lignes 5,6,7
matrice <- prop.table(matrice)*100
matrice <- data.frame(matrice)
matrice <- matrice[5:8,]
matrice$Freq <- round(matrice$Freq,1) 

ggplot(matrice, aes(x = factor(1), y=factor(matrice$Freq), fill=c("0 enfant", "1 enfant", "2 enfants", "3 enfants et +")) )+ geom_bar(width = 1,stat="identity")+
  coord_polar(theta = "y") + 
  theme(axis.text = element_blank())+
  theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) + labs(title="Proportion des personnes partant \n en vacances suivant leur nombre \n d'enfants", x="", y="", fill="Nombre d'enfants")+
  geom_label(
    aes(y = matrice$Freq, label = paste(matrice$Freq, " %")), 
    x=c(0,0,0,1),y=c(6,3,1,-0.5), size = 2, show.legend = FALSE
  )+ 
  scale_fill_manual(values=c("#4A83D9", "#69B940", "#CA66BF","#8C5C98","#E68527"))+coord_polar(theta = "y")

  
#ACP MELVIL
df <- data.frame(enfants=donnéesQuestionnaire$`Q12 [1]`, km=donnéesQuestionnaire$`Q97 [1]`, assurance=donnéesQuestionnaire$`Q122 [1]`)
df <- df[complete.cases(df), ]
df
res.pca <- PCA(df, graph = FALSE)
# Créer une variable aléatoire continue de longueur 10
set.seed (123)
my.cont.var <- rnorm (3)
# Colorer les variables en fonction de la variable continue
fviz_pca_var(res.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var",
             repel = TRUE # Évite le chevauchement de texte
             )
res.pca$ind
plot(res.pca)

fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = FALSE # Évite le chevauchement de texte
              )

fr <- data.frame(var = c(10, 4, 5, 3, 3, 7, 2, 6, 2, 8, 5),
                 group = factor(c("c", "a", "b", "a", "b", "b", "a", "b", "a", "c", "c")))#table de données
fit <- aov(var ~ group, df)# analyse de variance
summary(fit)


#Anova Godefroi
# Les effets du mode de vacances sur le nombre d'enfants dans une famille
nbEnfant <- donnéesQuestionnaire$`Q12 [1]` # Valeurs
modeVacance <- as.factor(c(donnéesQuestionnaire$`Q88 [1]`))# groupes

#Construction du dataframe
anova1 <- data.frame(nbEnfant,modeVacance)#table de données
print(anova1$modeVacance)

#Construction de l'anova 
# Sens : mode -> nbEnfant
mod <- aov(nbEnfant ~ modeVacance, data=df) # y est la variable numérique et A indique les groupes 
summary(mod)
print(mod)

#Comparaison multiple
TukeyHSD(mod)

#Vérification des hypothèses (1)
x <- split(anova1$nbEnfant,anova1$modeVacance)
x

#Vérification des hypothèses (2)
bartlett.test(x)
shapiro.test(residuals(mod))

#Boxplot ne fonctionne pas
boxplot(anova1$nbEnfant~anova1$modeVacance)





# AFC : Corrélation entre le mode de vacances et la situation familiale.

modeVacance <- donnéesQuestionnaire$`Q88 [1]`
situationFamiliale <- donnéesQuestionnaire$`Q11 [1]`

# 1. convertir les données en tant que table
table <- table(modeVacance,situationFamiliale)
table <- table[,-4]
rownames(table) <- c("Totalement Organisé", "Semi-Organisé", "En demi-pension", "En pension complète", "Mode aventure", "Mode itinérant")
table

chisq <- chisq.test(table)
chisq

res.ca <- CA(table, graph=TRUE)
print(res.ca)

#Affichage des dimensions
eig.val <- get_eigenvalue (res.ca)
eig.val



#ACM Situation Familiale, Distance parcourue pour aller en vacances, Comment vous rendez-vous sur place, Comment vous déplacez-vous sur place?
situationFamiliale <- donnéesQuestionnaire$`Q11 [1]`
distanceParcourue <- donnéesQuestionnaire$`Q97 [1]`
moyenDeplacementSurPlace <- donnéesQuestionnaire$`Q89 [1]`
moyenDeplacement <- donnéesQuestionnaire$`Q90 [1]`

table2 <- rbind(situationFamiliale,distanceParcourue,moyenDeplacementSurPlace,moyenDeplacement)
table2

res.acm <- MCA(table2)


#AFC Situation familiale et corrélation avec Partez-vous le plus souvent : 
situationFamiliale <- donnéesQuestionnaire$`Q11 [1]`
accompagnateur <- donnéesQuestionnaire$`Q87 [1]`

# 1. convertir les données en tant que table
table3 <- table(accompagnateur,situationFamiliale)
table3 <- table3[-(6:47),-4]
rownames(table3) <- c("En couple sans enfant", "En couple avec vos enfants", "Avec des amis", "En famille", "En solitaire")
table3

chisq <- chisq.test(table3)
chisq

res.ca2 <- CA(table3, graph=FALSE)
print(res.ca2)

#Affichage des dimensions
eig.val <- get_eigenvalue (res.ca2)
eig.val





#matriceACMtrie<-aperm(matriceACMtrie)
#rownames(matriceACMtrie) <- c("Kit anti-crevaison", "Huile de moteur / graisse pour la chaine", "Eau distillées pour batterie",
#                              "Outils divers (clefs/tournevis/bougies...","Nanomètre pour contrôler la préssion des pneus","Pas de materiel")

#res.mca <- MCA(matriceACMtrie, ncp = 5, graph = TRUE)
