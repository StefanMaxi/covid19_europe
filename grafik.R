#Todesfall
bayern.todesfall <- 6266 #Datum 21.12.2020
belgien.todesfall <- sum(Daten$belgium$belgium_mortality$Todesfälle)
sweden.todesfall <- sum(Daten$sweden$sweden_2$Todesfälle)
#select the last date
updateTag <- tail(Daten$czech$czech_1$Datum, n=1)
czech.latest <- subset(Daten$czech$czech_1, Datum == updateTag)
czech.todesfall <- sum(czech.latest$Gesamttodeszahl)

todesfall.four.lands <- c(bayern.todesfall, belgien.todesfall, sweden.todesfall, czech.todesfall)
todesfall.vergleich <- data.frame(Land = c("Bayern", "Belgien", "Sweden", "Czech"), Todesfaelle = todesfall.four.lands)
todesfall.vergleich
ggplot(todesfall.vergleich,aes(Land, Todesfaelle))+geom_bar(stat="identity",position = "dodge")

#population 4 lands
Land <- c("Bayern", "Belgium", "Sweden", "Czech")
Population <- c(13300551, 11614813, 10099265, 10693939)
populations <- data.frame(Land, Population)
#A table with the numbers should be shown in the slides

#dead cases per 100,000
Todesfallanzahl <- todesfall.four.lands / Population * 100000
deadPer100kDF <- data.frame(Land, Todesfallanzahl)
ggplot(deadPer100kDF,aes(Land, Todesfallanzahl))+
  geom_bar(stat="identity",position = "dodge",fill="purple")+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"))+
  ggtitle(" Todesfallzahl pro 100,000 Einwohner")