bavaria_death  <- subset(readRDS("data/bavaria/deaths_temporal_prepared.rds"), state == "Bayern")
bavaria_deathdaily <- bavaria_death %>% select(1,3)
names(bavaria_deathdaily)[1] <- 'Datum'
bavaria_deathdaily <- bavaria_deathdaily %>% mutate(Datum = as.character(Datum)) 
names(bavaria_deathdaily)[2] <- 'Bayern'

TschechienTot <- (Daten$czech$czech_9)
czechdeathdaily <- count(TschechienTot$Datum)
czechdeathdaily <- as.data.frame(czechdeathdaily)
names(czechdeathdaily)[1] <- 'Datum'
names(czechdeathdaily)[2] <- 'Tschechien'

BelgienTot <- as.data.table(copy(Daten$belgium$belgium_mortality)[, c(1,5)][, sum(Todesfälle), by = "Datum"][, Todesfälle := V1][, V1 := NULL][-.N])
names(BelgienTot)[2] <- 'Belgien'

SchwedenTot <- (Daten$sweden$sweden_2)
names(SchwedenTot)[2] <- 'Schweden'
Datum = seq(from = as.Date("2020-03-11"), to = as.Date("2020-12-30"), by = 'day')
SchwedenTot1 <- cbind(SchwedenTot, Datum)
SchwedenTot1 <- SchwedenTot1 %>% select(2,3)
SchwedenTot1
names(SchwedenTot1)[2] <- 'Datum'
SchwedenTot1 <- SchwedenTot1 %>% mutate(Datum = as.character(Datum)) 

Tot <- merge(BelgienTot, czechdeathdaily, all = TRUE)
Tot <- merge(Tot, bavaria_deathdaily, by='Datum',all = TRUE)
Tot <- merge(Tot, SchwedenTot1, by='Datum',all = TRUE)

Tot[is.na(Tot)] <- 0
view(Tot)
#end date 27.12.2020
deadCases2712 <- Tot[1:293,]
deadCases2712
BelDeath <- sum(deadCases2712$Belgien)
TscDeath <- sum(deadCases2712$Tschechien)
BayDeath <- sum(deadCases2712$Bayern)
SweDeatj <- sum(deadCases2712$Schweden)
sumofDeath <- c(BelDeath, TscDeath, BayDeath, SweDeatj)

casesDaily2712 <- cases.per.day[1:362,]
BelCase <- sum(casesDaily2712$Belgien)
TscCase <- sum(casesDaily2712$Tschechien)
BalCase <- sum(casesDaily2712$Bayern)
SweCase <- sum(casesDaily2712$Schweden)
sumofCases <- c(BelCase, TscCase, BalCase, SweCase)

Todesrate <- label_percent()(sumofDeath/sumofCases)

TodesrateDF <- data.frame(Land = c("Bayern", "Belgien", "Schweden", "Tschechien"), Todesrate = Todesrate)
TodesrateDF$Todesrate <- as.numeric(sub("%", "", TodesrateDF$Todesrate))

ggplot(TodesrateDF,aes(Land, Todesrate))+geom_bar(stat="identity",position = "dodge")+
  ggtitle("Todesrate")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))+labs(y="Todesrate(%)")
