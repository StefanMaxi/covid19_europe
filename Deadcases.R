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

