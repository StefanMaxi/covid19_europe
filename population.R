##population
populateAge <- as.data.table(read_excel("data/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx"))
view(populateAge)


#Belgien
Bel_Age <- subset(populateAge, ...3 == 'Belgium' & ...8 == 2020)

agegroup <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")

agePopulation <- function(datasubset) {
  #according to the layout of the xlsx
  ageResult <- vector()
  counter <- seq(1, 20, 2)
  print(datasubset[1,9])
  for (i in counter){
    res <- as.numeric(datasubset[[1,i+8]]) + as.numeric(datasubset[[1,i+9]])
    ageResult <- append(ageResult, res)
  }
  return(ageResult)
}

BelAgeDis <- agePopulation(Bel_Age)
BelAgeDis[10] <- BelAgeDis[10] + as.numeric(Bel_Age[[1,29]])#add the 100+ group to the 90+ group
BelAgeDis##results in thousands

#Sweden
Swe_Age <- subset(populateAge, ...3 == 'Sweden' & ...8 == 2020)
SweAgeDis <- agePopulation(Swe_Age)
SweAgeDis[10] <- SweAgeDis[10] + as.numeric(Swe_Age[[1,29]])#add the 100+ group to the 90+ group
SweAgeDis##results in thousands

#Czech
Cze_Age <- subset(populateAge, ...3 == 'Czechia' & ...8 == 2020)
CzeAgeDis <- agePopulation(Cze_Age)
CzeAgeDis[10] <- CzeAgeDis[10] + as.numeric(Cze_Age[[1,29]])#add the 100+ group to the 90+ group
CzeAgeDis##results in thousands

#Bayern age group ('0-9', '10-19','20-29','30-39','40-49','50-59','60-65','65-75','75+')
#source https://www.statistikdaten.bayern.de/genesis/online/data?operation=abruftabelleBearbeiten&levelindex=1&levelid=1610197444757&auswahloperation=abruftabelleAuspraegungAuswaehlen&auswahlverzeichnis=ordnungsstruktur&auswahlziel=werteabruf&code=12411-006r&auswahltext=&nummer=4&variable=4&name=AGR116&nummer=5&variable=5&name=KREISE&nummer=6&variable=6&name=GES&werteabruf=Value+retrieval 
BayAgeDis <- c(1223615, 1201114, 1624985, 1740806, 1662734, 2117000, 859379, 1290506, 1404598)

#infect rate in age groups
#Belgien
resultBelAge
BelcasesAged <- resultBelAge[1:10]#delete the unknown group
BelAgeRate <- BelcasesAged / (BelAgeDis*1000)
Belgium <- label_percent()(BelAgeRate)
Belgien.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),
                          Belgium)
Belgien.age

#Sweden
Schweden.age
SwecasesAged <- unlist(Schweden.age[1:10,2])
SweAgeRate <- SwecasesAged / (SweAgeDis*1000)
Schweden <- label_percent()(SweAgeRate)
Sweden.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),
                          Schweden)
Sweden.age

#Czech
Czech.age.group
CzechAgeRate <- Czech.age.group / (CzeAgeDis*1000)
Tschechien <- label_percent()(CzechAgeRate)
Czech.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),
                        Tschechien)
Czech.age

#Bayern
bayern.age.and.sex
bayAgeGroup <- rowSums(bayern.age.and.sex[1:11, 2:4])
bayAgeGroup <- bayAgeGroup[1:6]
BayAgeDis <- BayAgeDis[1:6] #take the first 6 groups
BayAgeRate <- bayAgeGroup/BayAgeDis
BayernAnteil <- label_percent()(BayAgeRate)
Bayern.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59"),
                         BayernAnteil)
Bayern.age

#
AgeDF <- merge(Czech.age, Belgien.age, by='Altersgruppe')
AgeDF <- merge(AgeDF, Sweden.age, by='Altersgruppe')
AgeDF2 <- as.data.table(AgeDF)
AgeDFmelted <- melt(AgeDF2, measure.var=c('Tschechien','Belgium','Schweden'),variable.name='Land', value.name='Anteil')

AgeDFmelted2 <- as.data.frame(AgeDFmelted)
#AgeDFmelted2$Anteil <- as.numeric(levels(AgeDFmelted2$Anteil))[AgeDFmelted2$Anteil]
AgeDFmelted2$Anteil <- as.numeric(sub("%", "", AgeDFmelted2$Anteil))
ggplot(AgeDFmelted2, aes(Altersgruppe, Anteil, color=Land)) + geom_point(size = 3) +ggtitle("Krankanteil nach Altersgruppe")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black",size = 15),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=15,colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))+labs(y="Anteil(%)")

