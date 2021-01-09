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

#Bayern age group ('0-9', '10-19','20-29','30-39','40-49','50-65','65-75','75+')
#source https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/BYHeft_derivate_00004364/A1310C%20201300.pdf;jsessionid=FC17B21B1E1341AA35CDAFF2433AC08F
BayAgeDis <- c(1089023, 1267734, 1569379, 15688811, 9727092, 2636372, 1298010, 1202136)

#infect rate in age groups
#Belgien
resultBelAge
BelcasesAged <- resultBelAge[1:10]#delete the unknown group
BelAgeRate <- BelcasesAged / (BelAgeDis*1000)
BelgienAnteil <- label_percent()(BelAgeRate)
Belgien.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),
                          BelgienAnteil)
Belgien.age

#Sweden
Schweden.age
SwecasesAged <- unlist(Schweden.age[1:10,2])
SweAgeRate <- SwecasesAged / (SweAgeDis*1000)
SchwedenAnteil <- percent(SweAgeRate)
Sweden.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),
                          SchwedenAnteil)
Sweden.age

#Czech
Czech.age.group
CzechAgeRate <- Czech.age.group / (CzeAgeDis*1000)
CzechAnteil <- label_percent()(CzechAgeRate)
Czech.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),
                          CzechAnteil)
Czech.age

#Bayern
