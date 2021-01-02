#Daten Einlesen
data <- update.data()
#Daten Übersetzen
Daten <- translate.DE(data)
#Daten Aufbereiten
transform.data(Daten)

##Daten anzeigen
View(data)
View(Daten)

##Bilden von Fallzahl/Tag Tabelle (Beispiele)
cases.per.day <- daily.cases(Daten, end = "2020-11-14", relative = FALSE)
View(cases.per.day)
bayern.day.cases <- ggplot(data = cases.per.day, mapping = aes(x=Datum, y=Bayern))
bayern.day.cases + geom_line(colour="red")

belgien.day.cases <- ggplot(data = cases.per.day, mapping = aes(x=Datum, y=Belgien))
belgien.day.cases + geom_line(colour="blue")

casesfourland <- ggplot() +
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Bayern), colour="red", size = 1) +
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Belgien), colour="blue", size = 1)+
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Schweden), colour="yellow", size = 1)+
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Tschechien), colour="green", size = 1)+
  ggtitle("Entwicklungen")+
  xlab("Datum") + ylab("Neue Faelle")

casesfourland

cases.per.day.relative100000 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE)
View(cases.per.day.relative100000)

cases.per.day.relative1 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE, reverenz = 1)
View(cases.per.day.relative1)

##Altersgruppe

#Bayern Altesgruppe
bayern.age.and.sex <- (Daten$bavaria$lgl$bavaria_lgl_age)
#data correction
bayern.age.and.sex$weiblich <- replace(bayern.age.and.sex$weiblich, c(1:9), bayern.age.and.sex$weiblich*1000)
bayern.age.and.sex$maennlich <- replace(bayern.age.and.sex$maennlich, c(1:9), bayern.age.and.sex$maennlich*1000)
bayern.age.and.sex$unbekannt <- replace(bayern.age.and.sex$unbekannt, c(12), bayern.age.and.sex$unbekannt*1000)
bayern.age.and.sex %>% mutate_if(is.numeric, ~round(.))
bayern.age.and.sex <- head(bayern.age.and.sex,-1)

#add the lost row with data date21.12.2020
bayern.age.and.sex <- bayern.age.and.sex %>% add_row(Altersgruppe="0 bis 9", weiblich=7415, maennlich=8021, unbekannt=364, .before = 1)
bayer.age.casessum <- rowSums( bayern.age.and.sex[,2:4] )

bayern.sex.and.age <- melt(bayern.age.and.sex, id.vars = "Altersgruppe", variable.name = "sex", value.name = "cases")
bayern.sex.and.age
ggplot(bayern.sex.and.age,aes(Altersgruppe, cases, fill=sex))+geom_bar(stat="identity",position = "dodge")

#Altersgruppe(ohne geschlecht)
Bayern.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","Aelter als 100", "unbekannt"),
                         Faelle = bayer.age.casessum)
ggplot(Bayern.age,aes(Altersgruppe, Faelle))+geom_bar(stat="identity",position = "dodge")+ggtitle("Bayern")

#Belgien
belgien.age.sex <- (Daten$belgium$belgium_age.sex)
belgien.age.sex

countingCasesBel <- function(dataset) {
  agegroup <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
  groupCountingResult <- rep(0, 10)
  counter <- c(1:10)
  for (x in counter) {
    groupCountingResult[x] <- sum((subset(dataset, Altersgruppe == agegroup[x]))$bel.Fälle)
  }
  return(groupCountingResult)
}

resultBelAge <- countingCasesBel(belgien.age.sex)

b.a.na <- subset(belgien.age.sex, is.na(Altersgruppe))
b.a.10 <- nrow(b.a.na)
resultBelAge <- append(resultBelAge, b.a.10)
Belgien.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+", "unbekannt"),
                          resultBelAge)

Belgien.age
ggplot(Belgien.age,aes(Altersgruppe, resultBelAge))+geom_bar(stat="identity",position = "dodge")+ggtitle("Belgien")+ ylab("Faelle")

#if sex neet to be seperated
#belgien.0.9.f <- nrow(subset(belgien.0.9,Geschlecht == "F"))
#belgien.0.9.f

##Schweden
Schweden.age <- (Daten$sweden$sweden_6)
ggplot(Schweden.age,aes(Altersgruppe, Fälle))+geom_bar(stat="identity",position = "dodge")+ggtitle("Sweden")

##Tschechien
Czech.all.cases <- (Daten$czech$czech_6)
#create age groups
countingCasesforAgeGroup <- function(dataset) {
  agegroup <- c(0:8)
  groupCountingResult <- rep(0, 9)
  for (x in agegroup) {
    groupCountingResult[x+1] <- nrow(subset(Czech.all.cases, Alter >= x*10 & Alter < (x+1)*10))
  }
  return(groupCountingResult)
}

Czech.age.group <- countingCasesforAgeGroup(Czech.all.cases)
czech.aelter100 <- nrow(Czech.all.cases) - sum(Czech.age.group)
#add the 90+ group to the groups vector
Czech.age.group <- append(Czech.age.group, czech.aelter100)

Czech.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),
                          Faelle = Czech.age.group)

Czech.age
ggplot(Czech.age,aes(Altersgruppe, Faelle))+geom_bar(stat="identity",position = "dodge")+ggtitle("Czech")

##Kernwerte
#Geschlecht Anteil 
#Bayern
bayernGeschlechtW <- sum(bayern.age.and.sex$weiblich) / (sum(bayern.age.and.sex$maennlich) + sum(bayern.age.and.sex$weiblich))
bayernGeschlechtM <- sum(bayern.age.and.sex$maennlich) / (sum(bayern.age.and.sex$maennlich) + sum(bayern.age.and.sex$weiblich))
bayernW <- label_percent()(bayernGeschlechtW) #51%
bayernM <- label_percent()(bayernGeschlechtM) #49%

#Belgien
belgienW <- sum(subset(belgien.age.sex, Geschlecht == "F")$bel.Fälle)
belgienM <- sum(subset(belgien.age.sex, Geschlecht == "M")$bel.Fälle)
percBelW <- label_percent()(belgienW / (belgienW + belgienM)) #55%
percBelM <- label_percent()(belgienM / (belgienW + belgienM)) #45%

#Sweden
swedenGeschlecht <- (Daten$sweden$sweden_5)
swedenW <- swedenGeschlecht[[2,2]]
swedenM <- swedenGeschlecht[[1,2]]
percSweM <- label_percent()(swedenM / (swedenM + swedenW)) #47%
percSweW <- label_percent()(swedenW / (swedenM + swedenW)) #53%

#Czech
czechW <- nrow(subset(Czech.all.cases, Geschlecht == "Z"))
czechM <- nrow(subset(Czech.all.cases, Geschlecht == "M"))
percCzeW <- label_percent()(czechW / (czechW + czechM)) #52%
percCzeM <- label_percent()(czechM / (czechW + czechM)) #48%

#Vergleich
mat_sex <- matrix(
  c(bayernM, percBelM, percSweM, percCzeM, 
    bayernW, percBelW, percSweW, percCzeW),
  nrow=4, ncol=2,
  dimnames = list(c("Bayern", "Belgien", "Sweden", "Czech"), c("Male", "Female"))
)
knitr::kable(mat_sex)
d_sex <- as.data.frame(as.table(mat_sex))
names(d_sex) <- c("Land", "Geschlecht", "Anteil")
knitr::kable(d_sex)

p <- ggplot(data = d_sex, mapping = aes(
  x = `Land`, fill = `Geschlecht`, y = `Anteil`
))
p + geom_col() + geom_hline(aes(yintercept=4.5),colour="red",linetype="dashed")

##Bilden von Inzidenz Tabelle (Beispiele)
incidence30 <- daily.incedence(Daten, end = "2020-11-14", intervall = 30, relative = FALSE)
View(incidence30)

incidence7 <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = FALSE)
View(incidence7)

incidence3 <- daily.incedence(Daten, end = "2020-11-14", intervall = 3, relative = FALSE)
View(incidence3)

incidence7abs <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = TRUE)
View(incidence7abs)

