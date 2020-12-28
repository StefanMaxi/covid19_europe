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
belgien.day.cases + geom_smooth(colour="blue")

casesfourland <- ggplot() +
  geom_smooth(data = cases.per.day, mapping = aes(x=Datum, y=Bayern), colour="red") +
  geom_smooth(data = cases.per.day, mapping = aes(x=Datum, y=Belgien), colour="blue")+
  geom_smooth(data = cases.per.day, mapping = aes(x=Datum, y=Schweden), colour="yellow")+
  geom_smooth(data = cases.per.day, mapping = aes(x=Datum, y=Tschechien), colour="green")

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
ggplot(Bayern.age,aes(Altersgruppe, Faelle))+geom_bar(stat="identity",position = "dodge")

#Belgien
belgien.age.sex <- (Daten$belgium$belgium_age.sex)
belgien.age.sex

belgien.0 <- subset(belgien.age.sex, Altersgruppe == "0-9")
b.a.0 <- nrow(belgien.0)
belgien.1 <- subset(belgien.age.sex, Altersgruppe == "10-19")
b.a.1 <- nrow(belgien.1)
belgien.2 <- subset(belgien.age.sex, Altersgruppe == "20-29")
b.a.2 <- nrow(belgien.2)
belgien.3 <- subset(belgien.age.sex, Altersgruppe == "30-39")
b.a.3 <- nrow(belgien.3)
belgien.4 <- subset(belgien.age.sex, Altersgruppe == "40-49")
b.a.4 <- nrow(belgien.4)
belgien.5 <- subset(belgien.age.sex, Altersgruppe == "50-59")
b.a.5 <- nrow(belgien.5)
belgien.6 <- subset(belgien.age.sex, Altersgruppe == "60-69")
b.a.6 <- nrow(belgien.6)
belgien.7 <- subset(belgien.age.sex, Altersgruppe == "70-79")
b.a.7 <- nrow(belgien.7)
belgien.8 <- subset(belgien.age.sex, Altersgruppe == "80-89")
b.a.8 <- nrow(belgien.8)
belgien.9 <- subset(belgien.age.sex, Altersgruppe == "90+")
b.a.9 <- nrow(belgien.9)
b.a.na <- subset(belgien.age.sex, is.na(Altersgruppe))
b.a.10 <- nrow(b.a.na)

Belgien.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+", "unbekannt"),
                          Faelle = c(b.a.0, b.a.1, b.a.2, b.a.3, b.a.4, b.a.5, b.a.6, b.a.7, b.a.8, b.a.9, b.a.10))

Belgien.age
ggplot(Belgien.age,aes(Altersgruppe, Faelle))+geom_bar(stat="identity",position = "dodge")

#if sex neet to be seperated
#belgien.0.9.f <- nrow(subset(belgien.0.9,Geschlecht == "F"))
#belgien.0.9.f

##Schweden
Schweden.age <- (Daten$sweden$sweden_6)
ggplot(Schweden.age,aes(Altersgruppe, Fälle))+geom_bar(stat="identity",position = "dodge")

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
ggplot(Czech.age,aes(Altersgruppe, Faelle))+geom_bar(stat="identity",position = "dodge")


##Bilden von Inzidenz Tabelle (Beispiele)
incidence30 <- daily.incedence(Daten, end = "2020-11-14", intervall = 30, relative = FALSE)
View(incidence30)

incidence7 <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = FALSE)
View(incidence7)

incidence3 <- daily.incedence(Daten, end = "2020-11-14", intervall = 3, relative = FALSE)
View(incidence3)

incidence7abs <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = TRUE)
View(incidence7abs)

