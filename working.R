#Daten Einlesen
data <- update.data()
#Daten Übersetzen
Daten <- translate.DE(data)
#Daten Aufbereiten
transform.data(Daten)

##Daten anzeigen
View(datan)
View(Daten)

##Bilden von Fallzahl/Tag Tabelle (Beispiele)
cases.per.day <- daily.cases(Daten, end = "2020-11-14", relative = FALSE)
View(cases.per.day)

cases.per.day.relative100000 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE)
View(cases.per.day.relative100000)

cases.per.day.relative1 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE, reverenz = 1)
View(cases.per.day.relative1)

##Bilden von Inzidenz Tabelle (Beispiele)
incedence30 <- daily.incedence(Daten, end = "2020-11-14", intervall = 30, relative = FALSE)
View(incedence30)

incedence7 <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = FALSE)
View(incedence7)

incedence3 <- daily.incedence(Daten, end = "2020-11-14", intervall = 3, relative = FALSE)
View(incedence3)

incedence7abs <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = TRUE)
View(incedence7abs)

