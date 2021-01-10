#04.01.2021
update.data <- function() { 
  #BAVARIA
  
  bavaria_lgl_overview  <- as.data.table(read.csv("data/bavaria/lgl/tabelle_01_2021-01-04.csv", skip = 1, sep = ";", encoding="UTF-8"))
  bavaria_lgl_change    <- as.data.table(read.csv("data/bavaria/lgl/tabelle_02_2021-01-04.csv", skip = 1, sep = ";", encoding="UTF-8"))
  bavaria_lgl_regions   <- as.data.table(read.csv("data/bavaria/lgl/tabelle_03_2021-01-04.csv", skip = 1, sep = ";", encoding="UTF-8"))
  bavaria_lgl_countys   <- as.data.table(read.csv("data/bavaria/lgl/tabelle_04_2021-01-04.csv", skip = 1, sep = ";", encoding="UTF-8"))
  bavaria_lgl_days      <- as.data.table(read.csv("data/bavaria/lgl/tabelle_05_2021-01-04.csv", skip = 1, sep = ";", encoding="UTF-8", dec = "."))
  bavaria_lgl_weeks1    <- as.data.table(read.csv("data/bavaria/lgl/tabelle_06_2021-01-04.csv", skip = 1, sep = ";", encoding="UTF-8"))
  bavaria_lgl_age       <- as.data.table(read.csv("data/bavaria/lgl/tabelle_07_2021-01-04.csv", skip = 1, sep = ";", encoding="UTF-8"))
  bavaria_lgl_weeks2    <- as.data.table(read.csv("data/bavaria/lgl/tabelle_08_2021-01-04.csv", skip = 1, sep = ";", encoding="UTF-8"))
  bavaria_lgl_tests     <- as.data.table(read.csv("data/bavaria/lgl/tabelle_09_2021-01-04.csv", skip = 1, sep = ";", encoding="UTF-8"))
  
  lgl <- list(bavaria_lgl_overview, bavaria_lgl_change, bavaria_lgl_regions, bavaria_lgl_countys, bavaria_lgl_days,
              bavaria_lgl_weeks1, bavaria_lgl_age, bavaria_lgl_weeks2, bavaria_lgl_tests)
  names(lgl) <- c("bavaria_lgl_overview", "bavaria_lgl_change", "bavaria_lgl_regions", "bavaria_lgl_countys", "bavaria_lgl_days",
                  "bavaria_lgl_weeks1", "bavaria_lgl_age", "bavaria_lgl_weeks2", "bavaria_lgl_tests")
  
  bavaria_rki           <- as.data.table(read.csv("data/bavaria/rki/RKI_COVID19.csv", encoding="UTF-8"))
  
  rki <- list(bavaria_rki)
  names(rki) <- c("bavaria_rki")
  bavaria <- list(lgl = lgl, rki = rki)
  
  
  #BELGIUM
  
  belgium_age.sex       <- as.data.table(read.csv("data/belgium/COVID19BE_CASES_AGESEX.csv"))
  belgium_countys       <- as.data.table(read.csv("data/belgium/COVID19BE_CASES_MUNI_CUM.csv"))
  belgium_hospilisation <- as.data.table(read.csv("data/belgium/COVID19BE_HOSP.csv"))
  belgium_mortality     <- as.data.table(read.csv("data/belgium/COVID19BE_MORT.csv"))
  belgium_tests         <- as.data.table(read.csv("data/belgium/COVID19BE_tests.csv"))
  
  belgium <- list(belgium_age.sex, belgium_countys, belgium_hospilisation, belgium_mortality, belgium_tests)
  names(belgium) <- c("belgium_age.sex", "belgium_countys", "belgium_hospilisation", "belgium_mortality", "belgium_tests")
  
  #CZECH
  czech_1        <- as.data.table(read.csv("data/czech/kraj-okres-nakazeni-vyleceni-umrti.csv", fileEncoding="UTF-8-BOM"))
  czech_3        <- as.data.table(read.csv("data/czech/nakaza.csv", fileEncoding="UTF-8-BOM"))
  czech_4        <- as.data.table(read.csv("data/czech/nakazeni-vyleceni-umrti-testy.csv", fileEncoding="UTF-8-BOM"))
  czech_6        <- as.data.table(read.csv("data/czech/osoby.csv", fileEncoding="UTF-8-BOM"))
  czech_7        <- as.data.table(read.csv("data/czech/pomucky.csv", fileEncoding="UTF-8-BOM"))
  czech_8        <- as.data.table(read.csv("data/czech/testy.csv", fileEncoding="UTF-8-BOM"))
  czech_9        <- as.data.table(read.csv("data/czech/umrti.csv", fileEncoding="UTF-8-BOM"))
  
  czech <- list(czech_1, czech_3, czech_4, czech_6, czech_7, czech_8, czech_9) 
  
  names(czech) <- c("czech_1", "czech_3", "czech_4", "czech_6", "czech_7", "czech_8", "czech_9")
  
  #SWEDEN
  sweden_1 <- as.data.table(read_excel("data/sweden/Folkhalsomyndigheten_Covid19.xlsx", sheet = 1))
  sweden_2 <- as.data.table(read_excel("data/sweden/Folkhalsomyndigheten_Covid19.xlsx", sheet = 2))
  sweden_3 <- as.data.table(read_excel("data/sweden/Folkhalsomyndigheten_Covid19.xlsx", sheet = 3))
  sweden_4 <- as.data.table(read_excel("data/sweden/Folkhalsomyndigheten_Covid19.xlsx", sheet = 4))
  sweden_5 <- as.data.table(read_excel("data/sweden/Folkhalsomyndigheten_Covid19.xlsx", sheet = 5))
  sweden_6 <- as.data.table(read_excel("data/sweden/Folkhalsomyndigheten_Covid19.xlsx", sheet = 6))
  sweden_7 <- as.data.table(read_excel("data/sweden/Folkhalsomyndigheten_Covid19.xlsx", sheet = 7))
  sweden_8 <- as.data.table(read_excel("data/sweden/Folkhalsomyndigheten_Covid19.xlsx", sheet = 8, col_types = "text"))
  
  sweden <- list(sweden_1, sweden_2, sweden_3, sweden_4, sweden_5, sweden_6, sweden_7, sweden_8)
  names(sweden) <- c("sweden_1", "sweden_2", "sweden_3", "sweden_4", "sweden_5", "sweden_6", "sweden_7", "sweden_8")
  
  data <- list(bavaria = bavaria, belgium = belgium, czech = czech, sweden = sweden)
  return(data)
}

translate.DE <- function(data) {
  Daten <- data
  #Bayern
  names(Daten$bavaria$lgl$bavaria_lgl_overview) <- c("Region", "Faelle", "Todesfaelle")
  names(Daten$bavaria$lgl$bavaria_lgl_change) <- c("Spalte", "Gesamtfallzahl", "Differenz.zum.letzten.Aktualisierungs.Zeitpunkt", "Neu.berichtete.aktuelle.Fälle.seit.letzter.Aktualisierung", "Nachmeldungen", "Löschungen")
  names(Daten$bavaria$lgl$bavaria_lgl_regions) <- c("Regierungsbezirk", "Anzahl.der.Fälle", "Fälle.Änderung.zum.Vortag", "Fallzahl.pro.100.000.Einwohner", "Fälle.der.letzten.7.Tage", "X7.Tage.Inzidenz.pro.100.000.Einwohner", "Anzahl.der.Todesfälle", "Todesfälle.Änderung.zum.Vortag")
  names(Daten$bavaria$lgl$bavaria_lgl_county) <- c("Landkreis.Stadt", "Anzahl.der.Fälle", "Fälle.Änderung.zum.Vortag", "Fallzahl.pro.100.000.Einwohner", "Fälle.der.letzten.7.Tage", "X7.Tage.Inzidenz.pro.100.000.Einwohner", "Anzahl.der.Todesfälle", "Todesfälle.Änderung.zum.Vortag")
  names(Daten$bavaria$lgl$bavaria_lgl_days) <- c("Datum", "Vortag.bekannt", "Heute.bekannt", "bay.Fälle")
  names(Daten$bavaria$lgl$bavaria_lgl_weeks1) <- c("Jahr","Kalenderwoche", "Altersgruppe", "Inzidenz", "Fälle")
  names(Daten$bavaria$lgl$bavaria_lgl_age) <- c("Altersgruppe", "weiblich", "maennlich", "unbekannt")
  names(Daten$bavaria$lgl$bavaria_lgl_weeks2) <- c("Kalenderwoche", "Altersgruppe", "Inzidenz", "Fälle")
  names(Daten$bavaria$lgl$bavaria_lgl_tests) <- c("Datum", "Tests", "positiv", "negativ", "Positivrate")
  
  names(Daten$bavaria$rki$bavaria_rki) <- c("FID","IdBundesland","Bundesland","Landkreis","Altersgruppe","Geschlecht","AnzahlFall","AnzahlTodesfall","Meldedatum","IdLandkreis","Datenstand","NeuerFall","NeuerTodesfall","Refdatum","NeuGenesen","AnzahlGenesen","IstErkrankungsbeginn","Altergruppe2")
  
  #Belgium
  names(Daten$belgium$belgium_age.sex) <- c("Datum", "Bundesland", "Landkreis", "Altersgruppe", "Geschlecht", "bel.Fälle")
  names(Daten$belgium$belgium_countys) <- c("NIS5", "Gemeinde.NL", "Gemeinde.FR", "Arrondissement.NL", "Arrondissement.FR", "Bundesland", "Landkreis", "Fälle")
  names(Daten$belgium$belgium_hospilisation) <- c("Datum", "Bundesland", "Landkreis", "Nr.Reporting", "Alle.in", "Alle.in.ICU", "Alle.in.RESP", "Alle.in.ECMO", "Neue.in", "Neue.raus") 
  names(Daten$belgium$belgium_mortality) <- c("Datum", "Landkreis", "Altersgruppe", "Geschlecht", "Todesfälle")
  names(Daten$belgium$belgium_tests) <- c("Datum", "Bundesland", "Landkreis", "Tests", "positiv")
  
  #Tschechien
  names(Daten$czech$czech_1) <- c("Datum", "Bundesland", "Landkreis", "Gesamtfallzahl", "Gesamtgeheiltenzahl", "Gesamttodeszahl") ## Gliederung Tschechien 
  names(Daten$czech$czech_3) <- c("Datum", "tsc.Fälle", "Gesamtfallzahl")
  names(Daten$czech$czech_4) <- c("Datum","Gesamtfallzahl","Gesamtgeheiltenzahl","Gesamttodeszahl","Gesamttestzahl")
  names(Daten$czech$czech_6) <- c("Datum","Alter","Geschlecht","Bundesland","Landkreis","AuslandInfektionZahl","AuslandCode")
  names(Daten$czech$czech_7) <- c("HilfeProdukt","Bundesland","GesamtMenge")
  names(Daten$czech$czech_8) <- c("Datum","tsc.Testzahl","GesamtfälleAnzahl_Test","tsc.Anzahl_1stTest","GesamfälleAnzahl_1stTest")
  names(Daten$czech$czech_9) <- c("Datum","Alter","Geschlecht","Bundesland","Landkreis")
  
  #Schweden
  names(Daten$sweden$sweden_1) <- c("Datum", "swe.Fälle", "Blekinge", "Dalarna", "Gotland", "Gävleborg", "Halland", "Jämtland_Härjedalen", "Jönköping",
                                    "Kalmar", "Kronoberg", "Norrbotten", "Skåne", "Stockholm", "Sörmland", "Uppsala", "Värmland", "Västerbotten", "Västernorrland",
                                    "Västmanland", "Västra_Götaland ", "Örebro", "Östergötland")
  names(Daten$sweden$sweden_2) <- c("Datum", "Todesfälle")
  names(Daten$sweden$sweden_3) <- c("Datum.Begin.der.Pflege", "Anzahl.ICU")
  names(Daten$sweden$sweden_4) <- c("Landkreis", "Fälle", "Fallzahl.pro.100.000.Einwohner", "Anzahl.Fälle.IC", "Todesfälle")
  names(Daten$sweden$sweden_5) <- c("Geschlecht", "Fälle", "Anzahl.Fälle.ICU", "Todesfälle")
  names(Daten$sweden$sweden_6) <- c("Altersgruppe", "Fälle", "Anzahl.der.Fälle.IC", "Todesfälle")
  names(Daten$sweden$sweden_7) <- c("Jahr", "Wochen.Nr", "Landkreis", "Anzahl.der.Fälle.der.Woche", "Gesamtzahl.der.Fälle", "Anzahl.Fälle.IC.der.Woche", "Gesamtzahl.der.Fälle.IC", "Todesfälle.der.Woche", "Gesamtzahl.der.Todesfälle", "Fallzahl.pro.100.000.Einwohner.der.Woche", "Gesamte.Fallzahl.pro.100.000.Einwohner")
  names(Daten$sweden$sweden_8) <- c("Jahr", "Wochen.Nr", "Kn.Code", "Kn.Name", "Kreis", "Gemeindebezirk", "Gesamtzahl.der.Fälle.pro.10000.Einwohner", "Anzahl.der.Fälle.pro.10000.Einwohner", "Gesamtzahl.der.Fälle", "neue.Fälle.der.Woche")
  
  
  
  return(Daten)
}

transform.data <- function(Daten) {
  #Bavaria
  Daten$bavaria$lgl$bavaria_lgl_days[, `:=`(Datum = as.Date(Datum, "%d.%m.%Y"), bay.Fälle = as.numeric(str_replace(as.character(bay.Fälle), "\\.", "")))]
  
  #Belgium
  Daten$belgium$belgium_age.sex[, Datum := as.Date(Datum)]
  
  #Czeck
  Daten$czech$czech_3[, Datum := as.Date(Datum)]
  
  #Sweden
  Daten$sweden$sweden_1[, Datum := as.Date(Datum)]
  
}

daily.cases <- function(Daten, end = Sys.Date(), relative = TRUE, reverenz = 100000) {
  inhabitants <- c(Bayern = 13124737, Belgien = 11431406, Schweden = 10327589, Tschechien = 10637794)
  factor <- reverenz / inhabitants
  pandamic.start <- as.Date("2020-01-01")
  pandemic.deadline <- as.Date(end)
  Datum <- as.data.table(list(seq.Date(pandamic.start, pandemic.deadline, "day")))
  names(Datum) <- "Datum"
  
  Bayern <- copy(Daten$bavaria$lgl$bavaria_lgl_days)[, c(1,4)]
  Belgien <- copy(Daten$belgium$belgium_age.sex)[, c(1,6)][, sum(bel.Fälle), by = "Datum"][, bel.Fälle := V1][, V1 := NULL][-.N] #Letze zele hat kein Datum
  Tschechien <- copy(Daten$czech$czech_3)[, c(1,2)]
  Schweden <- copy(Daten$sweden$sweden_1)[, c(1,2)]
  
  A <- Tschechien[Datum, on = "Datum"]
  B <- Schweden[A, on = "Datum"]
  C <- Belgien[B, on = "Datum"]
  D <- Bayern[C, on = "Datum"]
  cases <- D
  cases[is.na(cases)] <- 0
  names(cases) <- c("Datum", "Bayern", "Belgien","Schweden", "Tschechien")
  if (relative) {
    cases[, `:=`(Bayern = Bayern * factor[1], Belgien = Belgien * factor[2], Schweden = Schweden * factor[3], Tschechien = Tschechien * factor[4])]
  }
  
  return(cases)
}
daily.incedence <- function(Daten, end = Sys.Date(), intervall = 7, relative = TRUE, reverenz = 100000) {
  daily.data <- daily.cases(Daten, end, relative, reverenz)
  glider.fun <- function(vector) {
    intervall <- intervall - 1
    vector_long <- c(rep(0, times = intervall), as.vector(vector))
    result <- NULL
    for(i in seq(from = intervall, to = length(vector_long))) {
      new <- 0
      for(j in seq(from = i, to = i - (intervall))) {
        new <- new + vector_long[j]
      }
      result <- c(result, new)
    }
    return(result)
  }
  daily.data[, `:=`(Bayern = glider.fun(Bayern), Belgien = glider.fun(Belgien), Schweden = glider.fun(Schweden), Tschechien = glider.fun(Tschechien))]
  return(daily.data)
}

