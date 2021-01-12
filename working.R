#Daten Einlesen
data <- update.data()
#Daten Übersetzen
Daten <- translate.DE(data)
#Daten Aufbereiten
transform.data(Daten)

##Daten anzeigen
View(data)
View(Daten)

##Bilden von Todesfall/Tag Tabelle (Beispiele)
deathcases.per.day <- daily.death.cases(Daten, end = "2020-12-30", relative = FALSE)

##Bilden von Fallzahl/Tag Tabelle (Beispiele)
cases.per.day <- daily.cases(Daten, end = "2020-12-30", relative = FALSE)
View(cases.per.day)
bayern.day.cases <- ggplot(data = cases.per.day, mapping = aes(x=Datum, y=Bayern))
bayern.day.cases + geom_line(colour="red")

belgien.day.cases <- ggplot(data = cases.per.day, mapping = aes(x=Datum, y=Belgien))
belgien.day.cases + geom_line(colour="blue")

#Infektion pro Tag
casesfourland <- ggplot(show.legend=TRUE) +
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Bayern), colour="red", size = 0.5,show.legend = TRUE) +
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Belgien), colour="blue", size = 0.5,show.legend = TRUE)+
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Schweden), colour="yellow", size = 0.5,show.legend = TRUE)+
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Tschechien), colour="green", size = 0.5,show.legend = TRUE)+
  ggtitle("Infektionsentwicklungen")+
  scale_x_date(date_breaks = "1 week",expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 45,hjust =1,vjust = 1,colour = "black"),
        plot.title = element_text(size="35",lineheight =.6,face = "bold",colour = "black",hjust = 0.5),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))+
  xlab("Datum") + ylab("Neue Faelle")+
  geom_hline(aes(yintercept=15000),colour="red",linetype="dashed")

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

incidence7 <- daily.incedence(Daten, end = "2020-12-30", intervall = 7, relative = FALSE)
View(incidence7)
incidence7proEinwohner <- daily.incedence(Daten, end = "2020-12-30", intervall = 7, relative = TRUE)
View(incidence7proEinwohner)

incidence07a <- subset(incidence7proEinwohner,Datum>="2020-01-28"&Datum<="2020-08-03")
incidence07b<- subset(incidence7proEinwohner,Datum>="2020-02-04"&Datum<="2020-09-07")
incidence07c<-subset(incidence7proEinwohner,Datum>="2020-03-01"&Datum<="2020-07-13")
bay.inzidenz1<-as.data.frame(incidence07a[,c(1,2)])
#view(bay.inzidenz)
bel.inzidenz1<-as.data.frame(incidence07c[,c(1,3)])
#view(bel.inzidenz1)
sch.inzidenz1<-as.data.frame(incidence07b[,c(1,4)])
tsc.inzidenz1<-as.data.frame(incidence07c[,c(1,5)])

bay.inzidenz1$number<-seq(1,189,by=1)
bel.inzidenz1$number<-seq(1,135,by=1)
inzidenz.four.lands1<-merge(bay.inzidenz1,bel.inzidenz1,by="number",all=TRUE,sort = TRUE)
sch.inzidenz1$number<-seq(1,217,by=1)
tsc.inzidenz1$number<-seq(1,135,by=1)
inzidenz.four.lands2<-merge(sch.inzidenz1,tsc.inzidenz1,by ="number",all=TRUE,sort = TRUE)
inzidenz.four.lands1final<-merge(inzidenz.four.lands1,inzidenz.four.lands2,by="number",all=TRUE,sort = TRUE)

inzidenz.four.lands1final$New1 <- seq(1,224,by=1)
view(inzidenz.four.lands1final)

incidencefourland3 <- ggplot(data = inzidenz.four.lands1final, show.legend=TRUE)+ggtitle("7-Tage-InfektInzidenz(1. Welle)")+xlab("Tag")+ylab("7-Tage-Inzidenz")+
  geom_line(aes(x=number,y=Bayern),colour="red",show.legend =TRUE)+
  geom_line(aes(x=number,y=Belgien),colour="blue",show.legend =TRUE)+
  geom_line(aes(x=number,y=Schweden),colour="yellow",show.legend =TRUE)+
  geom_line(aes(x=number,y=Tschechien),colour="green",show.legend =TRUE)+
  scale_y_continuous(limits = c(0,1000))+
  scale_x_continuous(breaks = c(1,7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217),expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="35",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))

incidencefourland3



incidence07a2 <- subset(incidence7proEinwohner,Datum>="2020-08-03")
incidence07b2<- subset(incidence7proEinwohner,Datum>="2020-09-07")
incidence07c2<-subset(incidence7proEinwohner,Datum>="2020-07-13")

bay.inzidenz2<-as.data.frame(incidence07a2[,c(1,2)])
bel.inzidenz2<-as.data.frame(incidence07c2[,c(1,3)])
sch.inzidenz2<-as.data.frame(incidence07b2[,c(1,4)])
tsc.inzidenz2<-as.data.frame(incidence07c2[,c(1,5)])

bay.inzidenz2$number<-seq(1,150,by=1)
bel.inzidenz2$number<-seq(1,171,by=1)
inzidenz.four.lands3<-merge(bay.inzidenz2,bel.inzidenz2,by="number",all=TRUE,sort = TRUE)
sch.inzidenz2$number<-seq(1,115,by=1)
tsc.inzidenz2$number<-seq(1,171,by=1)
inzidenz.four.lands4<-merge(sch.inzidenz2,tsc.inzidenz2,by ="number",all=TRUE,sort = TRUE)
inzidenz.four.lands2final<-merge(inzidenz.four.lands3,inzidenz.four.lands4,by="number",all=TRUE,sort = TRUE)

incidencefourland4 <- ggplot(data = inzidenz.four.lands2final, show.legend=TRUE)+ggtitle("7-Tage-InfektInzidenz(2. Welle)")+xlab("Tag")+ylab("7-Tage-Inzidenz")+
  geom_line(aes(x=number,y=Bayern),colour="red",show.legend =TRUE)+
  geom_line(aes(x=number,y=Belgien),colour="blue",show.legend =TRUE)+
  geom_line(aes(x=number,y=Schweden),colour="yellow",show.legend =TRUE)+
  geom_line(aes(x=number,y=Tschechien),colour="green",show.legend =TRUE)+
  scale_y_continuous(limits = c(0,1000))+
  scale_x_continuous(breaks = c(1,7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182),expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="35",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))

incidencefourland4




# Belgien!=0 | Schweden!=0 |Tschechien!=0)
view(incidence7a)

#incidencefourland
mytheme <- theme(plot.title = element_text(size="20",color="brown",face = "bold"),
                 axis.text.x = element_text(angle = 45,hjust = 0.5,vjust = 0.5),
                 legend.box="vertical",
                 legend.position = "top",
                 #scale_color_manual(name = "Land", values=c("Bayern"="red", "Belgien"="blue", "Schweden"="green","Tschechien"="yellow")) ,
                 #scale_linetype_manual(name = "Land", values=c("Bayern"=2, "Belgien"=1, "Schweden"=9,"Tschechien"=2))
)
incidencefourland <- ggplot(data = incidence7, show.legend=TRUE)+ggtitle("Faelle pro Woche")+xlab("Meldewoche")+ylab("Faelle")+
  geom_line(aes(x=Datum,y=Bayern),colour="orange",show.legend =TRUE)+
  geom_line(aes(x=Datum,y=Belgien),colour="sky blue",show.legend =TRUE)+
  geom_line(aes(x=Datum,y=Schweden),colour="green",show.legend =TRUE)+
  geom_line(aes(x=Datum,y=Tschechien),colour="pink",show.legend =TRUE)+
  scale_x_date(date_breaks = "1 week")+
  scale_y_continuous(breaks=c(10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000))+
  scale_colour_discrete(name="Land",
                        breaks=c("Bayern","Belgien","Schweden","Tschechien"),
                        labels=c("Bayern","Belgien","Schweden","Tschechien"))+
  geom_hline(aes(yintercept=110000),colour="red",linetype="dashed")+
  mytheme
incidencefourland


incidencefourland2 <- ggplot(data = incidence7proEinwohner, show.legend=TRUE)+ggtitle("7-Tage-Inzidenz pro 100.000 Einwohner")+xlab("Meldewoche")+ylab("Faelle pro 100.000 Einwohner")+
  geom_line(aes(x=Datum,y=Bayern),colour="red",show.legend =TRUE)+
  geom_line(aes(x=Datum,y=Belgien),colour="blue",show.legend =TRUE)+
  geom_line(aes(x=Datum,y=Schweden),colour="yellow",show.legend =TRUE)+
  geom_line(aes(x=Datum,y=Tschechien),colour="green",show.legend =TRUE)+
  scale_x_date(date_breaks = "1 week")+
  scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800,900,1000))+
  scale_colour_discrete(name="Land",
                        breaks=c("Bayern","Belgien","Schweden","Tschechien"),
                        labels=c("Bayern","Belgien","Schweden","Tschechien"))+
  geom_hline(aes(yintercept=100),colour="red",linetype="dashed")+
  mytheme
incidencefourland2

incidence3 <- daily.incedence(Daten, end = "2020-11-14", intervall = 3, relative = FALSE)
View(incidence3)

incidence7abs <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = TRUE)
View(incidence7abs)


#Todeszahl
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


##Geschlecht
#Bayern
Data1<-(Daten$bavaria$rki$bavaria_rki)
sub <- subset(Data1,Bundesland=="Bayern")
ggplot(data = sub,aes(x=Geschlecht,y=AnzahlTodesfall))+
  geom_bar(stat = "identity",fill="skyblue",width =.6)+
  xlab("Geschlecht")+ylab("die Todeszahlen")+ggtitle("Todeszahlen ueber Geschlecht in Bayern")+
  scale_x_discrete(limits=c("W","M","unbekannt"),
                   labels=c("Weiblich","Maennlich","Unbekannt"))+
  theme(plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.x = element_text(angle=0, vjust=0.5, size=10,colour = "black"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))

#Todesfall in Bayern pro Tag
tod.bay <- as.data.frame(Tot[,c(1,4)])
ggplot(data = tod.bay,aes(as.Date(Datum),Bayern))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="30",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_x_date(date_breaks = "1 weeks")+
  scale_y_continuous(breaks=c(0,50,100,150,200,250),limits = c(0,250))+
  xlab("Datum")+ylab("Todesfaelle pro Tag")+ggtitle("Todesfaelle pro Tag in Bayern")

#Belgium
Data2 <- (Daten$belgium$belgium_mortality)
ggplot(data=Data2,aes(x=Geschlecht,y=Todesfälle))+
  geom_bar(stat = "identity",fill="skyblue",width =.6)+
  xlab("Geschlecht")+ylab("die Todeszahlen")+ggtitle("Todeszahlen ueber Geschlecht in Belgium")+
  scale_x_discrete(labels=c("Weiblich","Maennlich","Unbekannt"))+
  theme(plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.x = element_text(angle=0, vjust=0.5, size=10,colour = "black"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))

#Todesfall in Belgium pro Tag
tod.bel <- as.data.frame(Tot[,c(1,2)])
ggplot(data = tod.bel,aes(as.Date(Datum),Belgien))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="30",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_x_date(date_breaks = "1 weeks")+
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350),limits = c(0,350))+
  xlab("Datum")+ylab("Todesfaelle pro Tag")+ggtitle("Todesfaelle pro Tag in Belgien")


#Tschechien
Data3 <- (Daten$czech$czech_9)

ggplot(data=Data3,aes(x=Geschlecht))+
  geom_bar(stat = "count",fill="skyblue",width =.6)+
  xlab("Geschlecht")+ylab("die Todeszahlen")+ggtitle("Todeszahlen ueber Geschlecht in Tschechien")+
  scale_x_discrete(limits=c("Z","M","UN"),
                   labels=c("Weiblich","Maennlich","Unbekannt"))+
  theme(plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.x = element_text(angle=0, vjust=0.5, size=10,colour = "black"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))

#Todesfall in Tschechien pro Tag
tod.tsc <- as.data.frame(Tot[,c(1,3)])
ggplot(data = tod.tsc,aes(as.Date(Datum),Tschechien))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="30",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_x_date(date_breaks = "1 weeks")+
  scale_y_continuous(breaks=c(0,50,100,150,200,250),limits = c(0,250))+
  xlab("Datum")+ylab("Todesfaelle pro Tag")+ggtitle("Todesfaelle pro Tag in Tscheschien")


#Schweden
Data4 <- (Daten$sweden$sweden_5)

ggplot(data = Data4,aes(x=Geschlecht,y=Todesfälle))+
  geom_bar(stat = "identity",fill="skyblue",width =.6)+
  xlab("Geschlecht")+ylab("die Todeszahlen")+ggtitle("Todeszahlen ueber Geschlecht in Schweden")+
  scale_x_discrete(limits=c("Kvinna","Man","Uppgift sakans"),
                   labels=c("Weiblich","Maennlich","Unbekannt"))+
  theme(plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.x = element_text(angle=0, vjust=0.5, size=10,colour = "black"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))

#Todesfall in Schweden pro Tag
tod.sch <- as.data.frame(Tot[,c(1,5)])
ggplot(data = tod.sch,aes(as.Date(Datum),Schweden))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="30",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_x_date(date_breaks = "1 weeks")+
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350),limits = c(0,350))+
  xlab("Datum")+ylab("Todesfaelle pro Tag")+ggtitle("Todesfaelle pro Tag in Schweden")





#Todeszahlen Vergleich

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
ggplot(todesfall.vergleich,aes(Land, Todesfaelle))+
  geom_bar(stat="identity",position = "dodge",fill="purple")+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"))+
  ggtitle("Insgesamte Todesfallzahl in vier Laender")


#Todeszahl urber Geschlecht Vergleich

#Geschlecht Anteil

read.csv("data/bavaria/lgl/tabelle_08_20201221.csv")
sub1<-subset(Data2,Geschlecht=="F")
sum(sub1$Todesfälle)
sub2<-subset(Data2,Geschlecht=="M")
sum(sub2$Todesfälle)
count(Data3$Geschlecht)
Data4

#Bayern
bayernGeschlechtW <- c(2767/(2767+2914))
bayernGeschlechtM <- c(2914/(2767+2914))
percbayernW <- label_percent()(bayernGeschlechtW) #49%
percbayernM <- label_percent()(bayernGeschlechtM) #51%
#Belgien
belgienW <- c(9944)
belgienM <- c(9558)
percBelW01<- label_percent()(belgienW / (belgienW + belgienM))#51%
percBelM01<- label_percent()(belgienM / (belgienW + belgienM)) #49%

#Sweden
swedenW <- c(232394)
swedenM <- c(204842)
percSweM01<- label_percent()(swedenM / (swedenM + swedenW)) #47%
percSweW01<- label_percent()(swedenW / (swedenM + swedenW)) #53%

#Czech
czechW <- c(4944)
czechM <- c(6314)
percCzeW01<- label_percent()(czechW / (czechW + czechM)) #44%
percCzeM01<- label_percent()(czechM / (czechW + czechM)) #56%

mat_sex <- matrix(
  c(percbayernM, percBelM01, percSweM01, percCzeM01, 
    percbayernW, percBelW01, percSweW01, percCzeW01),
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
p + geom_col() + geom_hline(aes(yintercept=3.5),colour="red",linetype="dashed")+
  ggtitle("Anteil nach Geschleht Todeszahl")+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black",size = "16"),
        plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "20"),
        axis.title.y = element_text(colour = "brown",size = "20"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"))

# TodesZahl Matrix
Data1<-(Daten$bavaria$rki$bavaria_rki)
sub1<- subset(Data1,(Bundesland=="Bayern" & AnzahlTodesfall!= 0))
sub01 <- sub1 %>% select(AnzahlTodesfall,Meldedatum)
Data2 <- (Daten$belgium$belgium_mortality)
sub02 <- Data2 %>% select(Datum,Todesfälle)
Data3 <- (Daten$czech$czech_9)
Data03<- (Daten$czech$czech_1)
sub3 <- subset(Data03,Gesamttodeszahl != 0)
sub03 <- sub3 %>% select(Datum,Gesamttodeszahl)
Data04 <- read.csv("data/sweden/Schweden02.csv")

#Bayern
countsub.bay <- count(sub1, var='AnzahlTodesfall')
sum.bay<-sum(sub1$AnzahlTodesfall)
deadDayilyBayern <- sub01 %>% count('Meldedatum')

max.bay<-max(deadDayilyBayern$freq) #116
aaa01<-subset(deadDayilyBayern,Meldedatum<="2020/07/27")
aaa02<-subset(deadDayilyBayern,Meldedatum>="2020/07/27")
max.bay01<-max(aaa01$freq)
max.bay02<-max(aaa02$freq)

#Belgien
countsub.bel <- count(Data2,var='Todesfälle')
sum.bel<-sum(Data2$Todesfälle)
Data2 %>% group_by(Datum,Todesfälle)
sub200 <-summarise(sub002,sum_Todesfälle=sum(Todesfälle))

deadDailyBelgien<- sub02 %>% count("Datum")
deadDailyBelgien

max.bel<-max(countsub.bel$Todesfälle)
bbb01<- subset(Data2,Datum <= "2020-07-06")
bbb02<- subset(Data2,Datum >= "2020-07-06")
countsub.bel01 <- count(bbb01,var='Todesfälle')
countsub.bel02 <- count(bbb02,var='Todesfälle')
max.bel01<-max(countsub.bel01$Todesfälle)
max.bel02<-max(countsub.bel02$Todesfälle)

#Tschechien
countsub.cze <-count(Data3,var="Datum")
sum.cze<-sum(countsub.cze$freq)

max.cze<-max(countsub.cze$freq)
ccc01<- subset(countsub.cze,Datum<="2020-07-06")
ccc02<- subset(countsub.cze,Datum>="2020-07-06")
max.cze01<-max(ccc01$freq)
max.cze02<-max(ccc02$freq)

#Schweden
sum.sch<-sum(Data04$Todesfall)
max.sch<-max(Data04$Todesfall)
ddd01<- subset(Data04,Datum<="2020-08-31")
ddd02<- subset(Data04,Datum>="2020-08-31")
max.sch01<-max(ddd01$Todesfall)
max.sch02<-max(ddd02$Todesfall)



mat_sex <- matrix(
  c(sum.bay,sum.bel,sum.cze,sum.sch,
    max.bay,max.bel,max.cze,max.sch,
    max.bay01,max.bel01,max.cze01,max.sch01,
    max.bay02,max.bel02,max.cze02,max.sch02),
  nrow=4, ncol=4,
  dimnames = list(c("Bayern", "Belgien", "Tschechien", "Schweden"), c("Sum der Todesfallzahl","Maximum Todesfallzahl","Maximum.1.Welle","Maximum.2.Welle"))
)
knitr::kable(mat_sex)

#Kennwerte Aufgabe02
infektion01 <- subset(cases.per.day,Datum<="2020-08-31")
max(infektion01$Bayern)
max(infektion01$Belgien)
max(infektion01$Schweden)
max(infektion01$Tschechien)
infektion02<-subset(cases.per.day,Datum>="2020-08-31")
max(infektion02$Bayern)
max(infektion02$Belgien)
max(infektion02$Schweden)
max(infektion02$Tschechien)
infektion03 <- subset(cases.per.day,Datum>="2020-11-23")
max(infektion03$Tschechien)

inzidenz01 <- subset(incidence7proEinwohner,Datum<="2020-08-31")
max(inzidenz01$Bayern)
max(inzidenz01$Belgien)
max(inzidenz01$Schweden)
max(inzidenz01$Tschechien)
inzidenz02 <-subset(incidence7proEinwohner,Datum>="2020-08-31")
max(inzidenz02$Bayern)
max(inzidenz02$Belgien)
max(inzidenz02$Schweden)
max(inzidenz02$Tschechien)


#Balkendiagramm Fälle pro Tag Vorpräsentation 
#Anpassen der Daten 
cases.per.day.belgium <- select(cases.per.day, -Bayern, -Schweden, -Tschechien)
cases.per.day.bayern <- select(cases.per.day, -Belgien, -Schweden, -Tschechien) 
cases.per.day.schweden <- select(cases.per.day, -Bayern, -Belgien, -Tschechien) 
cases.per.day.tschechien <- select(cases.per.day, -Bayern, -Schweden, -Belgien) 

#Belgien
balken.belgien <- ggplot(cases.per.day.belgium, aes(Datum, Belgien))+
  geom_col(fill = "steelblue")+
  ggtitle("Belgien")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Faelle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")+
  scale_x_date(date_breaks = "1 month")

#Bayern
balken.bayern  <- ggplot(cases.per.day.bayern, aes(Datum, Bayern))+
  geom_col(fill = "steelblue")+
  ggtitle("Bayern")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Faelle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")+
  scale_x_date(date_breaks = "1 month")                  

#Tschechien
balken.tschechien <- ggplot(cases.per.day.tschechien, aes(Datum, Tschechien))+
  geom_col(fill = "steelblue")+
  ggtitle("Tschechien")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Faelle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")+
  scale_x_date(date_breaks = "1 month")

#Schweden
balken.schweden <- ggplot(cases.per.day.schweden, aes(Datum, Schweden))+
  geom_col(fill = "steelblue")+
  ggtitle("Schweden")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Faelle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")+
  scale_x_date(date_breaks = "1 month")


#Todincidence pro 7 Tag
Todincidence7proW<-read.csv("Todesfälle_vergleichbar.csv")
Todincidence07a <- subset(Todincidence7proW,Datum<="2020-08-03")
Todincidence07b<- subset(Todincidence7proW,Datum<="2020-09-07")
Todincidence07c<-subset(Todincidence7proW,Datum<="2020-07-13")
bay.todinzidenz1<-as.data.frame(Todincidence07a[,c(2,3)])
bel.todinzidenz1<-as.data.frame(Todincidence07c[,c(2,4)])
sch.todinzidenz1<-as.data.frame(Todincidence07b[,c(2,5)])
tsc.todinzidenz1<-as.data.frame(Todincidence07c[,c(2,6)])

bay.todinzidenz1$number<-seq(1,147,by=1)
bel.todinzidenz1$number<-seq(1,126,by=1)
Todinzidenz.four.lands1<-merge(bay.todinzidenz1,bel.todinzidenz1,by="number",all=TRUE,sort = TRUE)
sch.todinzidenz1$number<-seq(1,182,by=1)
tsc.todinzidenz1$number<-seq(1,126,by=1)
Todinzidenz.four.lands2<-merge(sch.todinzidenz1,tsc.todinzidenz1,by ="number",all=TRUE,sort = TRUE)
Todinzidenz.four.lands1final<-merge(Todinzidenz.four.lands1,Todinzidenz.four.lands2,by="number",all=TRUE,sort = TRUE)



Todincidencefourland3 <- ggplot(data = Todinzidenz.four.lands1final, show.legend=TRUE)+ggtitle("7-Tage-TodesInzidenz (1. Welle)")+xlab("Tag")+ylab("7-Tage-Inzidenz")+
  geom_line(aes(x=number,y=Bayern),colour="red",show.legend =TRUE)+
  geom_line(aes(x=number,y=Belgien),colour="blue",show.legend =TRUE)+
  geom_line(aes(x=number,y=Schweden),colour="green",show.legend =TRUE)+
  geom_line(aes(x=number,y=Tschechien),colour="yellow",show.legend =TRUE)+
  scale_y_continuous(breaks = c(0.00,0.25,0.50,0.75,1.00,1.25),limits = c(0.00,1.25))+
  scale_x_continuous(breaks = c(1,7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182),expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="35",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))

Todincidencefourland3



Todincidence07a2<- subset(Todincidence7proW,Datum >= "2020-08-03")
Todincidence07b2<- subset(Todincidence7proW,Datum >= "2020-09-07")
Todincidence07c2<-subset(Todincidence7proW,Datum >= "2020-07-13")
bay.todinzidenz2<-as.data.frame(Todincidence07a2[,c(2,3)])
bel.todinzidenz2<-as.data.frame(Todincidence07c2[,c(2,4)])
sch.todinzidenz2<-as.data.frame(Todincidence07b2[,c(2,5)])
tsc.todinzidenz2<-as.data.frame(Todincidence07c2[,c(2,6)])

bay.todinzidenz2$number<-seq(1,157,by=1)
bel.todinzidenz2$number<-seq(1,178,by=1)
Todinzidenz.four.lands2.1<-merge(bay.todinzidenz2,bel.todinzidenz2,by="number",all=TRUE,sort = TRUE)
sch.todinzidenz2$number<-seq(1,122,by=1)
tsc.todinzidenz2$number<-seq(1,178,by=1)
Todinzidenz.four.lands2.2<-merge(sch.todinzidenz2,tsc.todinzidenz2,by ="number",all=TRUE,sort = TRUE)
Todinzidenz.four.lands2final<-merge(Todinzidenz.four.lands2.1,Todinzidenz.four.lands2.2,by="number",all=TRUE,sort = TRUE)



Todincidencefourland4 <- ggplot(data = Todinzidenz.four.lands2final, show.legend=TRUE)+ggtitle("7-Tage-TodesInzidenz (2. Welle)")+xlab("Tag")+ylab("7-Tage-Inzidenz")+
  geom_line(aes(x=number,y=Bayern),colour="red",show.legend =TRUE)+
  geom_line(aes(x=number,y=Belgien),colour="blue",show.legend =TRUE)+
  geom_line(aes(x=number,y=Schweden),colour="yellow",show.legend =TRUE)+
  geom_line(aes(x=number,y=Tschechien),colour="green",show.legend =TRUE)+
  scale_y_continuous(breaks = c(0.00,0.25,0.50,0.75,1.00,1.25),limits = c(0.00,1.25))+
  scale_x_continuous(breaks = c(1,7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182),expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 45,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="35",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))
Todincidencefourland4

subTodInzi<-subset(Todincidence7proW,Datum<="2020-01-06")
deathincidencefourland <- ggplot(show.legend=TRUE) +
  geom_line(data = Todincidence7proW, mapping = aes(x=as.Date(Datum), y=Bayern), colour="red", size = 0.5,show.legend = TRUE) +
  geom_line(data = Todincidence7proW, mapping = aes(x=as.Date(Datum), y=Belgien), colour="blue", size = 0.5,show.legend = TRUE)+
  geom_line(data = Todincidence7proW, mapping = aes(x=as.Date(Datum), y=Schweden), colour="yellow", size = 0.5,show.legend = TRUE)+
  geom_line(data = Todincidence7proW, mapping = aes(x=as.Date(Datum), y=Tschechien), colour="green", size = 0.5,show.legend = TRUE)+
  ggtitle("TodesInzidenz")+
  scale_y_continuous(breaks = c(0.00,0.25,0.50,0.75,1.00,1.25),limits = c(0.00,1.25))+
  scale_x_date(date_breaks = "1 week",expand = c(0, 0))+
  theme(axis.text.x = element_text(angle = 45,hjust =1,vjust = 1,colour = "black"),
        plot.title = element_text(size="30",lineheight =.6,face = "bold",colour = "black",hjust = 0.5),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))+
  xlab("Datum") + ylab("TodesInzidenz")

deathincidencefourland

#Todesfaelleentwirklung
Todesa01 <- subset(Tot,Datum<="2020-08-03")
Todesb01<- subset(Tot,Datum<="2020-09-07")
Todesc01<-subset(Tot,Datum<="2020-07-13")
bay.tod1<-as.data.frame(Todesa01[,c(1,4)])
bel.tod1<-as.data.frame(Todesc01[,c(1,2)])
sch.tod1<-as.data.frame(Todesb01[,c(1,5)])
tsc.tod1<-as.data.frame(Todesc01[,c(1,3)])


bay.tod1$number<-seq(1,147,by=1)
bel.tod1$number<-seq(1,126,by=1)
Tod.four.lands1<-merge(bay.tod1,bel.tod1,by="number",all=TRUE,sort = TRUE)
sch.tod1$number<-seq(1,182,by=1)
tsc.tod1$number<-seq(1,126,by=1)
Tod.four.lands2<-merge(sch.tod1,tsc.tod1,by ="number",all=TRUE,sort = TRUE)
Tod.four.lands1final<-merge(Tod.four.lands1,Tod.four.lands2,by="number",all=TRUE,sort = TRUE)

Tod.Belgien1<- ggplot(data = bel.tod1, show.legend=TRUE)+ggtitle("Todesfaelle in Belgien pro Tag (1. Welle)")+xlab("Tag")+ylab("Todesfaelle")+
  #geom_line(aes(x=number,y=Bayern),color="red",show.legend =TRUE)+
  geom_line(aes(x=number,y=Belgien),color="blue",show.legend =TRUE)+
  #geom_line(aes(x=number,y=Schweden),color="yellow",show.legend =TRUE)+
  #geom_line(aes(x=number,y=Tschechien),color="green",show.legend =TRUE)+
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350),limits = c(0,350))+
  scale_x_continuous(breaks = c(1,7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182),expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="30",lineheight =.6,face = "bold",colour = "black",hjust=0.5),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))

Tod.Belgien1


Tod.fourland3 <- ggplot(data = Tod.four.lands1final, show.legend=TRUE)+ggtitle("Todesfaelle pro Tag (1. Welle)")+xlab("Datum")+ylab("Todesfaelle")+
  geom_line(aes(x=number,y=Bayern),color="red",show.legend =TRUE)+
  geom_line(aes(x=number,y=Belgien),color="blue",show.legend =TRUE)+
  geom_line(aes(x=number,y=Schweden),color="yellow",show.legend =TRUE)+
  geom_line(aes(x=number,y=Tschechien),color="green",show.legend =TRUE)+
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350),limits = c(0,350))+
  scale_x_continuous(breaks = c(1,7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182),expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="35",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))

Tod.fourland3


Todesa02<- subset(Tot,Datum>="2020-08-03")
Todesb02<- subset(Tot,Datum>="2020-09-07")
Todesc02<-subset(Tot,Datum>="2020-07-13")
bay.tod2<-as.data.frame(Todesa02[,c(1,4)])
bel.tod2<-as.data.frame(Todesc02[,c(1,2)])
sch.tod2<-as.data.frame(Todesb02[,c(1,5)])
tsc.tod2<-as.data.frame(Todesc02[,c(1,3)])

bay.tod2$number<-seq(1,157,by=1)
bel.tod2$number<-seq(1,178,by=1)
Tod.four.lands3<-merge(bay.tod2,bel.tod2,by="number",all=TRUE,sort = TRUE)
sch.tod2$number<-seq(1,122,by=1)
tsc.tod2$number<-seq(1,178,by=1)
Tod.four.lands4<-merge(sch.tod2,tsc.tod2,by ="number",all=TRUE,sort = TRUE)
Tod.four.lands2final<-merge(Tod.four.lands3,Tod.four.lands4,by="number",all=TRUE,sort = TRUE)

Tod.Belgien2<- ggplot(data = bel.tod2, show.legend=TRUE)+ggtitle("Todesfaelle in Belgien pro Tag (2. Welle)")+xlab("Tag")+ylab("Todesfaelle")+
  #geom_line(aes(x=number,y=Bayern),color="red",show.legend =TRUE)+
  geom_line(aes(x=number,y=Belgien),color="blue",show.legend =TRUE)+
  #geom_line(aes(x=number,y=Schweden),color="yellow",show.legend =TRUE)+
  #geom_line(aes(x=number,y=Tschechien),color="green",show.legend =TRUE)+
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350),limits = c(0,350))+
  scale_x_continuous(breaks = c(1,7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182),expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="30",lineheight =.6,face = "bold",colour = "black",hjust=0.5),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))

Tod.Belgien2

Tod.fourland4 <- ggplot(data = Tod.four.lands2final, show.legend=TRUE)+ggtitle("Todesfaelle pro Tag (2. Welle)")+xlab("Datum")+ylab("Todesfaelle")+
  geom_line(aes(x=number,y=Bayern),colour="red",show.legend =TRUE)+
  geom_line(aes(x=number,y=Belgien),colour="blue",show.legend =TRUE)+
  geom_line(aes(x=number,y=Schweden),colour="yellow",show.legend =TRUE)+
  geom_line(aes(x=number,y=Tschechien),colour="green",show.legend =TRUE)+
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350),limits = c(0,350))+
  scale_x_continuous(breaks = c(1,7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182),expand = c(0,0),limits = c(1,182))+
  theme(axis.text.x = element_text(angle = 0,hjust = 0.5,vjust = 1,colour = "black"),
        plot.title = element_text(size="35",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))

Tod.fourland4

deathcasesfourland <- ggplot(show.legend=TRUE) +
  geom_line(data = Tot, mapping = aes(x=as.Date(Datum), y=Bayern), colour="red", size = 0.5,show.legend = TRUE) +
  geom_line(data = Tot, mapping = aes(x=as.Date(Datum), y=Belgien), colour="blue", size = 0.5,show.legend = TRUE)+
  geom_line(data = Tot, mapping = aes(x=as.Date(Datum), y=Schweden), colour="yellow", size = 0.5,show.legend = TRUE)+
  geom_line(data = Tot, mapping = aes(x=as.Date(Datum), y=Tschechien), colour="green", size = 0.5,show.legend = TRUE)+
  ggtitle("Todesentwicklungen pro Tag in vier Länder")+
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350),limits = c(0,350))+
  scale_x_date(date_breaks = "1 week")+
  theme(axis.text.x = element_text(angle = 45,hjust =1,vjust = 1,colour = "black"),
        plot.title = element_text(size="30",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "25"),
        axis.title.y = element_text(colour = "black",size = "25"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"),
        plot.margin=unit(rep(1,4),'lines'))+
  xlab("Datum") + ylab("Todesfaelle")

deathcasesfourland

