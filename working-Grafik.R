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

sort(cases.per.day$Bayern)
order(cases.per.day$Bayern)
order(cases.per.day$Belgien)
order(cases.per.day$Schweden)
order(cases.per.day$Tschechien)

bayern.day.cases <- ggplot(data = cases.per.day, mapping = aes(x=Datum, y=Bayern))
bayern.day.cases + geom_line(colour="red")

belgien.day.cases <- ggplot(data = cases.per.day, mapping = aes(x=Datum, y=Belgien))
belgien.day.cases + geom_smooth(colour="blue")

casesfourland <- ggplot() +
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Bayern),end("2020-06-14"),colour="red") +
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Belgien), colour="blue")+
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Schweden), colour="yellow")+
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Tschechien), colour="green")+
  scale_x_date(date_breaks = "1 week")+
  theme(axis.text.x = element_text(angle = 45,hjust = 0.5,vjust = 0.5))

casesfourland

cases.per.day.relative100000 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE)
View(cases.per.day.relative100000)

cases.per.day.relative1 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE, reverenz = 1)
View(cases.per.day.relative1)


##Bilden von Inzidenz Tabelle (Beispiele)
incidence30 <- daily.incedence(Daten, end = "2020-11-14", intervall = 30, relative = FALSE)
View(incidence30)

incidence7 <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = FALSE)
View(incidence7)


#incidencefourland
mytheme <- theme(plot.title = element_text(size="20",color="brown",face = "bold"),
                 axis.text.x = element_text(angle = 45,hjust = 0.5,vjust = 0.5),
                 legend.box="vertical",
                 legend.position = "top",
                 #scale_color_manual(name = "Land", values=c("Bayern"="red", "Belgien"="blue", "Schweden"="green","Tschechien"="yellow")) ,
                 #scale_linetype_manual(name = "Land", values=c("Bayern"=2, "Belgien"=1, "Schweden"=9,"Tschechien"=2))
                 
)
incidencefourland <- ggplot(data = incidence7, show.legend=TRUE)+ggtitle("das Inzidenz in einer Woche")+xlab("Pro Woche")+ylab("Inzidenz pro Woche")+
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


incidence3 <- daily.incedence(Daten, end = "2020-11-14", intervall = 3, relative = FALSE)
View(incidence3)

incidence7abs <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = TRUE)
View(incidence7abs)

#Todeszahlen jedes Land

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
ggplot(data = sub,aes(as.Date(Meldedatum),AnzahlTodesfall))+
  geom_bar(stat="identity",fill="purple")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_x_date(date_breaks = "1 weeks")+
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500),expand = c(0,0))+
  xlab("Date")+ylab("AnzahlTodesfall")+ggtitle("Todeszahl pro Tag in Bayern")

sum(sub,Geschleicht='M')

max(sub$AnzahlTodesfall)
aaa <-subset(sub,Meldedatum<="2020/06/14")
max(aaa$AnzahlTodesfall)
mean(aaa$AnzahlTodesfall)
mean(sub$AnzahlTodesfall,Meldedatum >= "2020/06/14")
#Belgium
Data2 <- (Daten$belgium$belgium_mortality)
ggplot(data=Data,aes(x=SEX,y=DEATHS))+
  geom_bar(stat = "identity",fill="skyblue",width =.6)+
  xlab("Geschlecht")+ylab("die Todeszahlen")+ggtitle("Todeszahlen ueber Geschlecht in Belgium")+
  scale_x_discrete(labels=c("Weiblich","Maennlich","Unbekannt"))+
  theme(plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.x = element_text(angle=0, vjust=0.5, size=10,colour = "black"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))
 
#Todesfall in Belgium pro Tag
ggplot(data = Data2,aes(as.Date(Datum),Todesfälle))+
  geom_bar(stat="identity",fill="purple")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_x_date(date_breaks = "1 weeks")+
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500),expand = c(0,0))+
  xlab("Date")+ylab("AnzahlTodesfall")+ggtitle("Todeszahl pro Tag in Belgium")

mean(Data2$Todesfälle)
bbb <- subset(Data2,Datum >= "2020-07-06")
max(bbb$Todesfälle)
mean(bbb$Todesfälle)

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
ggplot(data = Data3,aes(x=as.Date(Datum)))+
  geom_bar(stat="count",fill="purple")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_x_date(date_breaks = "1 weeks")+
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500),expand = c(0,0))+
  xlab("Date")+ylab("AnzahlTodesfall")+ggtitle("Todeszahl pro Tag in Tschechien")

ccc<- subset(count(Data3$Datum),x>="2020-06-01")
count(Data3$Geschlecht)
max(ccc$freq)
mean(ccc$freq)

#Schweden
Data4 <- (Daten$sweden$sweden_2)

ggplot(data = Data4,aes(x=Kön,y=Totalt_antal_avlidna))+
  geom_bar(stat = "identity",fill="skyblue",width =.6)+
  xlab("Geschlecht")+ylab("die Todeszahlen")+ggtitle("Todeszahlen ueber Geschlecht in Bayern")+
  scale_x_discrete(limits=c("Kvinna","Man","Uppgift sakans"),
                   labels=c("Weiblich","Maennlich","Unbekannt"))+
  theme(plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.x = element_text(angle=0, vjust=0.5, size=10,colour = "black"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))

#Todesfall in Schweden pro Tag
Data04 <- read.csv("data/sweden/Schweden02.csv")
ggplot(data = Data04,aes(as.Date(Datum),Todesfall))+
  geom_bar(stat="identity",fill="purple")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="20",lineheight =.6,face = "bold",colour = "brown"),
        axis.title.x = element_text(colour = "brown",size = "16"),
        axis.title.y = element_text(colour = "brown",size = "16"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"))+
  scale_x_date(date_breaks = "1 weeks")+
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70),expand = c(0,0))+
  xlab("Date")+ylab("AnzahlTodesfall")+ggtitle("Todeszahl pro Tag in Schweden")

mean(Data04$Todesfall)
ddd <- subset(Data04,Datum>="2020-08-31")
max(ddd$Todesfall)
mean(ddd$Todesfall)


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
  









