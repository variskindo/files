#### Hausaufgabe 3 ####

### Aufgabe 1
##a

Footprint <- data.frame(
  c("Bier", "Kaffee", "Milch", "Rindfleisch", "Papier", "Baumwolle"),
  paste(1, rep(c("l", "kg"), each=3), sep=""),
  c(1.63, 1, .33, 39.87, .08, 1.29),
  c(300, 700,1000,15500,750,10000),
  c(.3, .5, 8, 12, 0, 1.81),
  c(100.1, 162, 80, 9, 244, 12.25)
)

colnames(Footprint) = c("Produkt", "Einheit", "Preis in EUR", 
                        "Wasserverbrauch in l", "CO2 in kg", 
                        "Jährlicher Pro-Kopf-Verbrauch Dtl.")

##b

Footprint <- cbind(Footprint, 
                   "Jährlicher Pro-Kopf-Verbrauch USA" = c(73.5, 106, 70, 11.25, 211, 12.25))

##c
Footprint[1][Footprint[7]==as.integer(Footprint$`Jährlicher Pro-Kopf-Verbrauch USA`)]

##d
Footprint[1][Footprint[3]<1 | Footprint[3]>10]

##e

Abweichung <- Footprint[4]-mean(Footprint$`Wasserverbrauch in l`) #Maximale Abweichung vom MW
Footprint[1][which(Abweichung==max(Abweichung), arr.ind=T)]       #[1] "Rindfleisch"


##f
List <- split(Footprint, Footprint$Einheit)



### Aufgabe 2
##a
  
setwd("C://Users/sarah/Google Drive/Uni/Zwischenjahr/R Seminar/Hausaufgaben")
Daten <- read.csv2("Patentanmeldungen.csv", check.names=F)
attach(Daten)

##b
row.names(Daten) <- Daten$Bundesland
Daten <- Daten[-1]
Daten<- Daten[!(as.integer(colnames(Daten))<2010)]

##c
neue <- c(4,8,13,14,16)
sum(Daten[neue,"2018"])     #[1] 1780

##d
Daten <- Daten[order(Daten$`2010`, decreasing=T), ]

##e
detach(Daten)
Daten2 <- read.csv2("Markenanmeldungen.csv", check.names=F)

##f
Daten <- Daten[order(row.names(Daten)),]
Daten2 <- Daten2[order(Daten2$Bundesland),]
merge(Daten, Daten2)
Anmeldungen <- data.frame(Daten[7:9]+Daten2[13:15])



### Aufgabe 3
##a
paste(month.name, "is the", as.Date(1:12), rep(c("st", "nd", "rd", "th"), c(1,1,1,9)), "Month of the year.")

##b
X <- c("Ni", "Um", "Ree", "Kap", "Pu", "Fi", "Hum", "Adi")
Y <- c("bok", "la", "ke", "bro", "das", "pa", "mel", "ma")

paste(X[1:8], Y[c(3,4,1,6,8,2,7,5)], sep="")

##c
C <- c("a;s;d;f;g;h;j;k;l")
C<- strsplit(C, ";")
C        #[1] "a" "s" "d" "f" "g" "h" "j" "k" "l"


##d


##e
Heidi <- "habe kein Ich Foto heute für leider dich."
Heidi <- strsplit(Heidi, " ")
richtig <- c(3, 1, 5, 7, 2, 4, 6, 8)
cat(Heidi[[1]][richtig])

##f
