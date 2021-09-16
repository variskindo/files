# Die Beispiele in dieser Datei sollen beim Einstieg
# in die Statistik-Software R (https://www.r-project.org/) helfen.

# R ist fuer Windows, Mac und Linux frei erhaeltlich,
# z.B. auf folgenden servern: 
# https://cran.uni-muenster.de/
# http://ftp5.gwdg.de/pub/misc/cran/
# http://mirrors.softliste.de/cran/

# Wer eine Linux-Distribution (z.B. Ubuntu, Debian, Suse, RedHat,...) auf dem
# Rechner hat, braucht in der Regel nicht ueber diese Server zu gehen, sondern
# kann R auf die uebliche Art und Weise als Softwarepaket auswaehlen und
# installieren.

# Zeilen, die mit dem selben Zeichen wie diese Zeile beginnen,
# sind Kommentarzeilen.
# Sie werden von R ignoriert und dienen dem Benutzer als Notiz.

# Dieses R Skript soll Zeile fuer Zeile ausgefuehrt werden. Wir empfehlen dazu
# die Verwendung des Programms RStudio, das auf https://www.rstudio.com/ fuer
# alle gaengigen Computersysteme verfuegbar ist (oder eben auch wieder als
# Linux-Paket).

# Sollte es Schwierigkeiten mit RStudio geben, kann R folgendermassen ohne
# RStudio verwendet werden. Auf Windows: Oeffne das Skript ueber das Menue
# (Datei -> Skript oeffnen), und fuehre im Skript-Fenster mit der
# Tastenkombination Ctrl-R Zeile fuer Zeile aus.  Auf Macintosh: OEffne das
# Skript ueber das Menue und fuehre im Skript-Fenster mit der Tastenkombination
# CMD-Return Zeile fuer Zeile aus.  Beliebiges System mit Emacs, bei dem "Emacs
# Speaks Statistics" installiert ist (siehe http://ess.r-project.org/): OEffne
# das Skript mit Emacs. Ist der Cursor in Emacs in einem Skript-Fenster, so
# erscheint eine Schaltflaeche, um einzelne Zeilen auszufuehren.  Beliebiges
# System: Kopiere (copy-paste) Zeile fuer Zeile in die R Konsole (suche nach
# '>') Zusaetzlich koennen zwischendurch bei Bedarf eigene Befehle in der R
# Konsole ausgefuehrt werden.

# Durch schnelles Durchklicken durch die Befehle lernt man nichts, also bei
# jedem Befehl mitdenken. Dies erfordert einiges an Zeit.  Pausen
# zwischendurch sind empfehlenswert.

###############################################################
###############################################################
##
## Abschnitt 1:
## Erste Schritte: R als Taschenrechner

3+4
3+2*2          # Selbstverstaendlich 'Punkt vor Strich'
(3+2)*2
exp(1)         # 'exp()' ist die Exponentialfunktion.
exp(log(5))    # 'log()' ist der natuerliche Logarithmus
log(8,base=2)  # Logarithmus zur Basis 2
sqrt(9)        # Die Wurzelfunktion (SQuare RooT)
sqrt(8)
3^3
9^(1/2)        # Ebenfalls die Wurzel von 9
1.2            # Dezimalzahlen werden mit '.' dargestellt
1,2            # Die deutsche Notation fuehrt zu einem Fehler

## Uebung 1a: Berechnen Sie den dekadischen Logarithmus von 4711.

## Uebung 1b: Berechnen Sie die dritte Wurzel von 27.

## Uebung 1c: Berechnen Sie das Produkt der Summe von 3 und 5 und der
## Differenz der Wurzel von 121 und der dritten Potenz von 1,5

#########################
## Abschnitt 2:
## Hilfe:

help("log")    # Zeige die Hilfeseite zum Befehl 'log()' an.
?sin           # Dasselbe wie help("sin")
help.start()   # Startet den Browser mit einer Html-Seite zu verschiedenen Manuals
help.search("t-Test") # Listet alle Befehle auf, die etwas mit 't-Test' zu tun haben.

## Uebung 2a: Finden Sie heraus, wie man mit R den Cosinus von 17 Grad berechnet.

## Uebung 2b: Finden Sie einen Winkel, der den Cosinus 0,7 hat. Finden Sie mit
## dem R-Onlinehilfesystem heraus, wie das geht.

## Uebung 2c: Berechnen Sie die dritte Wurzel von 125

#########################
## Abschnitt 3:
## Variablennamen

a <- 3         # Die Variable 'a' hat nun den Wert '3'
a
b <- 4 
b
a+b
b = 5          # dasselbe wie b <- 5, ist aber unuebersichtlich
b
4 -> b         # sogar das geht, ist aber unuebersichtlich
b

## Uebung 3a: Teilen Sie 23 durch 14 und speichern Sie das Ergebnis in einer
## Variablen. Multiplizieren Sie die Variable anschliessend mit 14.

## Uebung 3b: Addieren Sie 10 zu 3 und speichern Sie das Ergebnis in einer
## Variablen. Addieren Sie anschliessend 2 zu dieser Variablen.

#########################
## Abschnitt 4:
## Vektoren

## Jede Zusammenfassung von Dingen gleicher Art wird in R als 'Vektor' bezeichnet
## Benutze c() (c wie "concatenate", "coerce", "combine",...),
## um Dinge zu einem "Vektor" zusammenzufassen:

c(1,2,4)               # Vektor von (ganzen) Zahlen
x <- c(1,2.3,pi)       # Vektor von (reellen) Zahlen
class(x)               # Was ist der Typ des Objektes x?
c( c(1,2), c(3,pi,4) ) # c() fuegt mehrere Vektoren zu einem Vektor zusammen
z <- c( "a", "Zeichenkette", "Dies ist ein Text")
z                      # Vektor von Zeichenketten
class(z)               # Der Typ von z ist 'character', also eine Zeichenkette

# Grundrechenarten und Funktionen wirken auf Vektoren elementweise.
4*c(1,2,4)          # ergibt den Vektor (4*1,4*2,4*4)
c(1,2,4)+c(2,3,5)   # ergibt den Vekotr (1+2,2+3,4+5)
c(1,2)+c(2,3,1,5)   # dasselbe wie c(1,2,1,2)+c(2,3,1,5)
                    #    Sind die Vektorenl}ngen unterschiedlich, so wird der kuerzere
                    #    periodisch fortgesetz.
c(1,2,4)*c(2,3,1)   # ergibt den Vektor (1*2,2*3,4*1) (NICHT DAS SKALARPRODUKT!)
c(1,2,4)%*%c(2,3,1) # ergibt das Skalarprodukt der beiden Vektoren
c(1,2,3)^2          # ergibt den Vektor (1^2,2^2,3^2)
c(1,2,3,4,5)^c(2,3) # ergibt den Vektor (1^2,2^3,3^2,4^3,5^2); ignoriere die Warnung
exp(c(1,4,log(2)))  # ergibt (exp(1), exp(4), 2)

## Nuetzliche Zahlenfolgen erzeugen:
1:5                 # dasselbe wie c(1,2,3,4,5)
(1:5)*2             # dasselbe wie c(1,2,3,4,5)*2
rep(3,5)            # dasselbe wie c(3,3,3,3,3); 'rep' ist kurz fuer 'replicate'
rep( c(2,7) ,3)     # ergibt c(2,7,2,7,2,7); repliziere c(2,7) dreimal
seq(from=0,to=1,by=0.1) # Sequenz von 0 bis 1 in 0.1-Schritten
                    # ergibt c(0,0.1,0.2,0.3,...,0.9,1)

## Uebung 4a: Lassen Sie sich von R die 7er-Reihe bis 700 ausgeben.

## Uebung 4b: Berechnen Sie die Quadratwurzeln aller ganzen Zahlen von 1 bis 1000.

## Uebung 4c: Erzeugen Sie einen Vektor v, der alle negativen ganzen Zahlen
## enthaelt, die groesser als -100 sind, sowie alle positiven ganzen Zahlen,
## die kleiner als 100 sind, und alle Vielfache von 0,3 zwischen 30 und 40.

## Komponentenweise Vergleiche
x <- c(1,3,5,3)
x == 3              # komponentenweiser Test auf Gleichheit
                    #    ergibt (FALSE,TRUE,FALSE,TRUE)
x > 3               # ergibt (FALSE,FALSE,TRUE,FALSE)
x != 5              # komponentenweiser Test auf Ungleichheit
                    #    ergibt (TRUE,TRUE,FALSE,TRUE)

# Uebung 4d: Ueberpruefen Sie, ob der natuerliche Logarithmus von 2 kleiner
# ist als der Sinus von 43 Grad.

## Uebung 4e: Ueberpruefen Sie mit R, ob es natuerliche Zahlen n zwischen 1
## und 100 gibt, fuer die n^2 um mehr als 500 groesser ist als die vierte
## Potenz von n-50.

## Indizierung von Vektoren
x <- c(3:6)
x
x[4]                # 4. Element des Vektors x
x[c(2,4)]           # 2. und 4. Element; x[2,4] funktioniert nicht
x[c(FALSE,FALSE,TRUE,TRUE)] # dasselbe wie x[c(3,4)]
                    # Indizierung mit TRUE/FALSE Vektor
x[x>4]              # Beachte x>4 ist gleich (FALSE,FALSE,TRUE,TRUE)
x[x>=4]             # groesser oder gleich
x[x<=4]             # kleiner oder gleich
x[x>5 | x<4]        # groesser als 5 oder kleiner als 4
x[x<=5 & x>3]       # kleiner oder gleich 5 und groesser als 3
## Indizierung mit Wahr/Falsch-Aussagen ist unheimlich praktisch;
# Beispiel: Größen in mm von Fruchtfliegen, die an zwei Orten gefangen wurden
groesse <- 1.70 + seq(from=0.01,to=0.1,by=0.01)
fangort <- rep(c("Kathmandu","Bangkok"),5)
groesse[fangort=="Bangkok"]
groesse[fangort=="Bangkok" & groesse<1.75]


# Uebung 4f: Lassen Sie alle Groessen von Fliegen ausgeben, die aus
# Kathmandu stammen und mindestens Groesse 1.75 haben.


#########################
## Abschnitt 5:
## Mittelwert, Varianz und Standardabweichung einer Stichprobe
x <- c(4:6,0,-5)
x
length(x)    # Laenge des Vektors
sum(x)       # Summe des Vektors: 4+5+6+0-5
mean(x)      # Mittelwert: sum(x)/length(x)
             #   (4+5+6+0-5)/5
var(x)       # Varianz: sum( (x-mean(x))^2 ) / (length(x)-1)
             #   ( (4-1)^2+(5-1)^2+(6-1)^2+(0-1)^2+(-5-1)^2 )/4
sd(x)        # Standardabweichung: sqrt( var(x) )
median(x)    # Die Haelfte der Werte liegt unterhalb, die Haelfte
             #   der Werte liegt oberhalb eines Medians

# Uebung 5a: Berechnen Sie Mittelwert und Standardabweichung des Vektors aller
# geraden Zahlen zwischen 1 und 99.

## Uebung 5b: Berechnen Sie Median, Mittelwert und Varianz des Vektors v, den
## Sie in Aufgabe 4c definert haben.

#########################
## Abschnitt 6:
## Plotten
x <- seq(from=0,to=1,by=0.1)
plot(x,col="red")
plot(x,x^2,col="red")
plot(x,x^2,col="red",pch=16)   # Als point character: 16 steht fuer volle Kreise
plot(x,x^2,col="red",type="l") # Als plot-Typ: Linien statt Punkte
plot(sin,from=-3,to=3)         # Plotte die Sinus-Funktion zwischen -3 und 3
abline(v=2)                    # Fuegt eine vertikale Linie durch (2,0) hinzu
abline(h=1)                    # Fuegt eine horizontale Linie durch (0,1) hinzu

# Uebung 6a: Stellen Sie die Funktion f(x)=3x^2-2x-4 im Bereich von -1 bis 5
# graphisch dar.

## Uebung 6b: Visualisieren Sie die Grafen der Funktionen f(x)=2*x^2-30*x+5
## und g(x)=(x-10)^3 mit zwei verschiedenen Farben im selben
## Koordinatensystem. Informieren Sie sich dazu ueber die Funktionen "points"
## und "lines" im Online-Hilfesystem. Waehlen Sie einen interessanten Bereich
## fuer die x-Achse.

## Uebung 6c: Erzeugen Sie eine Abbildung, die als graphische Loesung von
## Uebung 4e aufgefasst werden kann.


#########################
## Abschnitt 7:
## Datenstrukturen

# Statistische Daten werden in R als data frames gespeichert.

bavaria <- data.frame(city=c("Munich","Nuremberg","Augsburg"),
                      area=c(310.43,186.38,146.93),
                      popul=c(1356594,500132,263477))

bavaria              # Beachte die Tabellenstruktur wie in Excel
bavaria$city         # Zugriff auf Spalten mit dem '$'-operator
bavaria$area
class(bavaria$city)  ## Ab R version 4.0 ist das 'character',
                     ## in aelteren Versionen wird das durch Einlesen mit
                     ## data.frame() zu 'factor'; dazu spaeter mehr
class(bavaria$area)
bavaria$area[1]      # bavaria$area ist ein Vektor, Zugriff auf Elemente mit []
bavaria$city[2]
bavaria[1]           # Auch [] funktioniert bei data frames.
                     # Das Resultat ist ein einelementiger data frame.
class(bavaria[1])
length(bavaria[1])
bavaria[[1]]         # Dasselbe wie bavaria$city 
bavaria[1,2]         # Indizierung wie bei Matrizen: Erste Zeile, zweite Spalte
bavaria[3,1]         # Indizierung wie bei Matrizen: Dritte Zeile, erste Spalte
bavaria[2:3,1:2]     # Zeilen 2 und 3, Spalten 1 und 2
bavaria[2:3,]        # Alle Eintraege der zweiten und dritten Zeile
bavaria[,2:3]        # Alle Eintraege der zweiten und dritten Spalte
bavaria[ bavaria$area<200 , ]  # Alle Zeilen mit bavaria$area < 200
bavaria[ bavaria$area<200, c("city","area") ]

# Jedesmal 'bavaria$' zu tippen ist umstaendlich
city              # Existiert nicht
attach(bavaria)   # Kopiert alle Elemente an einen Ort (sog. Suchpfad), an
                  #   dem R die Variablen findet. 'attach' = anhaengen
city              # 'city', 'area' und 'popul' sind nun bekannt
bavaria[ area<200, c("city","area") ]
mean(area)
var(area)
detach(bavaria)   # Loescht 'city', 'area' und 'popul' aus dem Suchpfad
city              # Ist wieder unbekannt

## Uebung 7: Berechnen Sie die Bevoelkerungsdichten der drei Staedte und
## weisen Sie das Ergebnis einer neuen Spalte im data.frame zu.

#########################
## Abschnitt 8:
## Daten einlesen

# Sind die Daten durch Leerzeichen und/oder Tabulatoren getrennt,
# so verwende den Befehl 'read.table()'.
# Sind die Daten durch Kommata getrennt,
# so verwende den Befehl 'read.csv()' ('csv'= comma separated values).

# Die erste Zeile einer Datendatei enthaelt ueblicherweise keine Daten,
# sondern die Namen der Spalten. Verwende in diesem Fall 
# das Argument 'header=TRUE', ansonsten 'header=FALSE'.

# Die Datei 'swarth1.txt' sei im aktuellen Verzeichnis gespeichert.
# Da aktuelle Verzeichnis erhaelt man mit getwd().
# Um zu einem anderen Verzeichnis zu wechseln, verwende 'setwd("verzeichnispfad")'

finches <- read.table("swarth1.txt",header=TRUE)
finches
str(finches)     # Kurzdarstellung des Datensatzes
head(finches)    # Zeigt nur die ersten 6 Zeilen an
attach(finches)  # 'Insel' und 'Schnabel' sind nun bekannt
mean(Schnabel)   # mittlere Schnabellaenge ueber alle Inseln
sd(Schnabel)     # Standardabweichung der Schnabellaenge ueber alle Inseln
mean(Schnabel[Insel=="Floreana"]) # mittlere Schnabellaenge auf der Insel 'Floreana'
detach(finches)

# Wenn Sie online sind, ist es auch moeglich, direkt ueber das Internet Daten einzulesen:

finches <- read.table("http://evol.bio.lmu.de/_statgen/StatBiol/15SS/swarth1.txt",header=TRUE)


# Sind die Daten in deutscher Dezimalschreibweise, so verwende
# das Argument 'dec=","', das ',' als Dezimalzeichen vereinbart.
# Man kann Tabellen auch aus vielen Programmen heraus im CSV-Format speichern,
# also mit Endung .csv. Um solche Dateien einzulesen, gibt es in R den
# Befehl read.csv(...). Mit diesem Befehl geht R davon aus, dass es eine Kopfzeile ("header")
# gibt und dass die Spalten durch Kommata getrennt sind, den CSV steht fuer "comma
# separated values". Wurden dennoch andere Spaltentrenner verwendet, kann man das mit
# der Option "sep" im Befehlt read.csv spezifizieren.

# Uebung 8a: lesen Sie die Daten ein. Berechnen Sie den Median der
# Schnabellaengen auf Floreana.

## Uebung 8b: Erzeugen Sie eine Datentabelle mit einem
## Tabellenkalkulationsprogramm wie Excel oder LibreOffice Calc, speichern Sie
## die Tabelle, und lesen Sie sie in R als data.frame ein. Benutzen Sie den
## Befehl "str", um die Daten in R zusammengefasst dargestellt zu bekommen.

#########################
## Abschnitt 9:
## Daten grafisch darstellen mit boxplot(), stripchart() oder hist()

# Boxplots werden in der Vorlesung noch ausfuehrlich besprochen.
# Die ~-Formel 'Schnabel~Insel' unterteilt den Schnabel-Vektor
# in Untergruppen entsprechend 'Insel'.
# In Worten: 'Schnabel in Abhaengigkeit von Insel'
attach(finches)       # 'finches' wurde in Abschnitt 10 definiert
boxplot(Schnabel~Insel)
## Fuer aeltere R Versionen (3.x):
plot(Schnabel~Insel)  # derselbe Plot (hier ist wichtig, dass Insel eine
                      # Faktor-Variable ist
plot(Insel,Schnabel)  # derselbe Plot (hier ist wichtig, dass Insel eine
                                        # Faktor-Variable ist
## Ab R Version 4.0 geht es so:
plot(Schnabel~factor(Insel))
plot(factor(Insel),Schnabel)

stripchart(Schnabel~Insel)
         # generiert ein sogenanntes Streudiagramm (oder Scatterplot) 
stripchart(Schnabel~Insel,pch=16,col="steelblue",ylim=c(0.5,3.5),method="stack")
         # ylim=c(0.5,3.5): Die drei Inseln werden bei (1,2,3) eingezeichnet.
         #                  Durch ylim=c(0.5,3.5) wird oben und unten um 0.5 eingerueckt
         # method="stack":  sich ueberlappende Punkte werden uebereinander gestapelt.
stripchart(Schnabel~Insel,pch=16,col="tomato2",ylim=c(0.5,3.5),method="jitter")
         # method="jitter": Punkte werden etwas geruettelt, indem jeder Punkt um einen
         #                  zufaelligen Wert verschoben wird
colors() # UEbersicht ueber alle vordefinierten Farben
hist(Schnabel)           # Histogramm
abline(v=mean(Schnabel)) # Fuegt eine vertikale Linie durch den Mittelwert hinzu
abline(v=mean(Schnabel),lwd=3) # Nun mit Liniendicke=3 (line width)
abline(v=mean(Schnabel)+sd(Schnabel),lwd=3,lty="dashed")
         # Eine gestrichelte (dashed) Linie durch Mittelwert + Standardabweichung
abline(v=mean(Schnabel)-sd(Schnabel),lwd=3,lty="dashed")
         # Eine gestrichelte (dashed) Linie durch Mittelwert - Standardabweichung
detach(finches)

# Uebung 9: Lassen Sie R die Histogramme und Boxplots erneut darstellen, aber diesmal
# in Ihrer Lieblingsfarbe ausgemalt.

#########################
## Abschnitt 10:
## Visualisieren von Verteilungen durch Dichtepolygone

attach(finches)       # 'finches' wurde in Abschnitt 10 definiert
# So sehen die Histogramme aus
hist(Schnabel[Insel=="Santa_Cruz"], main="Beobachtete Schnabellaengen auf Santa Cruz")
hist(Schnabel[Insel=="Floreana"], main="Beobachtete Schnabellaengen auf Floreana")

# Nun berechnen wir die Histogramme ohne sie zu plotten (plot=FALSE)
h.SCru <- hist(Schnabel[Insel=="Santa_Cruz"], plot=FALSE)
h.Fl <- hist(Schnabel[Insel=="Floreana"], plot=FALSE)

# Die Balken des Historgramms sind um h.Fl$mids konzentriert und
# haben die Hoehe h.Fl$dens. Durch diese Punkte zeichnen wir nun
# das Dichtepolygon.

# Erzeuge zunaechst einen "leeren" Plot 
plot(-1,-1, main="Dichtepolygone der Schnabellaengenverteilungen",
            sub="auf den Inseln Floreana und Santa Cruz",
            xlim=c(min(h.SCru$mids, h.Fl$mids),
              max(h.SCru$mids, h.Fl$mids)),
            ylim=c(0,max(h.SCru$dens, h.Fl$dens)),
     xlab="Beobachtete Schnabellaenge",ylab="Dichte")

# fuege nun die Dichtepolygone hinzu:
points(h.SCru$mids, h.SCru$dens, lty=1, lwd=2, type="l")
points(h.Fl$mids, h.Fl$dens, lty=3, lwd=2, type="l")

# schoenere Dichtepolygone:
plot(-1,-1, main="Dichtepolygone der Schnabellaengenverteilungen",
            sub="auf den Inseln Floreana und Santa Cruz",
            xlim=c(min(h.SCru$mids, h.Fl$mids),
              max(h.SCru$mids, h.Fl$mids)),
            ylim=c(0,max(h.SCru$dens, h.Fl$dens)),
     xlab="Beobachtete Schnabellaenge",ylab="Dichte")

h.SCru <- hist(Schnabel[Insel=="Santa_Cruz"], breaks=12:22,plot=FALSE)
h.Fl <- hist(Schnabel[Insel=="Floreana"], breaks=12:22, plot=FALSE)
lines(h.SCru$mids, h.SCru$dens, lty=1, lwd=2)
lines(h.Fl$mids, h.Fl$dens, lty=3, lwd=2)

detach(finches)

## Uebung 10: Fuegen Sie auch die Dichtepolygone der Schnabelgroessen fuer die
## anderen Inseln hinzu.

## 

#########################
## Abschnitt 11:
## Zufallszahlen erzeugen und darstellen

# Die Normalverteilung ist die wichtigste Verteilung.
# Sehen wir uns ihre Verteilung an und ziehen zufaellige Werte.
# In R ist - dnorm() der Befehl fuer die Dichtefunktion
#          - rnorm() der Befehl zum Generieren zufaelliger (random) Werte
plot(dnorm,from=-3,to=3) # Dichtefunktion der standard Normalverteilung
x <- rnorm(10)           # Generiere 10 Werte einer standard Normalverteilung
x
y <- rnorm(1000)         # Generiere 1000 Werte einer standard Normalverteilung
hist(y)                  # Das Histogramm von y
hist(y, breaks=seq(from=-4,to=4,by=0.2), col="orange") # Feinere Einteilung

z <- 1:6
sample(z, size=6)  # Zieht 6 Werte aus z ohne Zuruecklegen;
                   #   es werden hier alle Werte in zufaelliger Reihenfolge gezogen
s <- sample(z, size=10, replace=TRUE)
s                  # Zieht 10 Werte aus z MIT Zuruecklegen;
                   #   dies ist eine Simulation eines zehnmaligen Wuerfelwurfes
mean(s)            # Mittelwert des samples
sd(s)              # Standardabweichung des samples
sample(z, size=10, replace=TRUE)

## Uebung 11a: Ziehen Sie eine Stichprobe von n=10 standardnormalverteilten
## Werten und berechnen Sie Mittelwert und Standardabweichung dieser
## Stichprobe. Wiederholen Sie dieses Experiment mehrfach fuer n=10, n=100 und
## n=100000.

# Uebung 11b: Ziehen Sie 100 Werte aus einer Normalverteilung mit Mittelwert 1.3
# und Varianz 7.2. Berechnen Sie fuer diese Stichprobe den Mittelwert und die
# Standardabweichung und erstellen Sie ein Histogramm. Zeichnen Sie Mittelwert
# +/- Standardabweichung ins Histogramm ein.


#########################
## Abschnitt 12:
## Schleifen

summe <- 0
for( a in c(1,4,6,8)) {
  summe <- summe + a
  cat("a hat jetzt den Wert ",a,"\n")
                   ## '\n' fuegt einen Zeilenumbruch ein
}
summe
sum(c(1,4,6,8))


# Uebung 12a: Addieren Sie die ersten 20 Quadratzahlen jeweils ein durch Verwendung einer
# for-Schleife und einmal ohne Schleife mit dem Befehl sum.

## Uebung 12b: Erzeugen Sie einen Vektor w der Laenge 100. Simulieren Sie dann
## fuer jedes n von 1 bis 100 eine Stichprobe von n standardnormalverteilten
## Zufallswerten und speichern Sie deren Mittelwert in w[n]. Stellen Sie
## anschliessend dar, wie w[n] von n abhaengt.

## Uebung 12c: Erzeugen Sie einen Vektor w der Laenge 100. Simulieren Sie dann
## fuer jedes n von 1 bis 100 eine Stichprobe von n^2 standardnormalverteilten
## Zufallswerten und speichern Sie deren Mittelwert in w[n]. Stellen Sie
## anschliessend dar, wie w[n] von n^2 abhaengt.

#########################
## Abschnitt 13:
## Sitzung beenden
q()

## Jetzt haben Sie sich eine Pause verdient. Schauen Sie sich danach noch mal
## alles bisherige an. Wenn Sie das alles gut verstanden haben, sind Sie
## schon ein bisschen fortgeschritten und koennen mit den folgenden Abschnitten
## weitermachen.

#########################
## Abschnitt 14:
## Faktoren

# Im Finken-Beispiel aus Abschnitt 10 ist 'Insel' eine sogenannte Faktor-Variable.
attach(finches)
class(Insel)
Insel
# Faktor-Variablen dienen dazu, Daten in Untergruppen zu gruppieren

# Fuer Fortgeschrittene:
# Wird eine numerische Variable beim Einlesen faelschlicherweise als Faktor-Variable
# eingestuft, so kann man dies per Hand wie folgt korrigieren:
f <- factor(c(3,5,4,10))
f
as.numeric(f)             # Hierdurch wird die interne Representation der Faktor-levels
                          #   als numerischer Vektor aufgefasst.
as.numeric(as.vector(f))  # Dies liefert das gewuenschte Ergebnis.
as.vector(f)              # Liefert die Bezeichnungen der Levels als Zeichenvektor.

##################################################
##################################################
## Abschnitt 15:
## Anwendungsbeispiel mit Darwin-Finken

## Benutzen Sie setwd("C:where/your/data/are") oder den entsprechenden
## Menue-Eintrag, um R mitzuteilen, wo die Daten zu finden sind.

finches <- read.table("FinchesSulloway.txt",header=TRUE,sep="\t") # read data from file
attach(finches)
boxplot(BeakH~SpeciesID)
boxplot(WingL~SpeciesID)
stripchart(WingL~SpeciesID,method="stack",vertical=TRUE)

#pdf(file="wingbox.pdf",width=10,height=7)
boxplot(WingL~IslandID,col="yellow",horizontal=TRUE,main="Wing Lengths by Island")
#dev.off()

                                        #
#pdf(file="wingstrip.pdf",width=10,height=7)
stripchart(WingL~IslandID,method="stack",col="red",cex=3,lwd=2)
                                        #dev.off()

stripchart(WingL~IslandID,method="stack",col="red",cex=3,lwd=2)
dev.copy2pdf(file="wingstrip2.pdf")

#pdf(file="wingboxstrip.pdf",width=10,height=7)
boxplot(WingL~IslandID,col="yellow",horizontal=TRUE,main="Wing Lengths by Island")
stripchart(WingL~IslandID,method="jitter",col="red",cex=3,pch=1,lwd=2,add=TRUE)
#dev.off()

#pdf(file="winghist.pdf",width=10,height=7)
h1 <- hist(WingL[IslandID==""],freq=FALSE,col="#FF990044",xlim=c(50,95),breaks=-1:10*5+50,
           main="Histogramm (Densities!) with transparen colors",xlab="Wing Lengths")
h2 <- hist(WingL[IslandID=="SCris_Chat"],freq=FALSE,col="#FF009944",breaks=-1:10*5+50,add=TRUE)
h3 <- hist(WingL[IslandID=="Flor_Chrl"],freq=FALSE,col="#00FF0044",breaks=-1:10*5+50,add=TRUE)
h4 <- hist(WingL[IslandID=="Snti_Jams"],freq=FALSE,col="#0000FF44",breaks=-1:10*5+50,add=TRUE)
legend(80,0.18,legend=c("","SCris_Chat","Flor_Chrl","Snti_Jams"),
       col=c("#FF990044","#FF009944","#00FF0044","#0000FF44"),pch=16)
#dev.off()

#pdf(file="wingbar.pdf",width=10,height=7)
M <- rbind(h1$counts,h2$counts,h3$counts,h4$counts)
colnames(M) <- h1$mids
barplot(M,beside=TRUE,col=c("#FF9900","#FF0099","#00FF00","#0000FF"),main="Barplot of Wing Lengths (Numbers)")
legend(40,6,legend=c("","SCris_Chat","Flor_Chrl","Snti_Jams"),col=c("#FF9900","#FF0099","#00FF00","#0000FF"),pch=16)
#dev.off()

#pdf(file="wingdens.pdf",width=10,height=7)
plot(h1$mids,h1$density,col="#FF9900",lwd=3,t="l",xlim=c(50,100),main="Density Polygons",
     xlab="Wing Lengths",ylab="Density")
lines(h2$mids,h2$density,col="#FF0099",lwd=3)
lines(h3$mids,h3$density,col="#00FF00",lwd=3)
lines(h4$mids,h4$density,col="#0000FF",lwd=3)
legend(80,0.18,legend=c("","SCris_Chat","Flor_Chrl","Snti_Jams"),col=c("#FF9900","#FF0099","#00FF00","#0000FF"),pch=16)
#dev.off()

#pdf(file="beakbox.pdf",width=10,height=7)
boxplot(BeakH~SpeciesID,col="yellow",main="Beak Sizes by Species")
#dev.off()

#pdf(file="beakboxstrip.pdf",width=10,height=7)
boxplot(BeakH~SpeciesID,col="yellow",main="Beak Sizes by Species")
stripchart(BeakH~SpeciesID,method="jitter",vertical=TRUE,add=TRUE,pch=1,col="red")
#dev.off()

detach(finches)

#########################
## Abschnitt 16:
## Verschachtelte Schleifen
x <- c()
for ( i in 1:3) {
    x[i] <- 0
    for (j in (1:3)) {
        cat(i," plus ",j," gibt ",i+j,"\n")
        x[i] <- x[i]+i*j
    }
}
x

#########################
## Abschnitt 17:
## Mehr zu Vektoren und Listen

# Indizierung von Vektoren mit Namen
x <- 10:14
names(x) <- c("Charles","Robert","Darwin","Albert","Einstein")
x
x[c(2,1)]
x[c("Robert","Charles")]
x["Robert"]
x[["Robert"]]
x[[c("Robert","Charles")]]  # Does not work
x<13
x[x<13]

x[["Charles"]] + x[["Darwin"]]

## Listen koennen im Gegensatz zu Vektoren verschachtelt sein (Liste von Listen).

V <- c(2,3,4)
V[2]+V[3]
c(V,V)

L <- list(2,3,4)
L
L[2]+L[3]
L[[2]]+L[[3]]
class(L[ 2 ] )
class(L[[ 2 ]] )
L[[2]] + L[[3]]

el <- list("Milch","Schoki","Kaffee")
class(el[[2]])
class(el[2])
el[1:2]
el[[1:2]]


L <- list(c(1,"hello",3) , c(4,5))
L
L[1]
L[2]
L[2][1]
L[[2]][1]
class(L)
class(L[1])
class(L[[1]])
L <- list( Joe=c(1,"hello",3) , Jack=c(4,5))
L[1]
L[[1]]
L["Joe"]
L[["Joe"]]
L$Joe

#########################
## Abschnitt 18:
## Farbenlehre fuer Fortgeschrittene:
y <- rnorm(100,mean=50,sd=10)
hist(y,breaks=0:20*5,col="tomato")
colors()
mycolor <- rgb(red=0.0,green=0.3,blue=0.8,alpha=0.5)
mycolor
hist(x,breaks=0:20*5,col=mycolor,add=TRUE)
x <- rep(0:100,101)/100
y <- rep(0:100,rep(101,101))/100
plot(x,y,col=rgb(red=x,green=y,blue=0),pch=15)
plot(x,y,col=rgb(red=x,green=y,blue=1.0),pch=15)


######################################
## Anhang A
##
## Loesungen zu einigen der Uebungen:

log(4711,base=10)
27^(1/3)
?cos
cos(17/360*(2*pi))
x <- 23/14
x*14
seq(from=7,to=700,by=7)
1:100*7
sqrt(1:1000)
log(2)<sin(43/360*2*pi)
groesse[fangort=="Kathmandu" & groesse>=1.75]
v <- 1:49*2
mean(v)
sd(v)
x <- seq(from=-1,to=5,by=0.01)
plot(x,3*x^2-2*x-4,t="l")
abline(v=0)
abline(h=0)
Z <- rnorm(100,mean=1.3,sd=sqrt(7.2))
mean(Z)
sd(Z)
hist(Z,col="skyblue")
abline(v=mean(Z)+c(0,1,-1)*sd(Z),lwd=2,col="tomato4")
summe <- 0
for(n in 1:20) {
  summe <- summe+n^2
}
summe
sum((1:20)^2)
