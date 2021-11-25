#ANALIZA BAZY DANYCH
#TRZESIENIA ZIEMI

#zmiana nazm kolumn na polskie

colnames(earthquakes) <- c(".", "rok", ".", "miesiac", ".", "dzien", ".",
                           "skala_richtera", ".", "obszar", ".", "panstwo",
                           ".", "zgony")

#ilosc wybranych trzesien ziemi

length(earthquakes$rok)

#ile w danym roku by??o trz??sie?? ziemi

table(earthquakes$rok)

#sortowanie trzesien ziemi rosnaco wg liczby zgonow

sort(earthquakes$zgony)

#odchylenie standardowe roku trzesien ziemi
#o ile ka??dy rok w ktorym wystepowalo trzesienie jest oddalony od sredniej

sd(earthquakes$rok)

mean(earthquakes$rok)

#podstawowe parametry

summary(earthquakes$skala_richtera)

#odchylenie standardowe skali richtera

sd(earthquakes$skala_richtera)

#liczba trzesien ziemi w danym panstwie

table(earthquakes$panstwo)

#minimalna liczba zgonow

min(earthquakes$zgony)

#maksymalna liczba zgonow

max(earthquakes$zgony)

#suma liczby zgonow

sum(earthquakes$zgony)

#??rednia liczba zgonow

mean(earthquakes$zgony)

#skala richtera

kat.skala_richtera <- cut(earthquakes$skala_richtera, breaks = c(5, 6, 7, 8, 9,
                            max(earthquakes$skala_richtera, na.rn = FALSE)))

table(kat.skala_richtera)

barplot(table(kat.skala_richtera), col = "gold")

#NAJSILNIEJSZE TRZESIENIE ZIEMI

#gdzie mialo miejsce

earthquakes$panstwo[which.max(earthquakes$skala_richtera)]

#kiedy mialo miejsce

earthquakes$rok[which.max(earthquakes$skala_richtera)]

#gdzie mialo miejsce trzesienie ziemi, ktore pochlonelo najwiecej ofiar

earthquakes$panstwo[which.max(earthquakes$zgony)]

#NAJSLABSZE TRZESIENIE ZIEMI

#gdzie mialo miejsce

earthquakes$panstwo[which.min(earthquakes$skala_richtera)]

#kiedy mialo miejsce

earthquakes$rok[which.min(earthquakes$skala_richtera)]

#gdzie mialo miejsce trzesienie ziemi, ktore pochlonelo najmniej ofiar

earthquakes$panstwo[which.min(earthquakes$zgony)]

#najmlodsze trzesienie ziemi

max(earthquakes$rok)

#najstarsze trzesienie ziemi

min(earthquakes$rok)

#wykres pudelkowy zgonow w zaleznosci od sily trzesienia ziemi

boxplot(earthquakes$zgony ~ earthquakes$skala_richtera, col = "brown2",
        xlab ="skala richtera", ylab = "zgony", main = "zestawienie zgon??w i
        si??y trz??sie?? ziemi", lwd = 1)

#korelacja liczby zgonow od sily trzesienia ziemi)

cor(earthquakes$zgony, earthquakes$skala_richtera)



#JULIA RYFA
#WIKTORIA PALKA
#I ROK ZINTEGROWANE PLANOWANIE ROZWOJU GRUPA 2
