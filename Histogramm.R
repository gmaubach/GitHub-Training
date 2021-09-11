################################################################################
## Varianten der Histogramm-Darstellung
## unter ausschliesslicher Verwendung der R-Basis-Grafik
##
## Guenter Faes, https://www.faes.de/ad-oculos/
## Version 1.0, 01.02.2021
## RStudio: Version 1.4.1717
## R-Version: R-4.1.0
##
################################################################################

## Laden der Beispieldaten aus dem Paket mlbench:
library(mlbench)  # Machine Learning Benchmark Problems
data(PimaIndiansDiabetes)

## Information zum Datensatz PimaIndiansDiabetes:
# pregnant:	Number of times pregnant
# glucose:	Plasma glucose concentration (glucose tolerance test)
# pressure:	Diastolic blood pressure (mm Hg)
# triceps:	Triceps skin fold thickness (mm)
# insulin:	2-Hour serum insulin (mu U/ml)
# mass:   	Body mass index (weight in kg/(height in m)\^2)
# pedigree:	Diabetes pedigree function
# age:    	Age (years)
# diabetes:	Class variable (test for diabetes)


## Nur um den Datensatznamen handlicher zu machen:
PID <- PimaIndiansDiabetes

## Ein paar Informationen ueber den Datensatz:
# (Es wird keinerlei Bearbeitung des Datensatzes vorgenommen!)
View(PID)    # Darstellung als Tabelle    
str(PID)     # Darstellung der Struktur
summary(PID) # Beschreibung der Verteilung


# Das Merkmal mass (BMI) wird als Beispiel-Beobachtung verwendet.

########################################################################
# (1) Einfaches Histogramm als Ausgangspunkt:
hist(PID$mass)

########################################################################
# (2) Ein BMI (mass) von 0 kann als unwahrscheinlich angesehen werden, deswegen
# werden nur Werte > 0 beruecksichtigt.
# (Das Merkmal mass ist das 6. Merkmal im DataFrame)
hist(PID$mass[PID$mass > 0], main = paste("Histogramm des Merkmals ", names(PID[6])),
     ylab = "Haeufigkeit",
     xlab = paste(names(PID[6])), 
     xlim = c(10,70), ylim = c(0, 250))

# Farbliche Darstellung:
hist(PID$mass[PID$mass > 0], main = paste("Histogramm des Merkmals ", names(PID[6])),
     ylab = "Haeufigkeit",
     xlab = paste(names(PID[6])), 
     xlim = c(10,70), ylim = c(0, 250),
     col = "lightblue")

########################################################################
# (3) Bereiche in verschiedenen Farben darstellen:
# Dazu wird das Histogramm als Objekt gespeichert.
# Ein BMI von < 20 und > 30 wird als kritisch angesehen und deswegen rot eingefaerbt.
Histogramm <- hist(PID$mass[PID$mass > 0], plot = FALSE)
plot(Histogramm, col = ifelse(Histogramm$breaks < 20 | Histogramm$breaks >= 30, "red", "green"),
     main = paste("Histogramm des Merkmals ", names(PID[6])),
     ylab = "Haeufigkeit",
     xlab = paste(names(PID[6])), 
     xlim = c(10,70), ylim = c(0, 250))

# Hinzunahme des Parameters Mittelwert fuer mass:
# (Der Mittelwert ist erwartungsgemaess hoeher als in der summary-Ausgabe.)
MW_mass <- mean(PID$mass[PID$mass > 0]); MW_mass
abline(v = MW_mass, lty = 2, col = "darkblue")
Ausgabetext <- paste("Mittelwert: ", round(MW_mass, digits = 2))
text(MW_mass + 10, 240, Ausgabetext)

########################################################################
## (4) Darstellung der mass-Verteilung als Histogramm und als Dichtekurve
## in einem eigenen Fenster:

par(mfrow= c(2,1)) # Die 2 Ausgabefenster vorbereiten


# Nochmalige Darstellung des obigen Histogramms im oberen Fenster:
Histogramm <- hist(PID$mass[PID$mass > 0], plot = FALSE)
plot(Histogramm, col = ifelse(Histogramm$breaks < 20 | Histogramm$breaks >= 30, "red", "green"),
     main = paste("Histogramm des Merkmals ", names(PID[6])),
     ylab = "Haeufigkeit",
     xlab = paste(names(PID[6])), 
     xlim = c(10,70), ylim = c(0, 250))

# Hinzunahme des Parameters Mittelwert:
# (Der Mittelwert ist erwartungsgemaess hoeher als in der summary-Ausgabe.)
MW_mass <- mean(PID$mass[PID$mass > 0]); MW_mass
abline(v = MW_mass, lty = 2, col = "darkblue")
Ausgabetext <- paste("Mittelwert: ", round(MW_mass, digits = 2))
text(MW_mass + 10, 240, Ausgabetext)

# Darstellung der Dichteverteilung im unteren Fenster:
plot(density(PID$mass[PID$mass > 0]),
     main = paste("Dichteverteilung des Merkmals ", names(PID[6])),
     col = "darkblue", type ="l", lwd = 2,
     xaxt = "n")

par(mfrow= c(1,1)) # Ausgabestandard wieder herstellen

########################################################################
## (5a) Zwei Histogramme ueberlappend darstellen.
## Da Informationen "verdeckt" werden koennen, ist die Verwendung zu pruefen!
## Siehe Variante 5b.

# Selektion der Daten fuer mass auf Basis des Merkmals diabetes:
Dia_Nein <- PID$mass[PID$mass > 0 & PID$diabetes == "neg"]
Dia_Ja <- PID$mass[PID$mass > 0 & PID$diabetes == "pos"]

# Histogramm fuer mass und Diabetes = negativ:
hist(Dia_Nein, main = paste("Merkmals mass unter Beruecksichtigung von Diabetes"),
     sub = "Ein Beispiel der Histogramm-Moeglichkeiten",
     ylab = "Haeufigkeit",
     xlab = "Merkmal mass", 
     xlim = c(10,70), ylim = c(0, 150),
     col = "green")

# Histogramm fuer mass und Diabetes = positiv:
hist(Dia_Ja, col = "red", add = TRUE)

# Eine Legende hinzufuegen:
leg.text <- c("negativ", "positv")
legend(60, 120, legend = leg.text, title = "Diabetes",title.col = "black", text.col = c("green", "red"))

# Als kleines Extra werden die jeweiligen Mittelwerte eingezeichnet:
MW_Dia_Nein <- mean(Dia_Nein)
MW_Dia_Ja <- mean(Dia_Ja)

abline(v = MW_Dia_Nein, lty = 2, col = "darkgreen", lwd = 3)
Ausgabetext <- paste("Mittelwert Diabetes negativ: ", round(MW_Dia_Nein, digits = 2))
text(MW_Dia_Nein - 10, 150, Ausgabetext)

abline(v = MW_Dia_Ja, lty = 2, col = "darkred", lwd = 3)
Ausgabetext <- paste("Mittelwert Diabetes positv: ", round(MW_Dia_Ja, digits = 2))
text(MW_Dia_Ja + 8, 135, Ausgabetext)

########################################################################
## (5b) Zwei Histogramme zum Vergleich untereinander darstellen.
## (Informationsgehalt ist hoeher als bei 5a!)

par(mfrow= c(2,1)) # Die 2 Ausgabefenster vorbereiten

# Histogramm fuer Diabetes negativ, Ausgabe oberes Fenster:
hist(Dia_Nein, main = "Histogramm des Merkmals mass fuer Diabetes = negativ",
     ylab = "Haeufigkeit",
     xlab = "Merkmal mass", 
     xlim = c(10,70),        # Achtung! x-Achse muss mit der 2. Ausgabe uebereinstimmen!
     ylim = c(0, 170),
     col = "green")

abline(v = MW_Dia_Nein, lty = 2, col = "darkgreen", lwd = 3)
Ausgabetext <- paste("Mittelwert Diabetes negativ: ", round(MW_Dia_Nein, digits = 2))
text(MW_Dia_Nein + 8, 150, Ausgabetext)

# Histogramm fuer Diabetes poistiv, Ausgabe unteres Fenster:
hist(Dia_Ja, main = "Histogramm des Merkmals mass fuer Diabetes = positiv",
     ylab = "Haeufigkeit",
     xlab = "Merkmal mass", 
     xlim = c(10,70),        # Achtung! x-Achse muss mit der 1. Ausgabe uebereinstimmen!
     ylim = c(0, 170),
     col = "red")

abline(v = MW_Dia_Ja, lty = 2, col = "darkred", lwd = 3)
Ausgabetext <- paste("Mittelwert Diabetes positv: ", round(MW_Dia_Ja, digits = 2))
text(MW_Dia_Ja + 8, 135, Ausgabetext)

par(mfrow= c(1,1)) # Ausgabestandard wieder herstellen

########################################################################
## (6) Die Funktion hist() und die Auswirkung von einigen Parametern

# 6a, Darstellung als Dichteverteilung ...
hist(Dia_Nein, freq = FALSE,
     main = "Histogramm des Merkmals mass fuer Diabetes = negativ",
     ylab = "Dichte",
     xlab = "Merkmal mass",
     xlim = c(10,70),
     col = "green",)
# ... und einblenden der Dichteverteilung:
lines(density(Dia_Nein))  # Voreinstellung kernel = gaussian (Gausssche Kernel-Funktion)

# 6b 1, Anpassung der Klassen (Breaks):
hist(Dia_Nein, freq = FALSE,
     main = "Histogramm des Merkmals mass fuer Diabetes = negativ",
     ylab = "Dichte",
     xlab = "Merkmal mass",
     xlim = c(10,70),
     col = "green",
     breaks = c(10, 20, 30, 40, 50, 60, 70))

# 6b, 2:
hist(Dia_Nein, freq = FALSE,
     main = "Histogramm des Merkmals mass fuer Diabetes = negativ",
     ylab = "Dichte",
     xlab = "Merkmal mass",
     xlim = c(10,70),
     col = "green",
     breaks = 20)

# 6b, 3:
hist(Dia_Nein, freq = FALSE,
     main = "Histogramm des Merkmals mass fuer Diabetes = negativ",
     ylab = "Dichte",
     xlab = "Merkmal mass",
     xlim = c(10,70),
     col = "green",
     breaks = (max(Dia_Nein)-min(Dia_Nein) / 10))


