################################################################################
## Das R-Grafiksystem ggplot2
## Video Barplot mit ggplot2
##
## Günter Faes, https://www.faes.de/ad-oculos/
## Version 1.0, 18.05.2021
##
## Ein Einstieg in die grafische Darstellung mit ggplot2 stellen die Videos
## Histogramm mit ggplot2, YouTube-Link: https://youtu.be/grHN1H7pGA4
## und
## Boxplot mit ggplot2, YouTube-Link: https://youtu.be/2GueHuCGH4A
## dar. 
##
################################################################################

## Laden der benötigten Pakete:
library(ggplot2)
library(mlbench)  # Machine Learning Benchmark Problems, Beispieldaten

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


## Ein paar Informationen über den Datensatz:
View(PID)    # Darstellung als Tabelle    
str(PID)     # Darstellung der Struktur
summary(PID) # Beschreibung der Verteilung

## --------------------- Barplotbeispiele -----------------------

## ----------------------1. Beispiel: ---------------------------
# Übersicht über die Schwangerschaften (pregnant)
ggplot(PID, aes(x = as.factor(pregnant))) +
  # Barplot erzeugen:
  geom_bar(fill = "lightblue") + 
  # Beschriftung hinzufügen:
  labs(title = "Barplot des Merkmals preganat (Schwangerschaften)",
       subtitle = "Datensatz PimaIndiansDiabetes (mlbench), 1. Beispiel",
       caption = "Video des Ad-Oculos-Projekts: Barplot mit ggplot2",
       x = "Anzahl Schwangerschaften",
       y = "Häufigkeiten")

## ----------------------1b. Beispiel: --------------------------
# Übersicht über die Schwangerschaften (pregnant)
# Barplot als horizontale Dartsellung:
ggplot(PID, aes(x = as.factor(pregnant))) +
  # Barplot erzeugen:
  geom_bar(fill = "lightblue") + 
  # Beschriftung hinzufügen:
  labs(title = "Barplot des Merkmals preganat (Schwangerschaften)",
       subtitle = "Datensatz PimaIndiansDiabetes (mlbench), 1b. Beispiel",
       caption = "Video des Ad-Oculos-Projekts: Barplot mit ggplot2",
       x = "Anzahl Schwangerschaften",
       y = "Häufigkeiten") +
  # Horizontale Darstellung:
  coord_flip()



## ----------------------2. Beispiel: ---------------------------
# Übersicht über die Schwangerschaften (pregnant)
# Hinzufügen zur Grafik die Anzahl Beobachtungen.Dazu wird ein neur
# Dataframe gebildet.

Schwanger <- data.frame(table(as.factor(PID$pregnant)))
names(Schwanger) <- c("Schwangerschaften", "Häufigkeit")
View(Schwanger)

ggplot(Schwanger, aes(x = Schwangerschaften, y = Häufigkeit)) +
  # Barplot über geom_col erzeugen:
  geom_col(fill = "lightblue") + 
  # Beschriftung hinzufügen:
  labs(title = "Barplot des Merkmals preganat (Schwangerschaften)",
       subtitle = "Datensatz PimaIndiansDiabetes (mlbench), 2. Beispiel",
       caption = "Video des Ad-Oculos-Projekts: Barplot mit ggplot2",
       x = "Anzahl Schwangerschaften",
       y = "Häufigkeiten") + 
  # Anzahl Beobachtungen hinzufügen:
  geom_text(aes(label = Häufigkeit), nudge_y = 3)
  

## ----------------------3. Beispiel: ---------------------------
# Übersicht Diabetes in Abhängigkeit zum Alter

# Erstellen eines Dataframes Alter/Diabetes-Befund/Häufigkeit:
Diabetes <- data.frame(table(PID$age, PID$diabetes))
names(Diabetes) <- c("Alter", "Befund", "Häufigkeit")
View(Diabetes)


ggplot(Diabetes) +
  # Barplot über geom_col erzeugen:
  #Durch position_dodge() wird die Azahl neg/pos nebeneinander ausgegeben.
  geom_col(aes(x = Alter, y = Häufigkeit, fill = Befund), position = position_dodge()) +
  # Beschriftung hinzufügen:
  labs(title = "Übersicht Alter und Diabetes-Befund",
       subtitle = "Datensatz PimaIndiansDiabetes (mlbench), 3. Beispiel",
       caption = "Video des Ad-Oculos-Projekts: Barplot mit ggplot2",
       x = "Alter",
       y = "Häufigkeiten")  


## ----------------------3b. Beispiel: ---------------------------
# Übersicht Diabetes in Abhängigkeit zum Alter
# Einstellen der Balkenfarbe in negativ = grün und positiv = rot

ggplot(Diabetes) +
  # Barplot über geom_col erzeugen:
  #Durch position_dodge() wird die Azahl neg/pos nebeneinander ausgegeben.
  geom_col(aes(x = Alter, y = Häufigkeit, fill = Befund),
           position = position_dodge())+
  # Setzen der Balkenfarbe:
  #(Eine Quelle für den Hex-Code ist: https://encycolorpedia.de/html)
  scale_fill_manual(values = c("lightgreen", "#ff7b5a")) +
  
  # Beschriftung hinzufügen:
  labs(title = "Übersicht Alter und Diabetes-Befund",
       subtitle = "Datensatz PimaIndiansDiabetes (mlbench), 3b. Beispiel",
       caption = "Video des Ad-Oculos-Projekts: Barplot mit ggplot2",
       x = "Alter",
       y = "Häufigkeiten")  


## --------------------- 4. Beispiel: ---------------------------
# Übersicht Diabetes in Abhängigkeit zum mittleren BMI (mass)

# Erstellen eines Dataframes mass (BMI) und diabetes:
# (mass = 0 wird als unwahrscheinliche Beobachtung ausgeschlossen!)
BMI <- data.frame(mass = PID$mass[PID$mass > 0], diabetes = PID$diabetes[PID$mass > 0])
View(BMI)

# Mittelwert & Standardabweichung für mass wenn diabetes = negativ:
BMI_MW_neg <- mean(BMI$mass[BMI$diabetes == "neg"]); BMI_MW_neg
BMI_Sd_neg <- sd(BMI$mass[BMI$diabetes == "neg"]); BMI_Sd_neg

# Mittelwert & Standardabweichung für mass wenn diabetes = positiv:
BMI_MW_pos <- mean(BMI$mass[BMI$diabetes == "pos"]); BMI_MW_pos
BMI_Sd_pos <- sd(BMI$mass[BMI$diabetes == "pos"]); BMI_Sd_pos

# Dataframe zur Darstellung als Barplot basteln:
BMI_neg <- c(BMI_MW_neg, BMI_Sd_neg)
BMI_pos <- c(BMI_MW_pos, BMI_Sd_pos)
BMI_Stat <- rbind(BMI_neg, BMI_pos)
BMI_Stat <- data.frame(BMI_Stat)
names(BMI_Stat) <- c("MW", "StdAbw")
BMI_Stat$Befund <- c("neg", "pos")
BMI_Stat    # Ausgabe des Dataframes BMI_Stat

ggplot(BMI_Stat) +
  # Barplot:
  geom_bar(aes(x = Befund, y = MW, fill = Befund), stat = "identity", alpha = 0.7) +
  # Errorbar für die Standardabweichung hinzufügen:
  geom_errorbar(aes(x = Befund, ymin = MW-StdAbw, ymax = MW+StdAbw),
                color = "blue",
                size = 1,
                width = 0.4) +
  # Setzen der Balkenfarbe:
  scale_fill_manual(values = c("lightgreen", "#ff7b5a")) +
    # Beschriftung hinzufügen:
  labs(title = "Mittlere BMI (mass) und Diabetes-Befund\ninklusive Standardabweichung",
       subtitle = "Datensatz PimaIndiansDiabetes (mlbench), 4. Beispiel",
       caption = "Video des Ad-Oculos-Projekts: Barplot mit ggplot2",
       x = "Alter",
       y = "Häufigkeiten")  

## ---- Skript-Ende -------









