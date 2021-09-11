################################################################################
## Boxplot-Darstellung von mehreren Beobachtungen
## unter ausschliesslicher Verwendung der R-Basis-Grafik
##
## Guenter Faes, https://www.faes.de/ad-oculos/
## Version 1.0, 04.10.2020
##
## RStudio: Version 1.4.1717
## R-Version: R-4.1.0
##
## Inspiration: Rolling Your RS (Blog), Base Graphics "The Drunk Uncle" of R ?
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


## Darstellung eines Merkmals als Boxplot:
boxplot(PID$glucose, main = "Verteilung des Merkmals 'glucose' als Boxplot", 
        ylab = "Glukosekonzentration",
        xlab = "Glukose")

## Alle Merkmale als Uebersicht darstellen:
boxplot(PID, main = "Alle Merkmale", ylab = "Merkmalsausprägung")

## Einzelne grafische Darstellung der Merkmale:
for (ii in 1:(ncol(PID)-1)) {
  form <- as.formula(paste(names(PID)[ii]," ~ diabetes",sep=""))
  boxplot(form,data=PID,main=names(PID)[ii])
  grid()
}

## Grafische Darstellung der Merkmale in einem Bild:

par(mfrow=c(2,4)) # 2 * 4 Grafiken in einer Darstellung

for (ii in 1:(ncol(PID)-1)) {
  form <- as.formula(paste(names(PID)[ii]," ~ diabetes",sep=""))
  boxplot(form,data=PID,main=names(PID)[ii])
  grid()
}

par(mfrow=c(1,1)) # Grafikparameter wieder auf eine Darstellung pro Window einstellen.
