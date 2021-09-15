##***************************************************
## Ad-Oculos-Projekt-Video R Formelnotation
## In diesem Skript werden verschiedene
## Formelnotationen anhand eines Regressionsmodells 
## beschrieben. Inhaltliche Aspekte wie Kausalität 
## und Interpretation spielen dabei keine Rolle!
## 
## 
## Datum: 07.12.2020
## Version: 1.0
## 
## RStudio: Version 1.4.1717
## R-Version: R-4.1.0
## 
## Projekt-Seite: https://www.faes.de/ad-oculos/
## Günter Faes, spv@faes.de
##***************************************************

## Beispieldaten laden:

# Kontinuierliche Daten, Rotwein-Datensatz:
# Quellen-Angabe:
# P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
# Modeling wine preferences by data mining from physicochemical properties.
# In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236.

# Input variables (based on physicochemical tests):
#   1 - fixed acidity
# 2 - volatile acidity
# 3 - citric acid
# 4 - residual sugar
# 5 - chlorides
# 6 - free sulfur dioxide
# 7 - total sulfur dioxide
# 8 - density
# 9 - pH
# 10 - sulphates
# 11 - alcohol
# Output variable (based on sensory data): 
# 12 - quality (score between 0 and 10)

Rotwein <- read.csv2("Rotwein.csv")
View(Rotwein)

# Kontinuierliche / kategoriale Daten, Fahrzeug-Datensatz:
# R-Datensatz mtcars (Motor Trend Car Road Tests)
# Quellenangabe:
# The data was extracted from the 1974 Motor Trend US magazine,
# and comprises fuel consumption and 10 aspects of automobile
# design and performance for 32 automobiles (1973..74 models).

# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	Engine (0 = V-shaped, 1 = straight)
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors

Auto <- mtcars
View(Auto)

##**********************************************************************
## 1. Beispiel, einfaches lineares Modell:
# Modell: Y = a + bx
# Daten: Kontinuierlich, Rotwein
# X: Alcohol
# Y: Quality

Y <- lm(Quality ~ Alcohol, data = Rotwein)
summary(Y)

##**********************************************************************
## 2. Beispiel, multiples lineares Modell:
# Modell: Y = a + b1x1 + b2x2
# Daten: Kontinuierlich, Rotwein
# X1: Alcohol
# X2: VolatileAcidity
# Y: Quality

Y <- lm(Quality ~ Alcohol + VolatileAcidity, data = Rotwein)
summary(Y)

##**********************************************************************
## 3a. Beispiel, multiples lineares Modell mit Wechselwirkung:
# In diesem Modell wird der Einfluss von X2 und X3 und der
# Term X2*X3 berücksichtigt.
# Modell: Y = a + b1x1 + b2x2 * b3X3
# Daten: Kontinuierlich, Rotwein
# X1: Alcohol
# X2: VolatileAcidity
# X3: FixedAcidity
# Y: Quality

Y <- lm(Quality ~ Alcohol + VolatileAcidity * FixedAcidity, data = Rotwein)
summary(Y)

# Daten- und Interaktionsübersicht auf Basis des Modells:
Y_Daten <- model.frame(Y)
View(Y_Daten)
Y_Interaktion <- model.matrix(Y)
View(Y_Interaktion)

##**********************************************************************
## 3b. Beispiel, multiples lineares Modell mit Wechselwirkung:
# Wie Modell 3a, nur unter Einbeziehung kategorialer Variablen.
# Modell: Y = a + b1x1 + b2x2 * b3X3
# Daten: Kontinuierlich / Kategorial, Auto
# X1: hp, Gross horsepower
# X2: wt, Weight (1000 lbs)
# X3: cyl, Number of cylinders, kategorial
# Y: mpg, Miles/(US) gallon

# Für das Modell ist es erforderlich, das Merkmal cyl vom Typ num
# nach Typ factor zu wandeln:
Auto$cyl <- as.factor(Auto$cyl)

Y <- lm(mpg ~ hp +  wt * cyl, data = Auto)
summary(Y)

# Daten- und Interaktionsübersicht auf Basis des Modells:
Y_Daten <- model.frame(Y)
View(Y_Daten)
Y_Interaktion <- model.matrix(Y)
View(Y_Interaktion)


##**********************************************************************
## 4. Beispiel, multiples lineares Modell mit Interaktion:
# In diesem Modell wird nur das Produkt aus X2 und X3 berücksichtigt.
# Modell: Y = a + b1x1 + (b2x2 : b3X3) 
# Daten: Kontinuierlich, Rotwein
# X1: Alcohol
# X2: TotalSulfurDioxide
# X3: FreeSulfurDioxide
# Y: Quality

Y <- lm(Quality ~ Alcohol + TotalSulfurDioxide : FreeSulfurDioxide, data = Rotwein)
summary(Y)

# Daten- und Interaktionsübersicht auf Basis des Modells:
Y_Daten <- model.frame(Y)
View(Y_Daten)
Y_Interaktion <- model.matrix(Y)
View(Y_Interaktion)


##**********************************************************************
## 5a. Beispiel, allgemeines zur Parameterschätzung einer Funktion:
# Den Schnittpunkt (Intercept) auf 0 festlegen.

# Modell: Y = bx
# Daten: Kontinuierlich, Rotwein
# X: Alcohol
# Y: Quality

Y <- lm(Quality ~ Alcohol - 1, data = Rotwein)
summary(Y)


##**********************************************************************
## 5b. Beispiel, allgemeines zur Parameterschätzung einer Funktion:
# Ausfuühren einer Rechenoperation innerhalb der Formel.


# Modell: Y = a + b1x1 + b2x2^2
# Daten: Kontinuierlich, Rotwein
# X1: Alcohol
# X2: CitricAcid (Quadriert)
# Y: Quality

Y <- lm(Quality ~ Alcohol + I(CitricAcid^2), data = Rotwein)
summary(Y)

# Datenübersicht auf Basis des Modells:
Y_Daten <- model.frame(Y)
View(Y_Daten)


##**********************************************************************
## 5c. Beispiel, allgemeines zur Parameterschätzung einer Funktion:
# Alle unabhängige Merkmale in einer Formel berücksichtigen:

# Aus Beispielgründen wird ein verkleinerter Rotwein-Datensatz gebildet:
Sub_Rotwein <- data.frame(Rotwein[,1:4], Rotwein[,11:12])
View(Sub_Rotwein)

# Modell: Y = a + b1x1 + b2x2 + ... + bnxn
# Daten: Kontinuierlich, Sub_Rotwein
# X: Alle Variablen ausser Quality
# Y: Quality

Y <- lm(Quality ~ . , data = Sub_Rotwein)
summary(Y)

# Datenübersicht auf Basis des Modells:
Y_Daten <- model.frame(Y)
View(Y_Daten)


##**********************************************************************
## 5d. Beispiel, allgemeines zur Parameterschätzung einer Funktion:
# Ausschluss einiger Merkmale aus der Formel:

# Modell: Y = a + b1x1 + b2x2 - ... - bnxn
# Daten: Kontinuierlich, Sub_Rotwein
# X1: Alcohol
# X2: ResidualSugar
# Y: Quality

Y <- lm(Quality ~ . - FixedAcidity - VolatileAcidity - CitricAcid, data = Sub_Rotwein)
summary(Y)

# Daten- und Interaktionsübersicht auf Basis des Modells:
Y_Daten <- model.frame(Y)
View(Y_Daten)
Y_Interaktion <- model.matrix(Y)
View(Y_Interaktion)


