#$ problem_set electricity_demand

###############################################################
#$ exercise a) 
###############################################################

#$ task #######################################################

#' - Betrachten Sie ein einfaches Modell eines Strommarktes
#' - Es gibt folgende lineare Nachfragefunktion
#'    D =beta0+beta1*p+beta2*peak+eps

#' Wir haben folgende Parameterwerte

beta0 = 100
beta1 = -1
beta2 = 1
T = 100 # Zahl der Beobachtungen
sigma.eps = 5

#' i) Simulieren Sie T normalverteilte Nachfrageshocks eps mit Standardabweichung sigma.eps und Mittelwert 0

# Code hier einfügen...

#' ii) Generieren Sie einen Vektor peak der Länge T der abwechselnd jeweils eine 0 und eine 1 enthält

# Code hier einfügen...

#' iii) Simulieren Sie einen Vektor p der Länge T von zufällig verteilten Preisen

# Code hier einfügen...

#' iv) Berechnen Sie den Nachfragevektor D

# Code hier einfügen...


#' v) Führen Sie mit dem Befehl lm eine lineare Regression durch mit der Sie die Parameter der Nachfragefunktion schätzen. Speichern Sie das Ergebniss der Regression in der Variable 'reg' ab.


# Code hier einfügen...


#' vi) Zeigen Sie mit Hilfe des Befehls summary(reg) eine detailierte Analyse der Regression.


#' vii) Schätzen Sie eine verkürzte Schätzgleichung in der Sie die nachgefragte Menge D nur auf den Preis p regressieren. Speichern Sie das Ergbnis in der Variable reg2 und überlegen Sie zuvor theoretisch, ob hier ein Endogenitätsproblem besteht und ob der KQ-Schätzer entsprechend konsistent oder inkonsistent ist. Vergleichen Sie Ihre theoretische Überlegung mit dem Regressionsergebniss.


#$ solution ###################################################

beta0 = 100
beta1 = -1
beta2 = 1
T = 100 # Zahl der Beobachtungen
sigma.eps = 5

#' i) Simulieren Sie T normalverteilte Nachfrageshocks eps mit Standardabweichung sigma.eps und Mittelwert 0
eps = rnorm(T,0,sigma.eps)

#' ii) Generieren Sie einen Vektor peak der Länge T der abwechselnd jeweils eine 0 und eine 1 enthält

peak = rep(0:1, length=T)

#' iii) Simulieren Sie einen Vektor p der Länge T von zufällig verteilten Preisen

p = runif(T,0,1)

#' iv) Berechnen Sie die Nachfragevektor D

D = beta0+beta1*p+beta2*peak + eps

#' v) Führen Sie mit dem Befehl lm eine lineare Regression durch, mit der Sie die Parameter der Nachfragefunktion schätzen. Speichern Sie das Ergebniss der Regression in der Variable 'reg' ab.

reg = lm(D~p+peak)

#' vi) Führen Sie mit dem Befehl lm eine lineare Regression durch, mit der Sie die Parameter der Nachfragefunktion schätzen. Speichern Sie das Ergebniss der Regression in der Variable 'reg' ab.

reg = lm(D~p+peak+eps)


# Code hier einfügen


#$ tests ######################################################

# Check given variables
check.var(c("beta0","beta1","beta2"),
          exists=TRUE, length=TRUE, class=TRUE, values=TRUE)

check.var(c("T"), exists=TRUE, length=TRUE, class=TRUE)

# Check construction of eps
check.var(c("eps"), rnorm(T,0,1), exists=TRUE, length=TRUE, class=TRUE,hint.name="rnorm")
test.normality(eps,hint.name="rnorm")
test.mean(eps,0,hint.name="rnorm")
test.variance(eps,25,hint.name="rnorm")

give.award("Random Shocker")


# Check peak
check.var("peak",exists=TRUE, length=TRUE, class=TRUE, values=TRUE, hint.name="peak.hint")


# Check p
check.var("p",exists=TRUE, length=TRUE, class=TRUE)


# Check D
check.var("D",{beta0+beta1*p+beta2*peak+eps},exists=TRUE, length=TRUE, class=TRUE)

# Check reg
check.regression("reg","lm(D~p+peak)")


give.award("Regression Runner")


# Check reg2
check.regression("reg2","lm(D~p)", failure.message = "Your regression reg2 seems not correct. Read again the exercise!")


#$ hints ######################################################
add.hint("rnorm",{
  cat('R has functions to generate random numbers for all sort of distributions. They typically start with the letter "r" followed by a short name for the distribution. To draw normally distributed random numbers, search for "rnorm" in the R help, you find a description of the parameters that "rnorm" takes.')
})


add.hint("se.p.hint",{
  cat('Run summary(reg). You can find the value of se.p (the standard error of the coefficient for the price p) in that output.\n')
})

add.hint("peak.hint",{
  cat('Search in the R help for the command rep. The parameter length.out will be helpful. Consider for example:\n')
  print.example('rep(c("A","B","C"),length.out = 8)')
})

#$ end_exercise
