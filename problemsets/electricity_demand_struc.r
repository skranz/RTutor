#$ problem_set electricity_demand

###############################################################
#$ exercise 5.4
###############################################################

#$ task #######################################################

#' - Betrachten Sie ein einfaches Modell eines Strommarktes
#' - Es gibt folgende lineare Nachfragefunktion
#'    D =beta0+beta1*p+beta2*peak+eps

# Wir haben folgende Parameterwerte
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

# Code hier einfügen...

#' vii) Schätzen Sie eine verkürzte Schätzgleichung in der Sie die nachgefragte Menge D nur auf den Preis p regressieren. Speichern Sie das Ergbnis in der Variable reg2 und überlegen Sie zuvor theoretisch, ob hier ein Endogenitätsproblem besteht und ob der KQ-Schätzer entsprechend konsistent oder inkonsistent ist. Vergleichen Sie Ihre theoretische Überlegung mit dem Regressionsergebniss.

# Code hier einfügen...

#$ solution ###################################################

beta0 = 100
beta1 = -1
beta2 = 1
T = 100 # Zahl der Beobachtungen
sigma.eps = 5

eps = rnorm(T,0,sigma.eps)
peak = rep(0:1, length=T)
p = runif(T,0,1)
D = beta0+beta1*p+beta2*peak + eps
reg = lm(D~p+peak)
reg2 = lm(D~p)


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



###############################################################
#$ exercise 5.5
###############################################################

#$ task #######################################################

#' - Nehmen Sie nun an, dass in den Peak Stunden immer Gaskraftwerke produzieren und in Off-Peak Stunden immer nur Kohlekraftwerke
#' - Die Preise p sollen ähnlich wie im Merrit-Order den variablen Kosten des teuersten produzierenden Kraftwerks entsprechen.
#' - Simulieren und schätzen Sie dieses neue Modell ähnlich wie in Auggabe 5.4

# Wir haben folgende Parameterwerte
beta0 = 100
beta1 = -1
beta2 = 1
T = 10000 # Zahl der Beobachtungen
sigma.eps = 5 # Standardabweichung der Nachfrageschocks


#' i) Simulieren Sie T normalverteilte Nachfrageshocks eps mit Standardabweichung sigma.eps und Mittelwert 0

# Code hier einfügen...

#' ii) Generieren Sie einen Vektor peak der Länge T der abwechselnd jeweils eine 0 und eine 1 enthält

# Code hier einfügen...

#' iii) Simulieren Sie einen Vektor p der Länge T von Preisen, die von Zufallseinfluessen abhängen aber systematisch in Peak Stunden höher sind als in Off-peak Stunden

# Code hier einfügen...

#' iv) Berechnen Sie den Nachfragevektor D

# Code hier einfügen...


#' v) Führen Sie mit dem Befehl lm eine lineare Regression durch mit der Sie die Parameter der Nachfragefunktion schätzen. Speichern Sie das Ergebniss der Regression in der Variable 'reg' ab.

# Code hier einfügen...

#' vi) Zeigen Sie mit Hilfe des Befehls summary(reg) eine detailierte Analyse der Regression.

# Code hier einfügen...

#' vii) Schätzen Sie eine verkürzte Schätzgleichung in der Sie die nachgefragte Menge D nur auf den Preis p regressieren. Speichern Sie das Ergbnis in der Variable reg2 und überlegen Sie zuvor theoretisch, ob hier ein Endogenitätsproblem besteht und ob der KQ-Schätzer entsprechend konsistent oder inkonsistent ist. Vergleichen Sie Ihre theoretische Überlegung mit dem Regressionsergebniss.

# Code hier einfügen...

#' viii) Nutzen Sie den Befehl confint um für reg2 die 99% Konfidenzintervalle für beide geschätzten Koeffizienten zu berechnen. Speichern Sie das Konfidenzintervall in der Variable ci2 Wenn alle Anahmen des linearen Regressionsmodells erfüllt sind, haben diese Konfidenzintervalle die Eigenschaft, dass in 99% der Fälle die wahren Koeffizienten beta0 und beta1 in diesen Konfidenzintervallen liegen. Scheinen die Konfidenzintervalle für reg2 korrekt zu sein oder nicht? Vergleichen Sie mit dem Konfidenzintervallen für reg. 


# Code unten anpassen...
# ci2 = ...    # 99% Konfidenzinterval für reg2

# Zeige auch noch Konfidenzintervall für reg


#$ solution ###################################################

beta0 = 100
beta1 = -1
beta2 = 1
T = 10000 # Zahl der Beobachtungen
sigma.eps = 5

eps = rnorm(T,0,sigma.eps)
peak = rep(0:1, length=T)
p = runif(T,0,1) + 0.5*peak
D = beta0+beta1*p+beta2*peak + eps
reg = lm(D~p+peak)
reg2 = lm(D~p)
ci2 = confint(reg2,level = 0.99)

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
test.H0.rejected(cor.test(p,peak, alternative="greater"),check.warning=FALSE, alpha.failure=0.05,
                 failure.message=paste0(
"Your prices are not significantly positively correlated with the peak dummy (p.value = {{p_value}}). We have cor(p,peak) = " , round(cor(p, peak),4), ". Try harder to generate prices that are more strongly positively correlated with peak."
), success.message = "Great, your prices are significantly higher at peak hours than in off-peak hours.")


# Check D
check.var("D",{beta0+beta1*p+beta2*peak+eps},exists=TRUE, length=TRUE, class=TRUE)

# Check reg
check.regression("reg","lm(D~p+peak)")
give.award("Regression Runner")


# Check reg2
check.regression("reg2","lm(D~p)", failure.message = "Your regression reg2 seems not correct. Read again the exercise!")

# Check ci2
check.var("ci2",exists=TRUE, length=TRUE, class=TRUE)

check.var("ci2",confint(reg2,level=0.99),values=TRUE, failure.message="Your confidence intervals ci2 for reg2 seem incorrect. Have you set the confidence level to 99%? Take a look at the help for confint to learn how to do that.")
give.award("Confidence Shaker")

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




###############################################################
#$ exercise 5.6
###############################################################

#$ task #######################################################

#' - Preise sollen nun gemäß des Merrit-Order Modells bestimmt werden

#' - Die Merrit-Order soll wie folgt beschrieben sein:
#'    c[t]= Q[t]-gamma*w[t]
#' – c[t]: Grenzkosten der Stromproduktion (variable Kosten des teuersten noch produzierenden Kraftwerks) in Periode t = Preis in Periode t
#' - Q[t]: Produzierte Strommenge in Periode t
#' – w[t] <= Q[t] gelieferte Windenergie in Periode t

# Wir haben folgende Parameterwerte
beta0 = 100
beta1 = -1
beta2 = 1
T = 10000 # Zahl der Beobachtungen
gamma = 0.5
sigma.eps = 5 # Standardabweichung der Nachfrageschocks


#' i) Simulieren Sie T normalverteilte Nachfrageshocks eps mit Standardabweichung sigma.eps und Mittelwert 0

# Code hier einfügen...

#' ii) Generieren Sie einen Vektor peak der Länge T der abwechselnd jeweils eine 0 und eine 1 enthält

# Code hier einfügen...

#' iii) Generieren Sie einen zufälligen Vektor wind aus einer Exponentialverteilung der Länge T

# Code hier einfügen...

#' iv) Berechnen Sie den Vektor der Gleichgewichtspreis p für jede Periode

# Code hier einfügen...

#' v) Berechnen Sie den Nachfragevektor D

# Code hier einfügen...

#' vi) Schätzen Sie die Nachfragefunktion und speichern Sie das Ergebniss der Regression in der Variable 'reg' ab. Überlegen Sie zuvor theoretisch, ob hier ein Endogenitätsproblem besteht und ob der KQ-Schätzer entsprechend konsistent oder inkonsistent ist. Vergleichen Sie Ihre theoretische Überlegung mit dem Regressionsergebniss.

# Code hier einfügen...


#$ solution ###################################################

beta0 = 100
beta1 = -1
beta2 = 1
T = 10000 # Zahl der Beobachtungen
sigma.eps = 5

eps = rnorm(T,0,sigma.eps)
peak = rep(0:1, length=T)
p = runif(T,0,1) + 0.5*peak
D = beta0+beta1*p+beta2*peak + eps
reg = lm(D~p+peak)
reg2 = lm(D~p)


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
check.var("p",gamma/(1-beta1*gamma)*(beta0+beta2*peak-w+eps),,exists=TRUE, length=TRUE, class=TRUE,values = TRUE, hint.name = "price.hint")

give.award("Equilibrium Calculator")


# Check D
check.var("D",{beta0+beta1*p+beta2*peak+eps},exists=TRUE, length=TRUE, class=TRUE, hint.name = "price.hint")

# Check reg
check.regression("reg","lm(D~p+peak)")


#$ hints ######################################################
add.hint("rnorm",{
  cat('R has functions to generate random numbers for all sort of distributions. They typically start with the letter "r" followed by a short name for the distribution. To draw normally distributed random numbers, search for "rnorm" in the R help, you find a description of the parameters that "rnorm" takes.')
})


add.hint("price.hint",{
  cat('You need to solve manually for the equilibrium price in each period, by solving the demand function and the inverse supply function (here the merrit order) for the price p and demand D. Then include the resulting formula for the price p in your R code.\n')
})

add.hint("peak.hint",{
  cat('Search in the R help for the command rep. The parameter length.out will be helpful. Consider for example:\n')
  print.example('rep(c("A","B","C"),length.out = 8)')
})

#$ end_exercise
