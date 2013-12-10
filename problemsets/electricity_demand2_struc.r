#$ problem_set electricity_demand2


###############################################################
#$ exercise 5.7 a)
###############################################################

#$ task #######################################################

#' Betrachten Sie das Merrit-Order Modell von Aufgabe 5.6

# Parameter
beta0 = 100
beta1 = -1
beta2 = 1
T = 10000 # Zahl der Beobachtungen
sigma.eps = 5
gamma = 0.5

# Simuliere Daten
eps = rnorm(T,0,sigma.eps)
peak = rep(0:1, length=T)
wind =rexp(T)
p = gamma/(1-beta1*gamma)*(beta0+beta2*peak-wind+eps)
D = beta0+beta1*p+beta2*peak + eps

#' i) Wiederholen Sie die OLS Regression und speichern Sie sie in der Variable ols ab

# Code einfuegen...

#' ii) Führen Sie die Stufe-1-Regression, der "Two-Stages-Least-Squares Regression" durch, mit wind und peak als Instrumente und speichern Sie das Ergebnis in der Variablen stage1

# Code einfuegen...

#' iii) Entnehmen Sie aus stage1 mit dem Befehl "fitted" die vorhergesagten Preise der ersten Stufe und speichern Sie die vorhergesagten Preise in der Variablen p.hat

# Code einfuegen...

#' iv) Führen Sie die Stufe-2-Regression, der "Two-Stages-Least-Squares Regression" durch und speichern Sie das Ergebnis in der Variablen stage2. Wirkt der resultierende Schätzer konsistent?

# Code einfuegen...

#' v) Installieren Sie das R Package "AER" und laden Sie es mit dem Befehl library.

# Code einfuegen...


#' vi) Nutzen Sie die Funktion ivreg um direkt die Instrumentalvariablen Schätzung der Nachfragefunktion durchzuführen. Speichern Sie das Ergebnis in der Variablen iv 

# Code einfuegen...

#' vii) Installieren und laden Sie das R Package texreg

#' viii) Nutzen Sie die Funktion texreg um die Ergebnisse von ols, stage2 und iv nebeneinander anzuzeigen. Das Ausgabeformat ist ähnlich dem Format, in dem Regressionsergebnisse in wissenschaftlichen Publikationen angezeigt werden.


#$ solution ###################################################

#' Betrachten Sie das Merrit-Order Modell von Aufgabe 5.6

# Parameter
beta0 = 100
beta1 = -1
beta2 = 1
T = 10000 # Zahl der Beobachtungen
sigma.eps = 5
gamma = 0.5

# Simuliere Daten
eps = rnorm(T,0,sigma.eps)
peak = rep(0:1, length=T)
wind =rexp(T)
p = gamma/(1-beta1*gamma)*(beta0+beta2*peak-wind+eps)
D = beta0+beta1*p+beta2*peak + eps

#' i) Wiederholen Sie die OLS Regression und speichern Sie sie in der Variable ols ab

ols = lm(D~p+peak)

#' ii) Führen Sie die Stufe-1-Regression, der "Two-Stages-Least-Squares Regression" durch, mit wind und peak als Instrumente und speichern Sie das Ergebnis in der Variablen stage1

stage1 = lm(p~peak+wind)

#' iii) Entnehmen Sie aus stage1 mit dem Befehl "fitted" die vorhergesagten Preise der ersten Stufe und speichern Sie die vorhergesagten Preise in der Variablen p.hat

p.hat = fitted(stage1)

#' iv) Führen Sie die Stufe-2-Regression, der "Two-Stages-Least-Squares Regression" durch und speichern Sie das Ergebnis in der Variablen stage2. Wirkt der resultierende Schätzer konsistent?

stage2 = lm(D~p.hat+peak)
summary(stage2)

#' v) Installieren Sie das R Package "AER" und laden Sie es mit dem Befehl library.

# install.packages("AER")
suppressWarnings(require(AER, quietly=TRUE, warn.conflicts=FALSE))

#' vi) Nutzen Sie die Funktion ivreg um direkt die Instrumentalvariablen Schätzung der Nachfragefunktion durchzuführen. Speichern Sie das Ergebnis in der Variablen iv 

iv = ivreg(D~p+peak|peak+wind)

#' vii) Installieren und laden Sie das R Package texreg

# install.packages("texreg")
suppressWarnings(require(texreg, quietly=TRUE, warn.conflicts=FALSE))


#' viii) Nutzen Sie die Funktion screenreg um die Ergebnisse von ols, stage2 und iv nebeneinander anzuzeigen. Das Ausgabeformat ist ähnlich dem Format, in dem Regressionsergebnisse in wissenschaftlichen Publikationen angezeigt werden.

#screenreg(list(ols, stage2, iv), custom.model.names=c("ols","stage2","iv"))


#$ tests ######################################################

# Check given variables
check.var(c("beta0","beta1","beta2"),
          exists=TRUE, length=TRUE, class=TRUE, values=TRUE)

check.var(c("T"), exists=TRUE, length=TRUE, class=TRUE)


#' i) Wiederholen Sie die OLS Regression und speichern Sie sie in der Variable ols ab

check.regression("ols","lm(D~p+peak)")

#' ii) Führen Sie die Stufe-1-Regression, der "Two-Stages-Least-Squares Regression" durch, mit wind und peak als Instrumente und speichern Sie das Ergebnis in der Variablen stage1

check.regression("stage1","lm(p~peak+wind)", hint.name="stage1")

#' iii) Entnehmen Sie aus stage1 mit dem Befehl "fitted" die vorhergesagten Preise der ersten Stufe und speichern Sie die vorhergesagten Preise in der Variablen p.hat

check.var("p.hat",fitted(stage1), check.all = TRUE)

#' iv) Führen Sie die Stufe-2-Regression, der "Two-Stages-Least-Squares Regression" durch und speichern Sie das Ergebnis in der Variablen stage2. Wirkt der resultierende Schätzer konsistent?

check.regression("stage2","lm(D~p.hat+peak)", hint.name = "stage2")
give.award("Two stages for Consistency")

#' v) Installieren Sie das R Package "AER" und laden Sie es mit dem Befehl library.

check.package("AER")

#' vi) Nutzen Sie die Funktion ivreg um direkt die Instrumentalvariablen Schätzung der Nachfragefunktion durchzuführen. Speichern Sie das Ergebnis in der Variablen iv 

check.regression("iv","ivreg(D~p+peak|peak+wind)", hint.name="ivreg")
give.award("Consistent IV")
give.award("Playing the Wind")


#' vii) Installieren und laden Sie das R Package texreg

check.package("texreg")
# install.packages("texreg")
suppressWarnings(require(texreg, quietly=TRUE, warn.conflicts=FALSE))


#' viii) Nutzen Sie die Funktion screenreg um die Ergebnisse von ols, stage2 und iv nebeneinander anzuzeigen. Das Ausgabeformat ist ähnlich dem Format, in dem Regressionsergebnisse in wissenschaftlichen Publikationen angezeigt werden.


#$ hints ######################################################

add.hint("stage1",{
  cat('In the first stage of the 2-Stage-Least Squares procedure, you regress your endogenous variable (here p) on all instrumental variables. All instrumental variables, means the exogenous explanatory variables of the demand function (here peak) as well as the instruments that are not part of the demand function (here wind).\n')
})


add.hint("stage2",{
  cat('In the second stage of the 2-Stage-Least Squares procedure, you estimate your original demand function with the difference that the endogenous price p is replaced by the fitted price p.hat from the first stage..\n')
})


add.hint("ivreg",{
  cat('Take a look at the help for ivreg an at its examples to see how your regression formula with instruments has to look like. In particular, note that the formula has three parts, if we have z1, z2 and z3 as instruments, a formula could look like "y ~ x1 + x2 | z1 + z2 + z3". Recall that you must also include the exogenous explanatory variables as instruments.\n')
})


#$ end_exercise
