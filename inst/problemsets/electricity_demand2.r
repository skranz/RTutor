
##############################################################
#' # Problemset electricity_demand2
##############################################################

#+ include=FALSE

# Remove comments below if you need to install packages
# install.packages('devtools');install.packages('whisker');install.packages('stringr')
# install.packages('RJSONIO');
# library(devtools)
# install_github(repo = 'restorepoint', username = 'skranz')
# install_github(repo = 'RTutor', username = 'skranz')

# To check your solutions in RStudio save (Ctrl-s) and then source (Ctrl-Shift-s)
# If you check 'Source on Save' in RStudio you just have to save (Ctrl-s)

ps.dir =  'D:/libraries/RTutor/problemsets/electricity_demand' # the folder in which this file is stored
ps.file = 'electricity_demand2.r' # this file
user.name = 'Prof' # your user name


library(RTutor)
check.problem.set('electricity_demand2', ps.dir, ps.file, user.name=user.name, reset=!FALSE)

#+ include=TRUE

cat('Name: ', user.name)

############################################
#' ## Exercise 5.7 a)
############################################


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

#' v) Installieren Sie das R Package "AER" und laden Sie es mit dem Befehl library.

library(AER)

#' vi) Nutzen Sie die Funktion ivreg um direkt die Instrumentalvariablen Schätzung der Nachfragefunktion durchzuführen. Speichern Sie das Ergebnis in der Variablen iv 

iv = ivreg(D~p+peak|peak+wind)

#' vii) Installieren und laden Sie das R Package texreg

#' viii) Nutzen Sie die Funktion texreg um die Ergebnisse von ols, stage2 und iv nebeneinander anzuzeigen. Das Ausgabeformat ist ähnlich dem Format, in dem Regressionsergebnisse in wissenschaftlichen Publikationen angezeigt werden.



#### end exercise 5.7 a)

