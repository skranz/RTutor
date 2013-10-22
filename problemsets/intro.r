#### Problemset intro


# To check your solutions in RStudio save (Ctrl-s) and then source (Ctrl-Shift-s)
# If you check "Source on Save" in RStudio you just have to save (Ctrl-s)

ps.dir =  "C:/libraries/RTutor/RTutor/problemsets" # your working directory
ps.file = "intro.r" # this file
library(restorepoint)
library(RTutor)
check.problem.set("intro", ps.dir, ps.file)

###########################################
#### Exercise a)
###########################################

# Grundlagen
# i) Erstelle zwei Variablen x und y und belege x mit dem Wert 5
# und y mit dem Wert aus (1+2)*x
# ii) Erstelle einen Vektor q mit den Zahlen von 1 - 10 mit Schrittweite 0.1
# iii) Erstelle einen Vektor w der aus 91 F?nfen besteht
# iv) Erstelle den Vektor e als Quotient von q und w
#hint.for("a)")

x = 5
y  = 3*x

q = seq(1,10,by = 0.1)
w = rep(5,91)

e = q / w

#### end exercise a)



###########################################
#### Exercise b)
###########################################

# i) Erstelle einen Vektor z mit den f?nf beliebigen Werten
# ii) Multiplizere den Vektor mit x und speichere das Ergebnis 
#     in a
# iii) Erstelle eine Matrix 2x5 m aus den Vektoren z und a
# iv) F?hre die Matrixmultiplikation m.squ = m*m' durch

z = 1:5
a = x*z
m = rbind(z,a)
m.squ = m %*% t(m)

#### end exercise b)


###########################################
#### Exercise c)
###########################################

#i) Erstelle eine 4x4 Matrix n und f?lle sie mit beliebigen Werten
#ii) Erstelle einen Vektor index, dessen erster Eintrag die Summe
# der ersten Zeile der Matrix n ist, der zweite Eintrag der Mittelwert
# von Zeile 2, der dritte soll das Maximum von Zeile 3 enthalten
# und der 4. die Varianz der 4. Zeile.
# hint.for("c)")
#iii) Belege Variable r durch eine elementweise Multiplikation von m und index
# iv) Sei t ein Vektor aus den quadrierten Werten der Diagonalen von r

#### end exercise c)


###########################################
#### Exercise d)
###########################################

# Logische Operationen
# i)    Erstelle Vektor norm <- rnorm (100,0,5) und speichere im Vektor
#       tr alle Werte aus norm, die gr??er Null sind.
# ii)   Erstelle einen Vektor bool, der f?r jeden Wert von tr FALSE ausgibt,
#       wenn dieser kleiner als 10 ist, und f?r jeden Wert > 10 TRUE (logische
#       Ausdr?cke).
# iii)  Multipliziere alle Werte aus tr, die gr??er 10 sind (speichere das
#       das Ergebnis in tr2), mit drei.
# iv)   Initiere die Variable even mit allen Werten aus norm, die kleiner
#       -5 oder gr??er 5 sind.
# v)    Kumuliere (hint : cumsum) alle Werte aus even, die zwischen -7.5 
#       und 7.5 liegen in der Variable even2
# hint.for("d)")

#### end exercise d)


###########################################
#### Exercise e)
###########################################

# Dataframes
# Lade hier die Aktienkurse von Apple seit 1984 herunter.
# http://ichart.finance.yahoo.com/table.csv?s=AAPL&a=08&b=7&c=1984&d=09&e=21&f=2013&g=d&ignore=.csv
# i) Lese die Datei in einen Dataframe df ein (Trennzeichen sind Kommas, Header ist TRUE)
#    Schaue dir anschlie?end den Dataframe genauer an.
# ii) Berechne das arithmetische Mittel der Spalten High und Low und f?ge diese Werte
#     in eine neue Spalte namens meanhilo
# iii) Plotte die neu erstellte Spalte
# iv) Berechne die Varianz der Spalte Adj. Close in der Variable var
# v)  Berechne die t?glichen returns der Aktie in ret( returns = Tageskurs-Vortageskurs)
#     Nutze die Spalte Adj. Close als Kurswert
# vi) Plotte die Returns der Aktie 


#### end exercise e)

