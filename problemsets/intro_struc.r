#$ problem_set intro


###############################################################
#$ exercise a)
###############################################################

#$ task #######################################################
# Grundlagen
# i) Erstelle zwei Variablen x und y und belege x mit dem Wert 5
# und y mit dem Wert aus (1+2)*x
# ii) Erstelle einen Vektor q mit den Zahlen von 1 - 10 mit Schrittweite 0.1
# iii) Erstelle einen Vektor w der aus 91 F?nfen besteht
# iv) Erstelle den Vektor e als Quotient von q und w
#hint.for("a)")

#$ solution ##########################################################
x = 5
y = x*(1+2)

q= seq(1,10,by=0.1)
w = rep (5,91)
e = q/w

#$ tests ######################################################
check.var("x", exists=TRUE,length=TRUE, values=TRUE, class = TRUE, hint.name="1")
check.var("y", exists=TRUE,length=TRUE, values=TRUE, class = TRUE, hint.name="1")
check.var("q", exists=TRUE,length=TRUE, values=TRUE, class = TRUE, hint.name="seq")
check.var("w", exists=TRUE,length=TRUE, values=TRUE, class = TRUE)
check.var("e", exists=TRUE,length=TRUE, values=TRUE, class = TRUE)

#$ hints ######################################################
add.hint("1",{cat('
# The following code stores 2+3 in a variable y
y = 2+3
or 
y <- 2+3"
')})
add.hint("seq",{cat('
Search in Google for "R generate a sequence of numbers" and check the shown mailing list answers or R help files for a command that can generate such sequences.
')})
#$ end_exercise
###############################################################
#$ exercise b)
###############################################################
#$ task #######################################################
# i) Erstelle einen Vektor z mit den f?nf beliebigen Werten
# ii) Multiplizere den Vektor mit x und speichere das Ergebnis 
#     in a
# iii) Erstelle eine Matrix 2x5 m aus den Vektoren z und a
# iv) F?hre die Matrixmultiplikation m.squ = m*m' durch
# 
#$ solution ###################################################
z = c(10,5,-8,2,1)
a = x*z
m = matrix(rep(0,10), nrow =2)
m[1,]= z
m[2,]= a
m.squ = m%*%t(m)
#$ tests ######################################################
check.var("z", exists=TRUE,length=TRUE, class = TRUE)
check.var("a", {x*z}, exists=TRUE,length=TRUE, class = TRUE, values = TRUE)
check.var("m", exists=TRUE,length=TRUE, class = TRUE)
check.var("m.squ",m%*%t(m), exists=TRUE,length=TRUE, class = !TRUE, values=TRUE)



#$ hints ######################################################

#$ end_exercise ###############################################
###############################################################
#$ exercise c)
###############################################################

#$ task #######################################################
#i) Erstelle eine 4x4 Matrix n und f?lle sie mit beliebigen Werten
#ii) Erstelle einen Vektor index, dessen erster Eintrag die Summe
# der ersten Zeile der Matrix n ist, der zweite Eintrag der Mittelwert
# von Zeile 2, der dritte soll das Maximum von Zeile 3 enthalten
# und der 4. die Varianz der 4. Zeile.
# hint.for("{{ex_name}}")
#iii) Belege Variable r durch eine elementweise Multiplikation von m und index
# iv) Sei t ein Vektor aus den quadrierten Werten der Diagonalen von r
#$ solution ###################################################

n = rbind(c(1,5,6,10),c(2,3,4,1),c(9,2,5,1),c(0,7,3,4))
index = c(sum(n[1,]),mean(n[2,]), max(n[3,]), var(n[4,]))
r = n*index
t = c(r[1,1]^2,r[2,2]^2,r[3,3]^2,r[4,4]^2)

#$ tests ######################################################

check.var("n", exists=TRUE,length=TRUE, class = TRUE)
check.var("r", exists=TRUE,length=TRUE, class = TRUE)
check.var("index", exists=TRUE,length=TRUE, class = TRUE, values=TRUE,
          expr = c(sum(n[1,]),mean(n[2,]), max(n[3,]), var(n[4,])),
          hint.name = "hint.index")

check.var("r", exists=TRUE,length=TRUE, class = TRUE, values=TRUE,
          expr = n*index)

check.var("t", exists=TRUE,length=TRUE, class = TRUE, values=TRUE,
          expr =c(r[1,1]^2,r[2,2]^2,r[3,3]^2,r[4,4]^2))

#$ hints ######################################################
add.hint("hint.index",{cat('
 The following code is used to define the mean variance sums and 
 maxima for a vector x
 m = mean(x)
 v = var(x)
 s = sum(x)
 mx = max(x)      ')})

#$ end_exercise ###############################################

###############################################################
#$ exercise d) 
###############################################################
#$ task
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
# hint.for("{{ex_name}}")
#$ solution ###################################################
norm <- rnorm(100,0,5)
tr <- norm[norm>0]
bool <- tr>10
tr2 <- tr[bool]* 3
even <- norm[norm < -5 | norm > 5]
even2 <- cumsum (even[even > -7.5 & even < 7.5])

#$ tests ######################################################

check.var("tr", exists=TRUE, class = TRUE)
check.var("bool", exists=TRUE, class = TRUE)
check.var("tr2", exists=TRUE, class = TRUE)
check.var("even", exists=TRUE, class = TRUE)
check.var("even2", exists=TRUE, class = TRUE)

holds.true({
  tr.shall = norm[norm>0]
  tr.shall == tr
},
           failure.message = "Sorry, tr is not defined correctly."
)

holds.true({
  bool.shall = tr>10
  bool.shall == bool
},
           failure.message = "Sorry, bool is not defined correctly."
)


holds.true({
  tr2.shall = tr[tr>10]*3
  tr2.shall == tr2
},
           failure.message = "Sorry, tr2 is not defined correctly."
)
holds.true({
  even.shall = norm[norm < -5 | norm > 5]
  even.shall == even
},
           failure.message = "Sorry, even is not defined correctly."
)
holds.true({
  even2.shall = cumsum (even[even > -7.5 & even < 7.5])
  even2.shall == even2
},
           failure.message = "Sorry, even2 is not defined correctly."
)

#$ hints #############################################################
add.hint("3",{cat('
 The following code is used to find values > or < other values
  x <- y > 10  <--- all values of y greater 10 stored in x
  x <- y[y>10 | y< -10] <-- all values > 10 or < -10 of y
                  ')})

#$ end_exercise ######################################################
######################################################################
#$ exercise e)
######################################################################

#$ task ##############################################################
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

#$ solution #########################################################

df <- read.csv("table.csv",header=TRUE,sep=",")
df$meanhilo <- (df[,2] + df[,3])/2 
plot(df$meanhilo)
var <- var(df[,7])
for (i in 2:length(df[,7])) {
  ret[i-1]=df[i,7]-df[i-1,7]
}
plot(ts(ret))

#$ tests #############################################################
check.var("df", exists=TRUE,length=TRUE, values=TRUE, class = TRUE)
check.var("ret", exists=TRUE, length=TRUE, values=TRUE, class= TRUE)

#$ hints #############################################################

add.hint("4",{cat('
                  Dataframe columns can be accessed by df[,x] (x being the number of the column you 
                  want to access) or by df$columnname. You can also make new columns like that, if you 
                  just use an unused column name (column number)       
                  e.g. df$newname = df$oldname1 - df$oldname2                  
                  ')})

#$ end_exercise #####################################################
