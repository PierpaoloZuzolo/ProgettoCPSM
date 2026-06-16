# Caricamento libreria per indici di forma (asimmetria e curtosi)
#install.packages("e1071")
library(e1071)

# 0. CARICAMENTO DATI
#https://archive.ics.uci.edu/dataset/186/wine+quality
dati <- read.csv("winequality-red.csv", sep =";") #in questo file i dati sono separati da ;

# Definiamo le variabili di interesse per l'analisi
var_categ_discreta <- dati$quality  # Qualità del vino (discreta, punteggio)
var_continua_x <- dati$alcohol      # Gradazione alcolica (continua, per l'analisi bivariata)
var_continua_y <- dati$pH           # Livello di pH (continua, per l'analisi bivariata)

# 1. TABELLA DELLE FREQUENZE
cat("\n--- 1. TABELLA DELLE FREQUENZE ---\n")

# Frequenze assolute
freq_assolute <- table(var_categ_discreta)

# Frequenze assolute cumulative
freq_ass_cum <- cumsum(freq_assolute)

# Frequenze relative
freq_relative <- prop.table(freq_assolute)

# Frequenze relative cumulative
freq_rel_cum <- cumsum(freq_relative)

# Creazione della tabella riassuntiva
tabella_frequenze <- data.frame(
  Valore = names(freq_assolute),
  Freq_Assoluta = as.numeric(freq_assolute),
  Freq_Ass_Cumul = as.numeric(freq_ass_cum),
  Freq_Relativa = as.numeric(freq_relative),
  Freq_Rel_Cumul = as.numeric(freq_rel_cum)
)
print(tabella_frequenze)

# 2. RAPPRESENTAZIONI GRAFICHE
# Grafico a barre (per dati discreti/categorici)
barplot(freq_assolute, main="Grafico a Barre (Qualità)", xlab="Qualità", ylab="Frequenza", col="steelblue")

# Grafico a torta
pie(freq_assolute, main="Grafico a Torta (Qualità)")

# Istogramma (per dati continui)
hist(var_continua_x, main="Istogramma (Alcol)", xlab="Gradazione Alcolica", col="coral")

# Box Plot
boxplot(var_continua_x, main="Box Plot (Alcol)", xlab="Gradizione Alcolica", col="yellow", horizontal=TRUE)

# 3. INDICI DI POSIZIONE E VARIABILITÀ
cat("\n--- 3. INDICI DI POSIZIONE E VARIABILITÀ ---\n")

# Indici di posizione
media <- mean(var_continua_x, na.rm = TRUE)
mediana <- median(var_continua_x, na.rm = TRUE)

# Funzione personalizzata per calcolare la moda campionaria
calcola_moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
moda <- calcola_moda(var_continua_x)

cat("Media campionaria:", media, "\n")
cat("Mediana campionaria:", mediana, "\n")
cat("Moda campionaria:", moda, "\n")

# Indici di variabilità
varianza <- var(var_continua_x, na.rm = TRUE)
dev_standard <- sd(var_continua_x, na.rm = TRUE)
scarto_medio_assoluto <- mean(abs(var_continua_x - media), na.rm = TRUE)
ampiezza_campo <- max(var_continua_x, na.rm = TRUE) - min(var_continua_x, na.rm = TRUE)
coeff_variazione <- (dev_standard / abs(media)) * 100

cat("\nVarianza campionaria:", varianza, "\n")
cat("Deviazione standard campionaria:", dev_standard, "\n")
cat("Scarto medio assoluto:", scarto_medio_assoluto, "\n")
cat("Ampiezza del campo di variazione:", ampiezza_campo, "\n")
cat("Coefficiente di variazione (%):", coeff_variazione, "\n")

# 4. INDICI DI FORMA E QUARTILI
cat("\n--- 4. INDICI DI FORMA E QUARTILI ---\n")

# Indici di forma (richiede pacchetto e1071)
ind_asimmetria <- skewness(var_continua_x, na.rm = TRUE)
ind_curtosi <- kurtosis(var_continua_x, na.rm = TRUE)

cat("Indice di Asimmetria (Skewness):", ind_asimmetria, "\n")
cat("Indice di Curtosi:", ind_curtosi, "\n")

# Quartili
quartili <- quantile(var_continua_x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
cat("\nQuartili:\n")
print(quartili)

# 5. ANALISI BIVARIATA
cat("\n--- 5. ANALISI BIVARIATA ---\n")

# Coefficiente di correlazione campionario
coeff_correlazione <- cor(var_continua_x, var_continua_y, use = "complete.obs") #parametro use per evitare i valori NA
cat("Coefficiente di correlazione campionario (Alcol vs pH):", coeff_correlazione, "\n")

# Diagramma a dispersione (Scatterplot)
plot(var_continua_x, var_continua_y, 
     main="Diagramma a Dispersione (Scatterplot)", 
     xlab="Alcol", 
     ylab="pH", 
     pch=19, col="darkblue")

# Aggiunta di una linea di tendenza
abline(lm(var_continua_y ~ var_continua_x), col="red", lwd=2)
