
install.packages('Hmisc')
library(Hmisc)
install.packages('dplyr')
library(dplyr)
install.packages('tidyr')
library(tidyr)

db <- desi_2019_dataset

drop_na(db)

summary(is.na(db))
colonnes_numeriques <- names(db)[sapply(db, is.numeric)]
for (colonne in colonnes_numeriques) {
  ecart_type <- sd(db[[colonne]], na.rm = TRUE) 
  cat("Écart-type pour la colonne", colonne, ":", ecart_type, "\n")
}
summary(db, na.rm = TRUE)



