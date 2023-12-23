install.packages('Hmisc')
library(Hmisc)

install.packages('stats')
library(stats)

install.packages('dplyr')
library(dplyr)

install.packages('FactoMineR')
library(FactoMineR)

install.packages('factoextra')
library(factoextra)

install.packages('tidyr')
library(tidyr)

install.packages("openxlsx")
library(openxlsx)

install.packages("carData")
library(car)

install.packages("GGally")
library(GGally)

db <- X92_1_db

cell_empty <- sum(is.na(db))
all_cell <- length(unlist(db))
print(all_cell)
prop_celvide <- (cell_empty/all_cell)
print(prop_celvide)

db <- replace(db, is.na(db), "Not mentioned")

sapply( lapply (db, unique), length)


for (col in colnames(db)) {
  db[[col]] <- ifelse(db[[col]] == "DK", "Not mentioned", db[[col]])
}

var_using <- c('using_web_placehome', 'using_web_placework', 'using_web_onmobile', 'using_web_placeother', 'using_web_total', 'using_web_socialmedia', 'using_web_purchase')
for (modal_freq in var_using) {
  db[[modal_freq]] <- ifelse(db[[modal_freq]] %in% c('Two or three times a week', 'About once a week', 'Two or three times a month', 'Less often', 'Often/sometimes'), 'partiellement', db[[modal_freq]])
}

var_using <- c('using_web_placehome', 'using_web_placework', 'using_web_onmobile', 'using_web_placeother', 'using_web_total', 'using_web_socialmedia', 'freq_info_gdpr', 'using_web_purchase')
for (modal_freq in var_using) {
  db[[modal_freq]] <- ifelse(db[[modal_freq]] %in% c('Never', 'No Internet access (SPONT.)', 'No Internet access', 'Never/no access'), 'jamais', db[[modal_freq]])
}

db$data_control_modif <- ifelse(db$data_control_modif %in% c('It depends on the website or application (SPONTANEOUS)', 'Partial control'), 'partiellement', db$data_control_modif)

db$data_nocontrol_fear <- ifelse(db$data_nocontrol_fear %in% c('Not very concerned', 'Fairly concerned'), 'partiellement', db$data_nocontrol_fear)

db$freq_info_gdpr <- ifelse(db$freq_info_gdpr %in% c('You are never asked to provide personal information online (SPONTANEOUS)', 'Never'), 'jamais', db$freq_info_gdpr)

db$conf_param_manage_level <- ifelse(db$conf_param_manage_level %in% c("Fairly difficult", "Very difficult"), "difficile",
                                     ifelse(db$conf_param_manage_level %in% c("Fairly easy", "Very easy"), "facile",
                                            db$conf_param_manage_level))

db$freq_info_gdpr <- ifelse(db$freq_info_gdpr %in% c('Rarely', 'Sometimes'), 'Sometimes', db$freq_info_gdpr)

db$cnil_existence <- ifelse(db$cnil_existence %in% c('Yes and you know which public authority is responsible', "Yes, but you don't know which public authority is responsible"), 'oui', db$cnil_existence)

db$gdpr_existence <- ifelse(db$gdpr_existence %in% c('Yes and you know what it is', "Yes, but you don't know exactly what it is"), 'oui', db$gdpr_existence)

var_gdpr <- c('data_right_access', 'data_nomarketing', 'data_false_erase', 'data_forget_erase', 'data_noalgo_fordecise', 'data_notransfer_supplier')
for (modal_gdpr in var_gdpr) {
  db[[modal_gdpr]] <- ifelse(db[[modal_gdpr]] %in% c('Yes and you have exercised it', 'Yes but you have not exercised it'), 'oui', db[[modal_gdpr]])
}


db$bloc_west <- ifelse(db$country %in% c('BE - Belgium', 'DK - Denmark', 'GR - Greece', 'ES -Spain', 'FI - Finland', 'FR - France', 'IE - Ireland', 'IT - Italy', 'LU - Luxembourg', 'NL - The Netherlands', 'AT - Austria', 'PT - Portugal', 'SE - Sweden', 'DE-W - Germany - West', 'DE-E Germany East', 'GB-UKM - United Kingdom', 'MT - Malta', 'CY - Cyprus (Republic)'), 1, 0)
prop.table(table(db$bloc_west))

db$using_web_socialmedia <- ifelse((db$using_web_socialmedia != "jamais" & db$using_web_socialmedia != "Not mentioned"), 1, 0)
db$using_web_total <- ifelse((db$using_web_total != "jamais" & db$using_web_total != "No Internet access at all"), 1, 0)
db$gdpr_acceptfor_payment <- ifelse((db$gdpr_acceptfor_payment !=  "Not mentioned" | db$gdpr_acceptfor_purchase != "Not mentioned" & db$gdpr_acceptfor_purchase != "jamais"), 1, 0)
db$gdpr_acceptfor_service <- ifelse((db$gdpr_acceptfor_acceservices != "Not mentioned" | db$gdpr_acceptfor_freeservices != "Not mentioned" | db$gdpr_acceptfor_persoservices != "Not mentioned") , 1, 0)
db$gdpr_acceptfor_communicate <- ifelse((db$gdpr_acceptfor_communicate != "Not mentioned"), 1, 0)
db$gdpr_acceptfor_offer <- ifelse((db$gdpr_acceptfor_reduction != "Not mentioned" | db$gdpr_acceptfor_offers != "Not mentioned"), 1, 0)
db$gdpr_acceptfor_others <- ifelse((db$gdpr_acceptfor_wintime != "Not mentioned" | db$gdpr_acceptfor_others != "Not mentioned"), 1, 0)
db$gdpr_refuse <- ifelse((db$gdpr_refuse != "Not mentioned"), 1, 0)
db$data_control_modif <- ifelse((db$data_control_modif != "Not mentioned" & db$data_control_modif != "No control at all"), 1, 0)
db$data_nocontrol_fear <- ifelse((db$data_nocontrol_fear != "Not mentioned" & db$data_nocontrol_fear != "Not at all concerned"), 1, 0)
db$conf_param_manage <- ifelse((db$conf_param_manage_level != "Not mentioned" & db$conf_param_manage_level != "difficile" | db$socialmedia_conf_param != "No" & db$socialmedia_conf_param != "Not mentioned"), 1, 0)
db$freq_info_gdprv2 <- ifelse((db$freq_info_gdpr != "jamais" & db$freq_info_gdpr != "Not mentioned"), 1, 0)
db$conf_readml <- ifelse((db$conf_readml != "Not mentioned" & db$conf_readml != "You do not read them at all"), 1, 0) 
db$ml_noclear <- ifelse((db$conf_noreadml_nofound != "Not mentioned" | db$conf_noreadml_toolong != "Not mentioned" | db$conf_noreadml_noclear != "You find them unclear or difficult to understand"), 1, 0)
db$reglementation_know <- ifelse((db$cnil_existence != "No" & db$cnil_existence != "Not mentioned" | db$gdpr_existence != "Not mentioned" & db$cnil_existence != "No"), 1, 0)


summary(db)
ecart_types <- lapply(db, sd)
print(ecart_types)

correlation_matrix <- ggpairs(db[, c("using_web_socialmedia","using_web_total","gdpr_acceptfor_payment","gdpr_acceptfor_service","gdpr_acceptfor_communicate","gdpr_acceptfor_others","gdpr_refuse","data_control_modif","data_nocontrol_fear","conf_param_manage","freq_info_gdprv2","conf_readml","ml_noclear","reglementation_know")]) 
print(correlation_matrix)

 
modele_europe <- lm(average_broadband_coverage~conf_readml+data_control_modif+freq_info_gdpr+gdpr_refuse+socialmedia_conf_param+data_nocontrol_fear+data_right_access+data_forget_erase+data_false_erase+gdpr_acceptfor_payment+data_nomarketing+data_notransfer_supplier+gdpr_acceptfor_purchase+conf_param_manage_level+data_noalgo_fordecise+gdpr_existence+conf_noreadml_toolong+cnil_existence+gdpr_acceptfor_acceservices,data=db)
summary(modele_europe)

residus_studentises <- rstudent(modele_europe)
plot(residus_studentises, ylab = "Residuals")
hist(residus_studentises, main = "Histogram of Residuals")


