
# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(tidyr)

# Définir l'ordre des mois
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Filtrer et transformer les données
filtered_data <- results_all_G %>%
  filter(sexe == "femelle",
         Genre %in% c("ungulate_large", "ungulate_medium", "ungulate_small", "primates"),
         (!is.na(en_allaitement) & en_allaitement == "oui") | 
           (!is.na(enceinte) & enceinte == "oui")) %>%
  mutate(mois = factor(mois, levels = month_order),  # Ordonner les mois
         Genre = case_when(
           Genre %in% c("ungulate_large", "ungulate_medium", "ungulate_small") ~ "Ungulates",
           TRUE ~ "Primates"  # Changer ici pour "Primates"
         ))

# Fonction pour calculer les proportions par mois et genre
compute_proportion <- function(data, variable, variable_name) {
  total_per_month <- data %>%
    group_by(mois) %>%
    summarise(total = n(), .groups = "drop")  # Nombre total d'individus par mois
  
  data %>%
    filter(!!sym(variable) == "oui") %>%
    group_by(mois, Genre) %>%
    summarise(count = n(), .groups = "drop") %>%
    left_join(total_per_month, by = "mois") %>%
    mutate(proportion = round(count / total, 2),
           variable = variable_name)  # Ajouter la variable 'variable'
}

# Calculer les proportions pour l'allaitement et la grossesse
data_lactating <- compute_proportion(filtered_data, "en_allaitement", "Lactating")
data_pregnant <- compute_proportion(filtered_data, "enceinte", "Pregnant")

# Combiner les deux ensembles de données
combined_data <- bind_rows(data_lactating, data_pregnant)

# Assurer que tous les mois sont représentés, même avec 0
combined_data <- complete(combined_data, mois = factor(month_order, levels = month_order), Genre, variable, fill = list(count = 0, proportion = 0))

# Définition des couleurs
colors <- c("Ungulates" = "#A6CEE3", "Primates" = "#CAB2D6")  # Changer ici pour correspondre

# Graphique de 4 quadrants avec allaitement à droite et grossesse à gauche
ggplot(combined_data) +
  geom_bar(aes(x = mois, y = proportion, fill = Genre), 
           stat = "identity", position = "dodge") +
  
  # Disposition en 4 quadrants avec faceting
  facet_grid(rows = vars(Genre), cols = vars(variable), 
             scales = "fixed", space = "free") +  # Ajustement pour avoir les mêmes dimensions
  
  scale_fill_manual(values = colors) +
  labs(title = "",
       x = "Month", y = "Proportion") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +  # Limites et intervalles de l'axe y
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),  # Titres des quadrants en gras
        panel.grid = element_blank(),  # Suppression des petits carrés en arrière-plan
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.line.x = element_line(color = "black"),
        legend.position = "none")  # Pas de légende
