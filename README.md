# TP-ACT3035
nom-du-projet/
│
├── README.md                 # Description du projet (ce fichier)
│
├── data/
    getwd()
    setwd("C:/Users/marco/OneDrive/Bureau/travaux université/Trimestre 3/ACT3035")
    # lien pour trouver les données https://www.kaggle.com/datasets/mirichoi0218/insurance
    
    install.packages("tidyverse")
    library(tidyverse)
    
    assurance <- read.csv("insurance.csv")
    #changer les noms des colonnes en français
    assurance <- assurance %>% 
      rename(
      age=age,
      sexe=sex,
      imc=bmi,
      enfant=children,
      fumeur=smoker,
      region=region,
      reclamation=charges
    )
    
    # pré-traitement des données
    
    assurance <- assurance %>% 
      mutate(across(where(is.character), as.factor))
    
    x <- assurance %>% select(-reclamation)
    y <- assurance$reclamation
    
    n <- 1338
    indice_repartition <- sample(1:n, size=floor(.7*n))

    x_train <- x[indice_repartition,]
    x_test <- x[-indice_repartition,]
    y_train <- y[indice_repartition]
    y_test <- y[-indice_repartition]

    # statistique descriptive
    reclamation_max <- assurance[which.max(assurance[,"reclamation"]),]
    print(reclamation_max)
    reclamation_min <- assurance[which.min(assurance[,"reclamation"]),]
    print(reclamation_min)

    # boite à moustache
    ggplot(assurance, aes(x=fumeur, y=imc, fill=fumeur)) + geom_boxplot()
    ggplot(assurance, aes(x=fumeur, y=reclamation, fill=fumeur)) + geom_boxplot()
    ggplot(assurance, aes(x=region, y=reclamation, fill=region)) + geom_boxplot()
    ggplot(assurance, aes(x=fumeur, y=age, fill=fumeur)) + geom_boxplot()

│
├── src/
│   ├── 01_acquisition.R      # Script d'acquisition des données
│   ├── 02_nettoyage.R        # Script de nettoyage ETL
│   ├── 03_exploration.R      # Script d'analyse exploratoire
│   ├── 04_modelisation.R     # Script de modélisation
│   └── 05_comparaison.R      # Script de comparaison des modèles
│
├── docs/
│   └── rapport.md           # Rapport final
│
└── results/
    ├── figures/              # Graphiques générés
    └── tables/               # Tableaux de résultats
