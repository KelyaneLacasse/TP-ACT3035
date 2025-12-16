# TP-ACT3035
nom-du-projet/
│
├── README.md                 # Description du projet (ce fichier)
│
├── data/
    getwd()
    setwd("C:/Users/marco/OneDrive/Bureau/travaux université/Trimestre 3/ACT3035")
    # lien pour trouver les données https://www.kaggle.com/datasets/mirichoi0218/insurance

    assurance <- read.csv("insurance.csv")


│   └── processed/            # Données nettoyées et transformées
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
