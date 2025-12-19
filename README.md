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
    summary(assurance)
    reclamation_max <- assurance[which.max(assurance[,"reclamation"]),]
    print(reclamation_max)
    reclamation_min <- assurance[which.min(assurance[,"reclamation"]),]
    print(reclamation_min)

    # corrélation entre les variables numériques (âge, imc, réclamation, nombre d'enfants)
    assurance %>% 
      select(where(is.numeric)) %>% 
      cor() %>% 
      heatmap(Rowv = NA, Colv = NA)

    # boite à moustache
    ggplot(assurance, aes(x=fumeur, y=imc, fill=fumeur)) + geom_boxplot()
    ggplot(assurance, aes(x=fumeur, y=reclamation, fill=fumeur)) + geom_boxplot()
    ggplot(assurance, aes(x=region, y=reclamation, fill=region)) + geom_boxplot()
    ggplot(assurance, aes(x=fumeur, y=age, fill=fumeur)) + geom_boxplot()

    #Nuage de point pour voir s'il y a une tendance
    assurance %>% 
      select(reclamation, age) %>% 
      ggplot(aes(x=age,y=reclamation))+
      geom_point(alpha=.99)+              
      geom_smooth(method="loess")+
      scale_y_log10()+                                      #pour mieux voir
      labs(title="Relation âge-réclamation (échelle log)",
           x="Âge",
           y="Réclamation")
           
    assurance %>% 
      select(reclamation, imc) %>% 
      ggplot(aes(x=imc,y=reclamation))+
      geom_point(alpha=.99)+              
      geom_smooth(method="loess")+
      scale_y_log10()+
      labs(title="Relation IMC-réclamation (échelle log)",
           x="IMC",
           y="Réclamation")

    #histogramme pour déterminer la fréquence des variables
    hist(assurance$reclamation)
    hist(assurance$age)
    hist(assurance$imc)

    #histogramme à barre des réclamations moyennes selon le nombre d'enfant
   assurance %>% 
      group_by(enfant) %>% 
      summarise(reclamation_moy=mean(reclamation)) %>% 
      ggplot(aes(x=enfant, y = reclamation_moy))+
      geom_col()+
      labs(title="Réclamation moyenne selon le nombre d'enfant",
           x=NULL,
           y="Réclamation moyenne")

    # Rajouter catégorie d'âge pour mieux déterminer la relation avec les réclamations
    # Rajouter la catégorie âge dans assurance
    assurance <- assurance %>% 
      mutate(
        categorie_age= cut(
          age,
          breaks = c(0,25,50,70, Inf),
          labels=c("Jeunes","Adultes","Seniors","Ainés"),
          right=FALSE))
          
    #Histogramme relation entre catégorie d'âge et réclamations      
    assurance %>% 
      group_by(categorie_age) %>% 
      summarise(reclamation_moy=mean(reclamation)) %>% 
      ggplot(aes(x=categorie_age, y = reclamation_moy))+
      geom_col()+
      labs(title="Réclamation moyenne selon la catégorie d'âge",
           x=NULL,
           y="Réclamation moyenne")

    #Discussion:
    
    #Il y a une limite pour déterminer la corrélation entre les réclamations et les autres varaibles,
    #car certaines ne sont pas numérique comme la variable fummeur qui pourrait avoir une corrélation avec 
    #les réclamations. Cependant, avec les boîtes à moustache, on peut remarquer qu'il y a une corrélation
    #entre fumeur et les réclamations, ce qui signifie que les risquent que les fumeurs fassent une réclamations
    #plus élevé sont plus élevé que les non fumeurs.

    # Le nuage de point entre l'âge et les réclamations peut permettre de voir qu'il y a une certaine corrélation
    # positive entre ces deux variables, lorsqu'une personne devient plus âgée, il y a plus de chance qu'elle
    # fasse des réclamations plus élevées. Cela peut aussi s'expliquer logiquemment que plus on vieillit, plus on 
    # peut avoir des problèmes de santé, donc avoir des réclamations plus élevé à l'assurance vie (santé) par les
    # coût des soins médicaux.

    # Avec le nuage de points pour déterminer la relation entre l'imc et les réclamations n'est pas concluant.
    # Il semble avoir une légère augmentation entre 30 et 40 de l'imc, mais on ne peut déterminer une réelle
    # relation entre ces deux variables avec ce graphique.

    # L'histogramme à barre permet de démontrer qu'il n'a pas vraiment de relation entre le nombre d'enfant
    # et le montant des réclamations de l'assuré.
    
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
