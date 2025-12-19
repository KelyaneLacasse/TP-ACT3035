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
    ![Image](https://github.com/KelyaneLacasse/TP-ACT3035/blob/8ae02ef3e1b7b5711bc802534e9bbd71a794e483/Corr%C3%A9lation%20entre%20les%20variables%20num%C3%A9riques.png)

    # calculer corrélation entre donnée et réclamation
    r_age <- cor(assurance$reclamation, assurance$age)
    r_imc <- cor(assurance$reclamation, assurance$imc)
    r_enfant <- cor(assurance$reclamation, assurance$enfant)
    #Pas vraiment concluant, mais on peut remarquer que les réclamations ne sont pas corrélées avec le nombre d'enfant

    # boite à moustache
    ggplot(assurance, aes(x=fumeur, y=imc, fill=fumeur)) + geom_boxplot()
    ![image alt](https://github.com/KelyaneLacasse/TP-ACT3035/blob/7eeacfc9629452ba66efaa17f04e425c6b2a4773/Boite%20%C3%A0%20moustache%20(fumeur-imc).png)

    ggplot(assurance, aes(x=fumeur, y=reclamation, fill=fumeur)) + geom_boxplot()
    ![image alt](https://github.com/KelyaneLacasse/TP-ACT3035/blob/5a1bd1e56ab03be44dac5fc4720879eec36b031f/Boite%20%C3%A0%20moustache%20(fumeur-r%C3%A9clamation).png)
    
    ggplot(assurance, aes(x=region, y=reclamation, fill=region)) + geom_boxplot()
    ![image alt](https://github.com/KelyaneLacasse/TP-ACT3035/blob/318a78bed3c6abd186a8486ddfc569e33658c839/Boite%20%C3%A0%20moustache%20(r%C3%A9gion-r%C3%A9clamations).png)
    
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
    ![image alt](https://github.com/KelyaneLacasse/TP-ACT3035/blob/4330fe1353fdd1140ceced9fc44387dfd61b3a93/Relation%20%C3%A2ge-r%C3%A9clamation.png)
           
    assurance %>% 
      select(reclamation, imc) %>% 
      ggplot(aes(x=imc,y=reclamation))+
      geom_point(alpha=.99)+              
      geom_smooth(method="loess")+
      scale_y_log10()+
      labs(title="Relation IMC-réclamation (échelle log)",
           x="IMC",
           y="Réclamation")
    ![image alt](https://github.com/KelyaneLacasse/TP-ACT3035/blob/d150dd3b79670b68060d16e9467d399d32b48d94/Relation%20IMC-r%C3%A9clamation.png)

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
    ![image alt](https://github.com/KelyaneLacasse/TP-ACT3035/blob/1a1cfb982683501bbeb09768f36daa9f7ef7378a/R%C3%A9clamation%20moyenne%20selon%20nombre%20d'enfant.png)

    # Rajouter catégorie d'âge pour mieux déterminer la relation avec les réclamations
    # Rajouter la catégorie âge dans assurance
    assurance <- assurance %>% 
      mutate(
        categorie_age= cut(
          age,
          breaks = c(0,25,50,70, Inf),
          labels=c("Jeunes","Adultes","Seniors","Ainés"),
          right=FALSE))
    #Pour ces données, il n'y a pas d'assuré sous la catégorie "Ainés"
          
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
    # Cependant, avec les catégories d'âge créer, on peut mieux remarquer la relation entre l'âge et le montant
    # des réclamations faits. La catégorie sénior fait en moyenne des plus hautes réclamations que les autres groupes.

    # Avec le nuage de points pour déterminer la relation entre l'imc et les réclamations n'est pas concluant.
    # Il semble avoir une légère augmentation entre 30 et 40 de l'imc, mais on ne peut déterminer une réelle
    # relation entre ces deux variables avec ce graphique.

    # L'histogramme à barre permet de démontrer qu'il n'a pas vraiment de relation entre le nombre d'enfant
    # et le montant des réclamations de l'assuré.

    # Définition du problème actuarielle:
    # Pour la modélisation, il pourrait être intéressant d'analyser les réclamations selon l'âge, être fumeur, l'imc.
    # Avec l'analyse des grapiques, histogrammes, on peut s'attendre qu'avec la modélisation, les réclamations
    # varient selon l'âge, le fait d'être fumeur et probablement l'imc aussi. 
    # La modélisation sur les réclamations pourra permettre aux assurances de mieux ajuster leur tarification envers 
    # leur assuré. En ajustant mieux leur tarification, ça évite que l'assurance soit en déficit à cause d'un risque pas 
    # bien couvert. Par exemple, on pourrait s'attendre que la tarification soit plus élevé si l'assuré est fumeur et/ou
    # plus âgé et/ou à une plus forte imc.

    # Modélisation avec modèle linéaire généralisé

    modelisation_glm <- glm(reclamation ~ .,
                         data = cbind(x_train,
                                      reclamation=y_train))
    summary(modelisation_glm)
    
    y_pred <- predict(modelisation_glm, newdata = x_test,
                      type = "response")
    y_pred
    #Il y a des réclamations négatives?
    
    # Valeur significative selon ce modèle:
    # L'âge, car elle augmente de 267,13$ par an
    # IMC, car elle augmente de 344,14 quand elle augmente de 1
    # Fumeur, augmente de 24 162,89$ s'ils sont fumeurs, donc la variable la plus significative
    # Enfant, augmente de 638,57 par enfant
    # De plus les probabilité Pr(>|t|) sont très petites, donc significative
    
    # Valeur non significative selon ce modèle:
    # Le sexe si c'est un homme ou une femme, les régions ne crée pas de différence significative sur le montant des réclamations

    # Modélisation régression linéaire
    modelisation_lm <- lm(reclamation ~ .,
                          data = assurance)
    summary(modelisation_lm)

    # Valeur significative selon ce modèle:
    # L'âge, car elle augmente de 242,73$ par an
    # IMC, car elle augmente de 344,00$ quand elle augmente de 1
    # Fumeur, augmente de 23 890,81$ s'ils sont fumeurs, donc la variable est la plus significative à comparer des autres
    # Enfant, augmente de 633,00$ par enfant
    # De plus les probabilité Pr(>|t|) sont très petites, donc elles ont un effet sur les réclamations faites
    
    # Valeur non significative selon ce modèle:
    # Le sexe et les régions ne crée pas de différence significative sur le montant des réclamations faites à l'assurance vie

│   └── 05_comparaison.R      # Script de comparaison des modèles
│
├── docs/
│   └── rapport.md           # Rapport final
│
└── results/
    ├── figures/              # Graphiques générés
    └── tables/               # Tableaux de résultats
