Livrable 5 - Séries temporelles et reproductibilité
================
Claudie-Maude Canuel
14 avril 2020

### 1. Activer les modules nécessaires

    library("packrat")
    library("lubridate")
    library("rmarkdown")
    library("tidyverse")
    library("forecast")
    library("e1071")

### 2. Importer le tableau de données

    hawai <- read.csv("Data/hawai.csv") # Importer le tableau
    hawai %>% head(10) # Visualiser les 10 premières lignes et l'entête
    dim(hawai) # Visualiser les dimensions de la matrice: il y a 526 observations
    summary(hawai) # Visualiser rapidement l'étendu des données

### 3. Créer une série temporelle du CO<sup>2</sup>

    # Générer une série temporelle
    hawai_ts <- ts(hawai %>%
        dplyr::select(CO2),
        start = c(1958, 3), # Débuter en mars 1958
        frequency = 12)
    hawai_ts # Visualiser la série temporelle de mars 1958 à décembre 2001

### 4. Séparer la série temporelle en deux parties (entrainement et test)

La partie entrainement devra contenir les premiers 70% des données, soit les premières 368 données (70% x 526), c'est-à-dire les 30.667 premières années (368 mois/12 mois an<sup>-1</sup>) du jeu de données, correspondant au temps 1988.85. La partie test devra contenir le 30% des données restantes.

    # Détermination de l'année de séparation
    annee <- 1958.167 + (0.7*526/12)
    annee # Visualiser

    # Partie entrainement
    hawai_ent <- hawai_ts %>%
          window(end = annee)
    hawai_ent # Visualiser

    # Partie test
    hawai_test <- hawai_ts %>%
          window(start = annee)
    hawai_test # Visualiser

### 5. Générer un modèle ETS avec les données d'entrainement, projeter la prévision, puis analyser ses résidus

Pour y parvenir, il faut d'abord générer un modèle avec la fonction *ets* du module *forecast*, puis utiliser la fonction *forecast* pour générer une prévision.

    hawai_model1 <- hawai_ent %>% ets() # Générer le modèle
    hawai_model1 # Visualiser les paramètres du modèle

    hawai_ets1 <- hawai_model1 %>% 
          forecast(h=12*(2002-annee)) # Générer la prédiction

    # Analyser visuellement à l'aide d'un graphique
    autoplot(hawai_ets1) + # Générer le graphique du modèle 
          autolayer(hawai_test, color = "red") + # Superposer les données test
          autolayer(fitted(hawai_ets1)) # Superposer la prévision afin de comparer

Le modèle obtenu est un modèle ETS(M,Ad,M). Cela signifie que le type d'erreur est multiplicatif, le type de tendance est adouci et le type de saison est multiplicatif. La valeur de *phi* est de 0.9756. L'*AIC* est de 1401.189. Nous sommes en présence d'un modèle avec tendance adoucie qui n'est pas optimal. En effet, la présence d'un *phi* non nul fait voir que le lissage n'est pas adéquat. Bien que le graphique montre que la prévision ETS (rose) prend bien des valeurs similaires aux données d'entrainement (noir), il montre surtout une divergence importante quant à la prévision des données test (la prévision est en bleu, alors que les données test sont en rouge). Cette divergence semble causée par l'adoucissement de la tendance générale. Ainsi, il serait sans doute préférable d'obtenir un modèle sans adoucissement. Avant de générer ce nouveau modèle, regardons les résidus du modèle ETS(M,Ad,M).

    # Analyser les résidus du modèle ETS(M,Ad,M)
    accuracy(hawai_ets1, hawai_ts)

    checkresiduals(hawai_model1)

L'erreur moyenne absolue échelonnée (MASE) est relativement faible pour la partie entrainement (0.202144) alors qu'elle s'éloigne considérablement de 0 pour la partie test (4.216469), ce qui confirme nos observations quant à la mauvaise performance du modèle au-delà de la partie entrainement.

Avec le graphique de l'autocorrélation, on peut remarquer que les résidus semblement structurés, ce qui compromet la validité du modèle. Le Ljung-Box test indique une *p-value* de 1.34e<sup>-08</sup>, ce qui rejette l'hypothèse que les résidus forment un bruit blanc. Il semble donc y avoir une tendance dans les résidus qui n'est pas prise en compte par le modèle ETS(M,Ad,M). Cela est causé par trois points qui vont au-delà de la zone de signifiance (seuil de 0.05) dans le graphique de l'autocorrélation.

De plus, l'histogramme montre que les résidus semblent distribués selon une distribution normale. Toutefois, il pourrait en être autrement. L'amplitude de la «cloche» est assez grande et nous pouvons remarquer la présence de valeurs aberrantes de part de d'autre de la courbe. Il importe de tester la distribution des résidus.

    # Tester la distribution des résidus
    shapiro.test(residuals(hawai_ets1)) 

    kurtosis(residuals(hawai_ets1), na.rm = TRUE) 

Effectivement, les tests précédents indiquent que les résidus ne sont pas distribués selon une une distribution normale. Le test de Shapiro-Wilk donne une p-value significative (3.903e<sup>-07</sup>) rejetant la normalité, alors que le test de Kurtosis donne une valeur de 2.992117, bien supérieure à 0. Cela qui indique la possibilité d'une non validité des intervalles prévisionnels.

Je vais maintenant générer un second modèle ETS sans adoucissement.

    hawai_model2 <- hawai_ent %>%
          ets(damped = FALSE) # Retirer l'adoucissement
    hawai_ets2 <- hawai_model2 %>%
          forecast(h=12*(2002-annee)) # Prédiction

    # Comparer visuellement à l'aide de graphiques
    autoplot(hawai_ets2) + # Générer le graphique du modèle 
          autolayer(hawai_test, color = "red") + # Superposer les données test
          autolayer(fitted(hawai_ets2)) # Superposer la prédiction afin de comparer

Nous obtenons ainsi un modèle ETS(M,A,A). Cela signifie que le type d'erreur est multiplicatif, le type de tendance est additif et le type de saison est additif. Le paramètre *phi* est maintenant nul, ce qui signifie que l'adoucissement n'est effectivement pas nécessaire. Comme les modèles sont différents, on ne peut comparer l'*AIC* qui est maintenant de 1410.34.

    # Analyser les résidus du modèle ETS(M,A,A)
    accuracy(hawai_ets2, hawai_ts)

    checkresiduals(hawai_model2)

La MASE est semblable pour la partie entrainement (0.2054700), alors qu'elle s'approche beaucoup plus de 0 pour la partie test (0.6165975), ce qui confirme nos observations quant à la meilleure performance de ce modèle.

Néanmoins, il semble encore y avoir une structure dans les résidus qui n'est pas prise en compte par le modèle. Le test de Ljung-Box a une p-value significative (1.398e<sup>-09</sup>), ce qui appui cette hypothèse. Cela est clairement visible sur le graphique d'autocorrélation des résidus, qui montre une structure des résidus davantage cyclique, alors que 4 points vont au-delà du seuil de 0.05. Ce résultat pourrait expliquer le choix du modèle ETS initial.

L'histogramme montre encore une fois une distribution des résidus qui semble normale, mais avec quelques valeurs aberrantes.

    # Tester la distribution des résidus
    shapiro.test(residuals(hawai_ets2)) 

    kurtosis(residuals(hawai_ets2), na.rm = TRUE)

Le test de Shapiro-Wilk indique une p-value de 2.41e<sup>-06</sup> alors que le test de Kurtosis montre une valeur de 2.685623, ce qui indique que les résidus ne suivent finalement pas une distribution normale, tout comme pour le modèle précédent. Cela indique que les intervalles prévisionnels ne sont probablement pas valides. Cela n'affecte toutefois pas la validité du modèle.

### 7. Conclusion

Le modèle ETS(M,Ad,M) généré, même s'il utilise une méthode optimisée, n'est pas valide puisqu'il s'éloigne considérablement de la réalité. En retirant l'adoucissement de la tendance, il est possible d'obtenir un modèle ETS(M,A,A) qui semble plus valide. En effet, malgré qu'il soit peu probable que ses résidus forment un bruit blanc, l'analyse graphique présentait une grande similitude entre les valeurs test et les valeurs prévisionnelles. De plus, il présentait une MASE s'approchant de 0 pour les deux parties, entrainement et test.

Le dernier modèle concorde également avec la documentation sur la crise climatique actuelle, dont le principal coupable serait l'augmentation continuelle du CO<sup>2</sup> atmosphérique. L'augmentation continuelle du CO<sup>2</sup> atmosphérique représente une tendance générale bien documentée. Un modèle qui adoucit cette tendance serait erroné.

Il demeure qu'il importerait de jeter un oeil aux valeurs aberrantes qui peuvent contribuer à la non validité du modèle. Il serait surtout pertinent d'utiliser d'autres types de modèles, comme des modèles dynamiques, qui peuvent tenir compte d'une série de covariables explicatives, puisqu'il semble clair que le modèle ne tient pas compte de certains facteurs. Par exemple, l'augmentation de la température, la déforestation ainsi que les habitudes de consommation pourraient être d'autres facteurs explicatifs liées à la fluctuation du CO<sup>2</sup> au fils du temps.
