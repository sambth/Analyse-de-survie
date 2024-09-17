---
title: "Consommation d'espace et ses déterminants"
author: "Sam TRAORE"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
 rmarkdown::html_document:
     theme: cerulean
     highlight: 
     df_print: paged
     number_sections: yes
     toc: yes
     toc_float:
       collapsed: TRUE
       smooth_scroll: TRUE
---


```{css toc-content, echo = FALSE}
 /* styles globale */
body {
  font-family: 'Arial', sans-serif;
  font-size: 16px;
  line-height: 0.7;
  color: #333;
}

/* Table des matières */
#TOC {
  color: #317eac;
  margin: 20px 0 10px 0;
}

/* entêtes */
h1, h2, h3, h4 {
  color: #159957;
}

h1 {
  font-size: 36px;
  text-align: center;
  background: linear-gradient(20deg, #155799, #159957);
  color: white;
}

h2, h3 {
  font-size: 24px;
  margin-top: 20px;
}

/* Paragraphes */
p {
  font-size: 18px;
  line-height: 1.5;
}

/* Images */
img {
  max-width: 100%;
  height: auto;
  margin-bottom: 20px;
}

/* Blocs de code */
pre {
  background-color: #f4f4f4;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 5px;
  overflow-x: auto;
}

/* Liste Groupe Elément Actif */
.list-group-item.active,
.list-group-item.active:hover,
.list-group-item.active:focus {
  color: white;
  background-color: #159957;
  background-image: linear-gradient(120deg, #155799, #159957);
}

/* Section d'auteur */
h4.author {
  text-align: center;
  font-size: 16px;
  color: white;
  padding: 8px 0;
  background: linear-gradient(120deg, #155799, #159957);
}

/* Section de l'en-tête */
#header {
  background: linear-gradient(120deg, #155799, #159957);
  
}

/* Table Styles */
table {
  border-collapse: collapse;
  width: 100%;
  margin-bottom: 20px;
}

table, th, td {
  border: 1px solid #ddd;
}

th, td {
  padding: 12px;
  text-align: left;
}

th {
  background-color: #159957;
  color: white;
}



```

 
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE,warning=FALSE,message=FALSE)
```

```{r}
library(rmdformats)
library(ggplot2)
library(cowplot)
library(scales)
library(corrplot)
library(MASS)
library(FactoMineR)
library(factoextra)
library(reshape2)
library(lmtest)
library(forecast)
library(QuantPsyc)
library(caret)
library(ModelMetrics)
library(glmnet)
library(olsrr)
library(survival)
library(survminer)
```

# Présentation des données

Notre dataset provient du site cerema du gouvernement Français et met en avant l'importance de lutter contre la consommation excessive d'espace, notamment en France et en Europe, pour préserver l'environnement. Ces données regroupent les surfaces qui ont été artificialisés en fonction de la destination des constructions : "activités", "habitat", "mixte" et "inconnu" dans plusieurs communes du territoire français entre 2009 et 2022.

En utilisant ces données, nous allons mener des études sur la survie des espaces naturels en fonction des différents cas d'utilisation dans l'objectif d'atteindre le "Zéro artificialisation nette" d'ici 2050.

Notre jeu de données initial est constitué de **34918 observations** (communes) et de **124 variables**.

* * *

```{r}
data_int <- read.csv("Consommation_espace_2009-2022", sep=";", header = TRUE)
#head(data_int)
```

```{r}
data <- data.frame(data_int$idcomtxt, data_int$idregtxt, data_int$iddeptxt, data_int$art09act22, data_int$art09hab22, data_int$art09mix22, data_int$art09inc22, data_int$art09rou22, data_int$art09fer22, data_int$naf09art22, data_int$artcom0922, data_int$pop19, data_int$emp19, data_int$surfcom2022, data_int$restsurfcom2022, data_int$artif)

colnames(data) <- c("commune", "region", "departement", "art09act22", "art09hab22", "art09mix22", "art09inc22", "art09rou22", "art09fer22", "naf09art22", "artcom0922", "pop19", "emp19", "surfcom2022", "restsurfcom2022", "status")
head(data)
```


* * *

## Description des variables

- Variables qualitatives :

|Variables | Description |
|:----------- |:-----------------|
| `Commune` | Nom de la commune |
| `Region` | Nom de la region |
| `Departement` | Nom du département |


- Variables quantitatives : 

|Varaibles | Description |
|:----------- |:-----------------|
| `art09act22` | Espace artificialisé destiné à l'activité entre 2009 et 2022 |
| `art09hab22` | Espace artificialisé destiné à l'habitat entre 2009 et 2022 |
| `art09mix22` | Espace artificialisé destiné aux mixtes entre 2009 et 2022 |
| `art09inc22` | Espace artificialisé dont la destination est inconnu entre 2009 et 2022 |
| `art09rou22` | Espace artificialisé destiné aux infrastructures routières entre 2009 et 2022 |
| `art09fer22` | Espace artificialisé destiné aux infrasttructures ferroviaires entre 2009 et 2022 |
| `artcom0922` | Part de surface communale artificialisé (%) entre 2009 et 2022 |
| `naf09art22` | Espace totale artificialisé entre 2009 et 2022 | 
| `pop19` | Population en 2019 |
| `emp19` | Nombre d'emploi en 2019 |
| `surfcom2022` | Surface communale en m² |
| `restsurfcom2022` | Surface communale restante en m² après artificialisation |
| `status` | 1 si la commune a été artificialisé et 0 sinon |

* * *

- Statistiques descriptives

```{r}
summary(data)
```
Nous remarquons que la commune **Serris** situé dans le  departement de **Seine-et-Marne** est la plus artificialisé avec environ **35%** de sa surface qui a été transformé en habitat, activité, route, etc... entre 2009 et 2022.

* * * 

## Analyse exploratoire

Première visualisation de nos données

```{r}

#Histogramme nombre totale de surface artificialisé
graphe <- ggplot(data, aes(x=naf09art22)) +     
 geom_histogram(aes(y=..density..), color="black", fill="orange")+
 geom_density(alpha=.01, color="red", fill="white")+
 labs(title="surface artificialisé", x="naf09art22", y="Densité")

#Boxplot 
box <- ggplot(data, aes(x="",y=naf09art22))+
  geom_boxplot(color="black", fill="orange")+
  labs(title="surface artificialisé")
```

```{r}
plot_grid(graphe,box, nrow=1, ncol=2)
```

Nous constatons que nous avons énormement de données abérantes donc notre graphique est complètement ecrasé.


```{r}
#Histogramme nombre totale de surface artificialisé
graphe_naf09art22 <- ggplot(data, aes(x=naf09art22)) +     
 geom_histogram(aes(y=..density..), color="black", fill="orange")+
 geom_density(alpha=.01, color="red", fill="white")+
 labs(title="Nombre totale de surface artificialisé", x="naf09art22", y="Densité")

#Boxplot nombre totale de surface artificialisé
box_naf09art22 <- ggplot(data, aes(x="",y=naf09art22))+
  geom_boxplot(color="black", fill="orange")+
  labs(title="Nombre totale de surface artificialisé")
```

```{r}
#Histogramme Part de surface artificialisé
graphe_artcom0922 <- ggplot(data, aes(x=artcom0922)) +     
 geom_histogram(aes(y=..density..), color="black", fill="blue")+
 geom_density(alpha=.01, color="red", fill="white")+
 labs(title="Part de surface artificialisé %", x="artcom0922", y="Densité") 

#Boxplot Part de surface artificialisé
box_artcom0922 <- ggplot(data, aes(x="",y=artcom0922))+
  geom_boxplot(color="black", fill="blue")+
  labs(title="Part de surface artificialisé %")
```

```{r}
#Histogramme de la population en 2019
graphe_pop19 <- ggplot(data, aes(x=pop19)) +     
 geom_histogram(aes(y=..density..), color="black", fill="grey")+
 geom_density(alpha=.01, color="red", fill="white")+
 labs(title="Population en 2019", x="pop19", y="Densité")

#Boxplot de la population en 2019
box_pop19 <- ggplot(data, aes(x="",y=pop19))+
  geom_boxplot(color="black", fill="grey")+
  labs(title="Poulation en 2019")
```

```{r}
#Histogramme nombre d'emploi en 2019
graphe_emp19 <- ggplot(data, aes(x=emp19)) +     
 geom_histogram(aes(y=..density..), color="black", fill="pink")+
 geom_density(alpha=.01, color="red", fill="white")+
 labs(title="Emploi en 2019", x="emp19", y="Densité")

#Boxplot Part de surface artificialisé
box_emp19 <- ggplot(data, aes(x="",y=emp19))+
  geom_boxplot(color="black", fill="pink")+
  labs(title="Emploi en 2019")
```

```{r}
#Histogramme Surface communale
graphe_surfcom2022 <- ggplot(data, aes(x=surfcom2022)) +     
 geom_histogram(aes(y=..density..), color="black", fill="brown")+
 geom_density(alpha=.01, color="red", fill="white")+
 labs(title="Surface communale", x="surfcom2022", y="Densité")

#Boxplot Surface communale
box_surfcom2022 <- ggplot(data, aes(x="",y=surfcom2022))+
  geom_boxplot(color="black", fill="brown")+
  labs(title="Surface communale")
```


```{r}
#Affichage des graphiques
#plot_grid(graphe_naf09art22,box_naf09art22, nrow=1, ncol=2)
#plot_grid(graphe_artcom0922,box_artcom0922, nrow=1, ncol=2)
#plot_grid(graphe_pop19,box_pop19, nrow=1, ncol=2)
#plot_grid(graphe_emp19,box_emp19, nrow=1, ncol=2)
#plot_grid(graphe_surfcom2022,box_surfcom2022, nrow=1, ncol=2)
```

* * *

# Estimation des fonctions de survie S, H et h

L'analyse de survie a pour objectif l'étude de caractéristiques d'une variable aléatoire modélisant une durée **T**. 

Dans notre cas,

**T** : La surface artificialisé entre 2009 et 2022.

**S(t)** : La fonction de survie de notre variable **T**. Ce qui correspond à la probabilité que la surface NAF d'une commune soit artificialisé.
$$S(t) = P(T > t) = 1 - F(t)$$

**h(t)** : La fonction de risque. Elle mésure le risque qu'une commune ait l'ensemble de sa surface artificialisé.
$$h(t) = \frac{f(t)}{S(t)}$$

**H(t)** : La fonction de risque cumulé. Elle mésure la probabilité que l'artificialisation d'un espace se produise avant un certain temps t.
$$H(t) = \int_{0}^{t} h(x) \, \mathrm{d}x = - ln(S(t))$$


* * *

## Estimateur de notre S, h et H

Nous observons une censure à gauche car nous n'avons pas les informations concernants les espaces NAF (Naturels, Agricoles et Forrestiers) qui ont été artificialisé avant 2009.

### Fonction de survie S

**Sans Censure**

En absence de censure, l'estimateur de notre fonction de survie est donnée par :
$$\hat{S}(t) = \frac{1}{n}\sum_{i=1}^n 1_{t_{i}>t}$$


```{r}
data$surv <- with(data, Surv(naf09art22))
naf_survie <- survfit(surv ~ 1, data= data)
#naf_survie
#plot(naf_survie, xlab = "Espace artificialisé", ylab = "S(t)", main = "Fonction de survie S(t)", col = "#9933CC")

```


```{r}
survplot <- ggsurvplot(naf_survie, data = data, xlab="Espace artificialisé", risk.table = TRUE, conf.int = T, title="Fonction de survie SANS censure", ggtheme = theme_minimal(), col = 'blue')
#survplot$plot
```

* * *

**Avec censure**

Dans ce cas, l'estimateur de notre fonction de survie S est definit par l'estimateur de Kaplan Meier :
$$\hat{S_{K M}}(t_i) = S(t_{i-1})(1-\frac{d_i}{n_i}) = \prod_{t_{i}< t}(1 - \frac{d_{i}}{n_{i}}) $$
avec :

- $\hat{S_{K M}}(t_i)$ : la probabilité qu'un espace ne soit pas artificialisé à l'instant t
- $d_{i}$ : la part d'espace NAF artificialisé à l'instant $t_{i}$
- $n_{i}$ : la part d'espace NAF qui risque d'être artificialisé juste avant $t_{i}$

* * *

```{r}
data$res_surf <- with(data, Surv(naf09art22, status==1))
naf_rest_survie <- survfit(res_surf~1, data= data, conf.type = "log-log")
#naf_rest_survie
```




```{r}
rest_survplot <- ggsurvplot(naf_rest_survie, data = data, xlab="Espace statusicialisé", risk.table = TRUE, conf.int = T, title="Fonction de survie AVEC censure", ggtheme = theme_gray(), col = 'red')
#rest_survplot$plot
```

* * *

**Les propriétés de l'estimateur de Kaplan-Meier**

- Biais et convergence

L'estimateur de **Kaplan-Meier** est un estimateur légèrement biaisé:
$$E(\hat{S_{K M}}(t)) < {S}(t)$$
où $E$ désigne l'espérance de l'estimateur de Kaplan-Meier.
Il est en revanche convergent : 
$$\forall\epsilon > 0, \; \lim_{n\to\infty} \mathbf{P}\left(|\hat{S_{K M}}(t)-{S}(t)|\ge\epsilon\right) = 0$$

Il est donc asymptotiquement sans biais : 
$$\lim_{n\to\infty} E(\hat{S_{K M}}(t)) = {S}(t)$$

- Variance

la variance de l'estimateur est donnée par :
$$\hat{Var}(\hat{S_{K M}(t)}) = \hat{S_{K M}}(t)²\sum_{t_i<t}\frac{d_i}{n_i(n_i-d_i)}$$

- Normalité asymptotique

**Propriété : ** L'estimateur de Kaplan-Meier $\hat{S_{K M}}$ suit asymptotiquement une loi Normale, cette propriété permet de déterminer un intervalle de confiance à partir de la variance de cet estimateur. 

En tout point de continuité de S : 

$$\sqrt{n}(\hat{S_{KM}}(t) - S(t))\xrightarrow{L} N(0,I(S(t))^{-1})$$ 

On peut donc faire une approximation par la loi normale et ainsi obtenir un intervalle de confiance à 5% :

$$IC(\alpha) = \left[\hat{S_{K M}}(t) \pm z_{\frac{\alpha}{2}}\sqrt{\hat{Var}(\hat{S_{K M}(t)})} \right]$$


```{r}
naf_rest_survie
```

**Conclusion : ** 

L'estimateur de Kaplan-Meier est convergent, asymptotiquement normal et presque sans biais quand le nombre d'espace à risque d'être artificialisé est grand.


* * *

- **Représentation graphique de la fonction de survie**


```{r}
plot_grid(survplot$plot,rest_survplot$plot, nrow=1, ncol=2)
```

La courbe de Kaplan-Meier est équivalente à la fonction de survie lorsqu'il n'y a pas de censure.

Nous constatons que la fonction de survie **SANS** censure et **AVEC** censure sont sensiblement les mêmes.

* * *

### Fonction de risque h(t)


**Représentation graphique de la fonction de risque**

```{r}
##### Sans censure
# dérivée de la fonction de survie S(t)
deriv_survie_s <- cumsum(naf_survie$surv)

h_t_s = naf_survie$surv / deriv_survie_s

##### Avec censure
# dérivée de la fonction de survie S(t)
deriv_survie_a <- cumsum(naf_rest_survie$surv)

h_t_a = naf_rest_survie$surv / deriv_survie_a


## Représentation graphique
#par(mfrow=c(1,2))
plot(h_t_s, xlab = "Espace artificialisé", ylab = "h(t)", main="Fonction de risque", col = "blue", type="s", lwd = 1.5)
points(h_t_a, col = "red", type="s")

legend("topright", legend=c("Sans censure","Avec censure"), col=c("blue","red"), lty=1:1)

```

Nous constatons que la fonction risque **SANS** censure et **AVEC** censure sont quasiment les mêmes.

* * *


### Fonction de risque cumulé H(t)


#### Estimation par Breslow

```{r}
H_t_s = -log(naf_survie$surv)
#plot(H_t_s, xlab="Espace artificialisé", ylab="Risque cumulé", main="Estimateur de Breslow", col='blue', type = "s")
```

```{r}
H_t_a = -log(naf_rest_survie$surv)
#plot(H_t_a, xlab="Espace artificialisé", ylab="Risque cumulé", main="Estimateur de Breslow", col='blue', type = "s")
```

* * *

| SANS censure | AVEC censure |
|:------------- |:--------------------------------------- |
| $\hat{H}_{Breslow}(t) = -log~(\hat{S}(t))$ | $\hat{H}_{Breslow}(t) = -log~(\hat{S_{K M}}(t)) = -\sum_{t_i \leq t} log \left(1 - \frac{d_i}{n_i}\right)$ |

La variance de l'estimateur de Breslow est donnée par :
$$\hat{Var}(\hat{H}_{Breslow}(t)) = \sum_{t_i \le t} \frac{d_i}{n_i(n_i - d_i)}$$

avec :

- $d_{i}$ : la part d'espace NAF artificialisé à l'instant $t_{i}$

- $n_{i}$ : la part d'espace NAF qui risque d'être artificialisé juste avant $t_{i}$

* * * 

- **Représentation graphique**

```{r}
par(mfrow=c(1,2))
plot(naf_survie, fun="cumhaz", xlab="Espace artificialisé", ylab="Risque cumulé", main="SANS censure", col='blue')
plot(naf_rest_survie, fun="cumhaz", xlab="Espace artificialisé", ylab="Risque cumulé", main="AVEC censure", col='red')
```

Nous constatons une forte augmentation du risque au cours du temps pour se stabiliser à 30.000 m².

* * *

#### Estimation par Nelson-Aalen

Il est basé sur la relation : $\int_0^t H(x) dx$ et est definit par:
$$\hat{H}_{NA}(t) = \sum_{i=1}^n\frac{d_i}{n_i} \times \mathbb{I_{(t_i <= t)}}$$

La variance de l'estimateur de Nelson-Aalen est :
$$\hat{Var}(\hat{H}_{NA}(t)) = \sum_{t_i \leq t} \frac{d_i}{n_i²}$$

* * *

- **Représentation graphqiue**

```{r}
#sans censure
s = summary(naf_survie)

tps_s <- s$time
h_na_s <- s$n.event/s$n.risk  # estimation de h; correspond à d_i/n_i dans la formule ci-dessus
H_na_s <- cumsum(h_na_s) # estimation H avec l'estimateur de Nelson_Aalen
#plot(tps_s, H_na_s, xlab="Espace artificialisé", ylab="Risque cumulé", main="Estimateur de Nelson-Aalen", col = "blue", type="s")
```

```{r}
#avec censure
tps_a <- naf_rest_survie$time
h_na_a <- naf_rest_survie$n.event/naf_rest_survie$n.risk  # estimation de h; correspond à d_i/n_i dans la formule ci-dessus
H_na_a <- cumsum(h_na_a) # estimation H avec l'estimateur de Nelson_Aalen
#plot(c(0,tps_a),c(0,H_na_a), xlab="Espace artificialisé", ylab="Risque cumulé", main="Estimateur de Nelson-Aalen", col = "blue", type="s")
```



```{r}
#par(mfrow=c(1,2))
plot(c(0,tps_s),c(0,H_na_s), fun="cumhaz", xlab="Espace artificialisé", ylab="Risque cumulé", main="Estimation par Nelson-Aalen", col = "blue", type="s")
#points(tps_a,col=2,type='s', pch=4)
points(c(0,tps_a),c(0,H_na_a), fun="cumhaz", col = "red", type="s")
legend("topleft", legend=c("Sans censure","Avec censure"), col=c("blue","red"), lty=1:1)


```

- En présence ou en absence de censure, nous avons le même graphique.

- Le risque cumulé augmente au fil des années. Ce qui veut dire que le risque qu'un espace soit artificialisé devient plus important à chaque fois qu'on avance au cours du temps dû à l'augmentation de la population et à la construction des habitats.


#### Comparaison : estimateurs de Nelson-Aalen et Breslow


```{r}
s = summary(naf_rest_survie)

HNA=cumsum(s$n.event/s$n.risk)
#ou HNA=cumsum(hNA)
HBreslow=-log(naf_rest_survie$surv) 
plot(s$time,HNA,type='s', xlab='Espace artificialisé')
points(naf_rest_survie$time,HBreslow,col=2,type='s', pch=4)
legend("topleft", legend=c("Nelson-Aalen","Brelow"), col=c("black","red"), title="Estimateurs", lty=1:1)

```

Graphiquement, les deux estimateurs sont très proches pour estimer la fonction de risque cumulé H(t).

* * *

# Estimation Paramétrique 

```{r}
espace_artf1 <- data$naf09art22[data$naf09art22 <= 1000000] # Variable espace artificialisé en m² 
espace_artf2 <- data$naf09art22[data$naf09art22 > 1000000]


espace_artf1 <- espace_artf1[espace_artf1!=0]
espace_artf2 <- espace_artf2[espace_artf2!=0]
```


## Test de conformité

<center><h4> **Tableau des modèles :** </h4></center>

|Modèle | Formule de repartition | Fonction de survie |
|:----------- |:-----------------| :--------------|
| `Weibull` | $F(t) = 1 - e^{(\frac{t - \gamma}{\mu})^2},  si : t 	\geqslant \gamma$ | $S(t) = e^{(\frac{t - \gamma}{\mu})^2},  si : t 	\geqslant \gamma$ |
| `Exponentielle` | $F(t) = 1 - e^{- \lambda t}$ | $S(t) = e^{- \lambda t}$ |
| `Gamma` | $F(x)=\frac{\int_0^xt^{\beta x-1}e^{-t}dt}{\mathbb{Γ}(\alpha)}$ | $S(t) = 1 - \frac{\int_0^xt^{\beta x-1}e^{-t}}{\mathbb{Γ}(\alpha)}$ |

* * *

### Loi Weibull

La fonction de répartition de la loi de Weibull est :   
$F(t) = 0, si : t	< \gamma$  
avec $\beta$, $\mu > 0$, $\gamma \geqslant 0$.

où :

- $\gamma$ : Décalage à l'origine du temps    
- $\beta$ : Paramètre de forme (donne le type de défaillance étudié)    
- $\mu$ : Paramètre d'échelle      

```{r}
library(fitdistrplus)
library(MASS)
estimation1 <- fitdist(s$time[s$time!=0], "weibull", method = "mle")
#summary(estimation1)

weibull1 <- fitdistr(espace_artf1, densfun="weibull")
weibull1

#AIC et BIC
weibull_aic <- AIC(weibull1)
weibull_bic <- BIC(weibull1)

#weibull_aic
#weibull_aic
```



```{r}
plot(estimation1)
```

* * *

### Loi exponentielle

La loi **exponentielle** est très utilisée dans les études d'analyse de survie, puisqu'elle est **"sans mémoire"**.

```{r}
exp = fitdist(s$time[s$time!=0],"exp", method = "mme")
#summary(exp)

expo1 <- fitdistr(espace_artf1, densfun="exponential", lower = 0)
expo1

#AIC et BIC
expo_aic <- AIC(expo1)
expo_bic <- BIC(expo1)

#expo_aic
#expo_bic
```

```{r}
x_simule=c(seq(0,10000000,1000))
loi_simule_exp=dexp(x_simule,exp$estimate[1])
hist(s$time[s$time>0],freq = FALSE)
lines(x_simule,loi_simule_exp, col = "#9933CC")
```


L'allure de la loi exponentielle  est la même que celle de l'histogramme. 

* * *

### Loi gamma

Une **variable** aléatoire $\mathbb X$ suit une loi gamme de paramètres $k$ et $\theta$ , $X \sim \mathcal{\mathbb{Γ}}(k,\,\theta)$ si sa densité est : 
$$F(x)=\frac{\int_0^xt^{\beta x-1}e^{-t}dt}{\mathbb{Γ}(\alpha)}$$
```{r}
gamma <- fitdist(s$time[s$time!=0],"gamma", method = "mme")
#summary(gamma)

#gamma1 <- fitdistr(espace_artf1, densfun="gamma", lower=0)

#AIC et BIC
#gamma_aic <- AIC(weibull1)
#gamma_bic <- BIC(weibull1)

#gamma_aic
#gamma_aic
```

```{r}
plot(gamma)
```

* * *

### Loi Log-normal

```{r}
lognorm <- fitdistr(espace_artf1, densfun="lognormal", lower = 0)
lognorm

lognorm_aic <- AIC(lognorm)
lognorm_bic <- BIC(lognorm)

#lognorm_aic
#lognorm_bic
```


* * *

**Rappel des critères de sélections de modèles**: <br>

- Critère d'information d'Akaike : 
$AIC = 2k - 2ln(L)$ où $k$ est le nombre de paramètres à estimer du modèle et $L$ est le maximum de la fonction de vraisemblance du modèle. <br>


- Critère d'information bayésien : 

$BIC =-2ln(L)+k.ln(N)$ avec $L$ la vraisemblance du modèle estimée, $N$ le nombre d'observations dans l'échantillon et $k$ le nombre de paramètres libres du modèle.

* * *

<center><h4> **Tableau récaputilatif des modèles avec AIC et BIC :** </h4></center>

|Modèle | AIC | BIC |
|:----------- |:-----------------| :--------------|
| `Weibull` | <span style="color: green;">*835412.2*</span> | <span style="color: green;">*835412.2*</span> |
| `Gamma` | <span style="color: red;">*857048.7*</span> |<span style="color: red;">*857065.3*</span> |
| `Exponentielle` |<span style="color: red;">*841211.7*</span> | <span style="color: red;">*841220.1*</span> |
| `log-normal` |<span style="color: red;">*835935*</span> | <span style="color: red;">*835951.9*</span> |



D'après le critère d'AIC et de BIC, le meilleur modèle est la **loi de Weibull**.

* * *

## Test paramétrique

### Test de Kolmogorov

**Hypothèses :**

- H0 :  la survie des espaces naturels est la même, quel que soit le type d'utilisation qui leur est attribué.

- H1 : Il existe des différences significatives dans la durée de vie des espaces naturels entre les différentes catégories d'utilisation des terres


#### Loi de Weibull

```{r}
set.seed(10)
p <- length(espace_artf1)
w <- rweibull(p,7.524022e-01, 6.867605e+04)

#test
ks.test(w,espace_artf1)
```


#### Loi Exponentielle

```{r}
set.seed(10)
e <- rexp(p, 1.204935e-05)

ks.test(e,espace_artf1)
```


#### Loi Gamma

```{r}
ks_result <- ks.test(s$time[s$time!=0], "pgamma", shape = gamma$estimate[1], rate = gamma$estimate[2])
ks_result
```

#### Loi Log-normal

```{r}
set.seed(10)
l <- rlnorm(p, 3.79,1.63)

ks.test(l,espace_artf1)  

```

* * *

### Test de Wilcoxon-Mann-Whitney

**Hypothèses : **

- H0 : Il n'y a pas de différence significative dans la durée de vie des espaces naturels entre les différentes catégories d'utilisation des terres (activités, habitat, mixte, inconnu)

- H1 : Il existe une différence significative dans la durée de vie des espaces naturels entre au moins deux catégories d'utilisation des terres


#### Loi de Weibull

```{r}
#ks.test(w,espace_artf1)
wilcox.test(w, espace_artf1)
```

* * *

#### Loi exponentielle

```{r}
wilcox.test(e, espace_artf1)
```
* * *

#### Loi Log-normal

```{r}
wilcox.test(l, espace_artf1)
```

* * *

<center><h4> **Tableau récaputilatif des tests :** </h4></center>

|Modèle | Kolmogorov | Wilcoxon-Mann-Whitney |
|:--------------- |:-----------------| :------------------ |
| `Weibull` | <span style="color: green;"> < 2.2e-16 </span> | <span style="color: green;"> 5.416e-07 </span> | 
| `Exponentielle` | <span style="color: red;"> 5.416e-07 </span> | <span style="color: red;"> < 2.2e-16 </span> | 
|`Gamma` | <span style="color: red;"> < 2.2e-16 </span> | <span style="color: red;"> < 2.2e-16 </span> |
|`Log-normal` | <span style="color: red;"> < 2.2e-16 </span> | <span style="color: red;"> < 2.2e-16 </span> |

* * *

```{r}
o = cbind(data$time, data$status)

#t=rweibull(n,2,3) 
#Attention: W(a,lambda)
# sous R  S(x) =  exp(- (x/lambda)^a) 
# nous  S(x) =   exp(- (lambda*x)^a)

##proportion des ?l?ments censur?s

### 4 EMV
lv=function(par){-sum((log(par[1])-par[1]*log(par[2])+(par[1]-1)*log(o[,1]))-
                      (o[,1]/par[2])^par[1]) # Formule pour estimer les paramtres d'une weibull de (a, 1/lambda)
}

#nlm(lv,c(.1,.1))#EMV
```


* * *

# Estimation en présence de covariable

**Covariable pop (population)**

L'augmentation de la population peut influcer le risque d'artificialisation de l'espace naturel. On prendra 3 classes de population.

- pop100K : population inférieure à 100000 habitants

- pop100K_300K : population comprise entre à 101000 et 300000 habitants

- popSup_300K : population supérieure à 300000 habitants

* * *

## Estimation non paramétrique

### Estimateur de Kaplan-Meier

```{r}
# Création classes d'âge
data$classe_pop = ifelse(data$pop19 < 100000, "-100k_hbts",
                         ifelse(data$pop19 >= 100000 & data$pop19 <= 300000, "100k_hbts-300k_hbts", "+300k_hbts")) 
                                #ifelse(data$pop19 >= 500000, "+500m_hbts")))
```

```{r}
km_classe_pop <- survfit(surv ~ classe_pop, 
                         data = data, 
                         conf.type = "log-log") 
```

```{r}
plot(km_classe_pop, 
     main= "Estimateur de Kaplan-Meier (survie pour nos 3 classes de population)",
     col = c("#6666CC", "#9933CC", "#990066"),
     xlab = "Espace artificialisé")

legend("topright", 
       legend = c("-100k_hbts", "100k_hbts-300k_hbts", "+300k_hbts"),
       col = c("#6666CC", "#9933CC", "#990066"), 
       pch = 19, 
       bty = "n")
```

La courbe de *-100k_hbts* se démarque des autres et décroit plus rapidement, suivi de celle *100k_hbts-300k_hbts* qui s'ecrase autour de 3 millions de m² d'espaces artificialisés.

La courbe de *+300k_hbts* décroit moins vite.

* * *

### Estimateur de Nelson-Aalen

```{r}
mdl_classe1 <- survfit(surv ~ 1,
                       data = data, 
                       subset = classe_pop == "-100k_hbts")
mdl_classe2 <- survfit(surv ~ 1,
                       data = data,
                       subset = classe_pop =="100k_hbts-300k_hbts")
mdl_classe3 <- survfit(surv ~ 1,
                       data = data,
                       subset = classe_pop == "+300k_hbts")

km_mdl1 <- summary(mdl_classe1)
km_mdl2 <- summary(mdl_classe2)
km_mdl3 <- summary(mdl_classe3)


sum_mdl1 <- cumsum(km_mdl1$n.event / km_mdl1$n.risk)
sum_mdl2 <- cumsum(km_mdl2$n.event / km_mdl2$n.risk)
sum_mdl3 <- cumsum(km_mdl3$n.event / km_mdl3$n.risk)

```


```{r, fig.height=5}
plot(km_mdl1$time,
     sum_mdl1, 
     type="s", 
     main= "Risque cumulé pour nos 3 classes de population", 
     col = "#6666CC",
     xlab = "Espace artificialisé")

lines(km_mdl2$time, sum_mdl2, type="s", col = "#9933CC") 
lines(km_mdl3$time, sum_mdl3, type="s", col = "#990066") 
#lines(km_mdl4$time, sum_mdl4, type="s", col = "#FF9999")

legend("topleft", 
       legend = c("-100k_hbts", "100k_hbts-300k_hbts", "+300k_hbts"),
       col = c("#6666CC", "#9933CC", "#990066"), 
       pch = 19, 
       bty = "n")
```

* * *

### Estimateur de Breslow

```{r}
mdl_classe1 <- survfit(surv ~ classe_pop,
                 data = data, 
                 subset = classe_pop=="-100k_hbts")

mdl_classe2 <- survfit(surv ~ classe_pop,
                 data = data, 
                 subset = classe_pop=="100k_hbts-300k_hbts")

mdl_classe3 <- survfit(surv ~ classe_pop,
                 data = data, 
                 subset = classe_pop=="+300k_hbts")

```

```{r}
plot(mdl_classe1$time,
     -log(mdl_classe1$surv),
     type="s",
     col="#6666CC",
     main="Risque cumulé pour nos 3 classes de population", 
     xlab = "Espace artificialisé")

lines(mdl_classe2$time,-log(mdl_classe2$surv),type="s",col="#9933CC")
lines(mdl_classe3$time,-log(mdl_classe3$surv),type="s",col="#990066")

legend("topleft", 
       legend = c("-100k_hbts", "100k_hbts-300k_hbts", "+300k_hbts"),
       col = c("#6666CC", "#9933CC", "#990066"), 
       pch = 19, 
       bty = "n")
```

On remarque que deux catégories se décinent pour les deux estimateurs (Breslow et  Nelson-Aalen) avec une qui croit plus rapidement que l'autre.

- Les *100k_hbts* dont leur courbe de risque augmente et finit l'étude à 1.

- Les *"100k_hbts-300k_hbts* et les plus de *+300k_hbts* dont les courbes de risques augmentent de la même manière. Mais quand l'espace artificialisé attaint les 250000m², la courbe de risque de *+300k_hbts* s'arrête à 1.9. Celle de *100k_hbts-300k_hbts* accroisse encore pour finir l’étude à 3.9 lorque l'espace artificialisé atteint les 550000m².

* * *

### Quelques statistiques descriptives, par classes de population

```{r}
print(km_classe_pop, print.rmean=TRUE)
```


* * *

## Modèle de Cox

Le modèle de Cox permet de modéliser des temps de survie avec des données censurées. Le principe est de relier la date d’arrivée d’un évènement à des variables explicatives. Le modèle de Cox s’applique à toute situation où l’on étudie le délai de survenue d’un événement.

la formule du modèle de Cox est la suivante :
$$h(t,X_i) = h_0(t)e^{\gamma(X_i)}$$
où :

- $\gamma(X_i) = \beta_1X_1 + \beta_2X_2+ ... +\beta_kX_k$

- $h$ : le risque

- $h_0$ : le risque de base

- $t$ : le temps

- $X_i$ : les variables explicatives ou prédictives

- $\beta_i$ : les coefficients de la régression

```{r}
library(survival)
model_cox<- coxph(Surv(data$naf09art22, data$status==1) ~ data$emp19+data$surfcom2022+data$pop19, data = data)
summary(model_cox)
```

On constate que l'ensemble de nos covariables sont significatifs avec p-value < 0,05.


**Selection de modèle avec StepAIC**

```{r}
library(MASS)
model_cox2 <- stepAIC(model_cox)
aic_cox <- extractAIC(model_cox2)
summary(model_cox2)
```

* * *

**Obtention des coefficients du modèle**

```{r}
library(broom, quietly = TRUE)
broom::tidy(model_cox2, exponentiate = TRUE)
```

```{r}
#install.packages("gtsummary")
library(gtsummary)
t2 <-
  model_cox %>% tbl_regression(exponentiate = TRUE)

t2
```


* * *

**Représentation des rapports de risques**

Afin d'avoir une meilleure lisibilité des rapports de risques, nous avons créé le graphique suivant :

```{r}
library(GGally, quietly = TRUE)
library(broom.helpers)
ggcoef_model(model_cox2, exponentiate = TRUE)
```

On constate que d'après le graphique nos covariables ont effectivement des p-values < 0,05. Ce qui confirme la significativité.

### Test des hypothèses

#### Hypothèses du hasard proportionelle

Il est nécessaire de vérifier les hypothèses de validité du modèle : Un modèle de Cox n’est valable que sous l’hypothèse de la proportionnalité des risques relatifs. Selon cette hypothèse les résidus de Schoenfeld ne dépendent pas du temps.

```{r}
#install.packages("flexsurv")
library(survminer)
library(flexsurv)
test_valid <- cox.zph(model_cox2)
test_valid
```

Une p-value inférieure à 5 % indique que l’hypothèse n’est pas vérifiée. Ici, on voit que les p-values sont, globalement et pour chaque variable prise individuellement, inférieures à 5%. Donc nous ne pouvons pas validé notre modèle de Cox.


Représentons la distribution des résidus de *Schoenfeld* afin de voir si la répartition des coefficients change ou non au cours du temps.

Il doit y avoir absence de tendance temporelle. Pour cela, si le plot est une droite horizontale, alors H0 vrai, sinon on rejette H0.

```{r fig.height=7.5}
ggcoxzph(test_valid)

```

* * *

#### Hyphotèses du log-linéarité des variables continues

```{r}
model <- coxph(Surv(data$naf09art22, data$status==1) ~ data$emp19+data$pop19+data$surfcom2022, data = data)

ggcoxfunctional(model, data=data)
```

On voit que pour nos variables `emp19` et `pop19`nous avons une forte concentration de nos données entre 0 et 150000.
Ensuite, au envirion 500000 emplois pour la variable `emp19`, nous avons une ligne droite et pour la variable `pop19` nous observons la ligne droite au environ de 1000000 habitants.
Enfin, nous avons plusieurs variations donc pas de droite pour la variable `surfcom2022`.

* * *

## Modèle de Weibull en présence de covariables

```{r}
weib = data
weib <- na.omit(weib)

# pour calculer la médiane de la variable t
median <- median(weib$naf09art22)

# er remplacer les zéros par la médiane dans une nouvelle variable t_modified
weib$Naf09art22 <- ifelse(weib$naf09art22 == 0, median, weib$naf09art22)

#summary(weib$Naf09art22)


#install.packages("SurvRegCensCov")
library(SurvRegCensCov)
#survreg(Surv(t_modifier, status) ~  surfcom2022 + pop19, data=weib, dist = "weibull")

```


```{r}
weibull_cov <- WeibullReg(Surv(Naf09art22, status) ~ emp19 + surfcom2022 + pop19, data = weib, conf.level = 0.95)
weibull_cov
#summary(weibull_cov)
```

Le coefficient estimé pour l’effet de la population est de -1.621023e-04, et le HR (hazard ratio = rapport des risques instantanés d'artificialisation) correspondant est de 0.9998379 (=exp(-1.621023e-04)). Le risque qu'un terrain soit artificialiser augmente quand la population augmente, et la augmentation est de l’odre de 5% pour chaque année supplémentaire. 

* * *

## Modèle exponentiel en présence de covariables

```{r}
exp_cov <- survreg(Surv(Naf09art22, status) ~ emp19 + surfcom2022 + pop19, data = weib, dist = "exponential")
summary(exp_cov)
```

On obtient une estimation du paramètre pour l’effet de la population (coefficient beta pour la covariable pop19) (1.91e-04) égale à celle obtenue avec l’hypothèse Weibull (avec survreg).

* * *

Le HR (hazard ratio) correspondant pour chaque covariable est de :

```{r}
exp(coef(exp_cov))

```

* * *

## Modèle log-normal en présence de covariables

```{r}
lognormal_cov <- survreg(Surv(Naf09art22, status) ~ emp19 + surfcom2022 + pop19, data = weib, dist = "lognormal")
summary(lognormal_cov)
```

* * *

```{r}
Weibull_cov <- survreg(Surv(Naf09art22, status) ~ emp19 + surfcom2022 + pop19, data = weib, dist = "weibull")
#exp_cov, lognormal_cov, weibull_cov
#BIC(model_cox)
```


## Sélection du modèle

<center><h4> **Tableau  des modèles :** </h4></center>


|Modèles sans covariables | AIC | BIC |
|:----------- |:-----------------| :--------------|
| `Weibull` | <span style="color: green;">*835412.2*</span> | <span style="color: green;">*835412.2*</span> |
| `Gamma` | <span style="color: red;">*857048.7*</span> |<span style="color: red;">*857065.3*</span> |
| `Exponentielle` |<span style="color: red;">*841211.7*</span> | <span style="color: red;">*841220.1*</span> |
| `log-normal` |<span style="color: red;">*835935*</span> | <span style="color: red;">*835951.9*</span> |


| Modèles avec covariables |  |  |
|:-----------|:-----------------|:--------------|
| `Weibull` | <span style="color: green;">*837659.8*</span> | <span style="color: green;">*837702.1*</span> |
| `Exponentielle` |<span style="color: red;">*839040.6*</span> | <span style="color: red;">*839074.5*</span> |
| `log-normal` |<span style="color: red;">*843340.5*</span> | <span style="color: red;">*843382.8*</span> |
| `Cox` | <span style="color: blue;">*642049.7*</span> | <span style="color: blue;">*642075.1*</span>  | 




En comparant les modèles en présence de covariables, nous constatons que le `modèle de Cox` a l'AIC le plus petit. Ce modèle n'étant pas valide, nous choisissons alors le `modèle de Weibull`.

* * * 

# Conclusion et perspective

## Conclusion

En conclusion, cette analyse de survie des données de Cerema nous offre des perspectives précieuses et des résultats pour informer les décisions stratégiques visant à minimiser la consommation excessive d'espace et à préserver l'environnement en guidant les planificateurs urbains et les responsables politiques afin d'atteindre les objectifs de durabilité pour l'environnement.

* * *

## perspectives

Nous avons d'autres variables que nous pouvons inclure dans notre études afin de voir leur impact sur la consommation des espaces naturels notamment en faisant des prévisions avec des modèles de Machine Learning tels que le `Random Forest` et `XGBoost`.

