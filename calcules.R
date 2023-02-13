require(ggplot2)
#importation du jeu de données
levee <- read.csv(file.choose(), header = TRUE)
head(levee)
str(levee)

#visualisation de la distribution
hist(levee$Temps)


levee$Variete <- as.factor(levee$Variete) 
boxplot(Temps ~ Variete, data = levee,
        ylab = "Temps",
        xlab = "Variété", cex.lab = 1.2)

ggplot()+
  geom_boxplot(data=levee, aes(x=Variete, y=Temps, fill=Variete))

m1 <- aov(Temps ~ Variete, data = levee)
par(mfrow = c(1, 2))
plot(m1)


levee$log.Temps <- log(levee$Temps)
m2 <- aov(log.Temps ~ Variete, data = levee)
par(mfrow = c(1, 1))
plot(m2)

#sqrt( )
levee$sqrt.Temps <- sqrt(levee$Temps)
m3 <- aov(sqrt.Temps ~ Variete, data = levee)
par(mfrow = c(1, 1))
plot(m3)

#carre( )
levee$carre.Temps <- (levee$Temps)^2
m3 <- aov(carre.Temps ~ Variete, data = levee)
par(mfrow = c(1, 1))
plot(m3)

#inv( )
levee$inv.Temps <- 1/(levee$Temps)
m3 <- aov(inv.Temps ~ Variete, data = levee)
par(mfrow = c(1, 1))
plot(m3)


summary(m1)

#### test de Tukey
##moyennes des groupes
moy <- tapply(X = levee$Temps, INDEX = levee$Variete, FUN = mean)
##en ordre croissant et en arrondissant aÌ deux deÌcimales
round(sort(moy), digits = 2)
TukeyHSD(m2, which = "Variete")


## Graphique des resultats
moy.orig <- tapply(X = levee$Temps, INDEX = levee$Variete, 
                   FUN = mean)
moy.orig
moy <- sort(moy.orig)
MSE <- summary(m2)[[1]][2, "Mean Sq"]
MSE
##calculer racine carrée de MSE
sqrt.MSE <- sqrt(MSE)
sqrt.MSE
##calculer les limites des barres d'erreur
lim.sup <- moy + sqrt.MSE
lim.inf <- moy - sqrt.MSE
##création d'un graphique vide sans axe des x's
par(mfrow = c(1, 1))
plot(x = 0, y = 0, type = "n",
     ylim = c(min(lim.inf), max(lim.sup)+1),
     xlim = c(0, 4), xlab = "Variété de laitue",
     ylab = "Temps de la levée (heure)",
     main = "Moyennes ± racine-carrée de MSE",
     xaxt = "n")
##ajouter axe des x's
axis(side = 1, at = c(1, 2, 3),
     labels = names(moy))
##ajouter moyennes
points(x = c(1, 2, 3),
       y = moy)
##ajouter barres d'erreurs
arrows(x0 = c(1, 2, 3),
       y0 = lim.inf,
       x1 = c(1, 2, 3),
       y1 = lim.sup, length = 0.05,
       angle = 90, code = 3)
##ajouter les lettres, lim.sup + 0.05
text(x = 1, y = lim.sup[1] + 0.55, labels = "a")
text(x = 2, y = lim.sup[2] + 0.55, labels = "ab")
text(x = 3, y = lim.sup[3] + 0.55, labels = "b")


res.glm.qp <- glm(Temps~Variete, data=levee, family = quasipoisson)
summary(res.glm.qp)

par(mfrow = c(2, 2))
plot(res.glm.qp)


# Comparaison des variétés. 
library(multcomp)
amod_glht <- glht(res.glm.qp, linfct = mcp(Variete = "Tukey"))
summary(amod_glht)


library(fitdistrplus)
descdist(levee$inv.Temps, discrete = FALSE)


Varietes <- c("Arroyo","Arroyo","Arroyo","Arroyo","Arroyo","Arroyo",
              "Arroyo","Arroyo","Arroyo","Arroyo","Arroyo","Arroyo",
              "Arroyo","Arroyo","Arroyo","Arroyo","Arroyo","Arroyo",
              "Susana","Susana","Susana","Susana","Susana","Susana",
              "Susana","Susana","Susana","Susana","Susana","Susana",
              "Susana","Susana","Susana","Susana","Susana","Susana",
              "Tropicana","Tropicana","Tropicana","Tropicana",
              "Tropicana","Tropicana","Tropicana","Tropicana",
              "Tropicana","Tropicana","Tropicana","Tropicana",
              "Tropicana","Tropicana","Tropicana","Tropicana",
              "Tropicana","Tropicana")
sample(Varietes, 54)


