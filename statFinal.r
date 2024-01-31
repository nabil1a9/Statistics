df=read.csv(file=file.choose())
str(df)
names(df)
dim(df)
attach(df)
summary(df)
df
# on a remarqué des valeurs manquantes dans les variables Class , Eccentricity
#df$Class[which(df$Class=="Kecimen")]=0
#df$Class[which(df$Class=="Besni")]=1
summary(df)
df$Class 
#df$Class=strtoi(df$Class)
summary(df)

sum(is.na(df))/prod(dim(df))
str(df)

df$Class[which(df$Class=="")]=NA


boxplot(df$Area)
Vmin=58239-1.5*(103057-58239)
Vmax=103057+1.5*(103057-58239)
b=which(df$Area<Vmin)
a=which(df$Area>Vmax)
df$Area[a]=NaN
df$Area[b]=NaN
boxplot(df$Area)

df$Area
summary(df$Area)


boxplot(df$MajorAxisLength)
summary(df$MajorAxisLength)
var_score=(487.7-340.8)*1.5
v_max=487.7 + var_score
v_min=340.8 - var_score
df$MajorAxisLength[which(df$MajorAxisLength>v_max)]=NA
df$MajorAxisLength[which(df$MajorAxisLength<v_min)]=NA
summary(df)


boxplot(df$MinorAxisLength)
summary(df$MinorAxisLength)
var_score=(277.2 - 218.0   )*1.5
v_max=277.2 + var_score
v_min=218.0 - var_score
df$MinorAxisLength[which(df$MinorAxisLength>v_max)]=NA
df$MinorAxisLength[which(df$MinorAxisLength<v_min)]=NA
summary(df)
boxplot(df$MinorAxisLength)


boxplot(df$Eccentricity)
summary(df$Eccentricity)
var_score=(0.8410 - 0.7401   )*1.5
v_max=0.8410 + var_score
v_min=0.7401 - var_score
df$Eccentricity[which(df$Eccentricity>v_max)]=NA
df$Eccentricity[which(df$Eccentricity<v_min)]=NA
summary(df)
boxplot(df$Eccentricity)


boxplot(df$ConvexArea)
summary(df$ConvexArea)
var_score=(107313 - 59977   )*1.5
v_max=107313 + var_score
v_min=59977 - var_score
df$ConvexArea[which(df$ConvexArea>v_max)]=NA
df$ConvexArea[which(df$ConvexArea<v_min)]=NA
summary(df)
boxplot(df$ConvexArea)


boxplot(df$Extent)
summary(df$Extent)
var_score=(0.7344 - 0.6709   )*1.5
v_max=0.7344 + var_score
v_min=0.6709 - var_score
df$Extent[which(df$Extent>v_max)]=NA
df$Extent[which(df$Extent<v_min)]=NA
summary(df)
boxplot(df$Extent)


boxplot(df$Perimeter)
summary(df$Perimeter)
var_score=(1295.4 - 949.9)*1.5
v_max=1295.4 + var_score
v_min=949.9 - var_score
df$Perimeter[which(df$Perimeter>v_max)]=NA
df$Perimeter[which(df$Perimeter<v_min)]=NA
summary(df)
boxplot(df$Perimeter)

install.packages("visdat")
library(visdat)
vis_miss(df)

install.packages("VIM")
library(VIM)
datt=kNN(df)
datt[ , 9:16] <- list(NULL)

datt$Class[which(datt$Class=="Kecimen")]=0
datt$Class[which(datt$Class=="Besni")]=1

summary(datt)
names(datt)
View(datt)
sum(is.na(datt))


boxplot(datt$Area)
boxplot(datt$MajorAxisLength)
boxplot(datt$MinorAxisLength)


df
View(datt)





#################################TACHE3
library(dplyr)
library(ggplot2)
library(gapminder)
library(ggpubr)



ggpubr::ggqqplot(datt$Area)
shapiro.test(datt$Area)
ggplot(df, aes(x=datt$Area))+
  geom_histogram(color="darkblue", fill="lightblue")
ggplot(df, aes(x=datt$Area))+
  geom_histogram(color="darkblue", fill="lightblue")
#Area ne suit pas la loi normal

ggpubr::ggqqplot(datt$MajorAxisLength)
shapiro.test(datt$MajorAxisLength)
ggplot(df, aes(x=datt$MajorAxisLength))+
  geom_histogram(color="darkblue", fill="lightblue")
ggplot(df, aes(x=datt$MajorAxisLength))+
  geom_histogram(color="darkblue", fill="lightblue")
# MajorAxisLength ne suit pas la loi normal

ggpubr::ggqqplot(datt$MinorAxisLength)
shapiro.test(datt$MinorAxisLength)
ggplot(df, aes(x=datt$MinorAxisLength))+
  geom_histogram(color="darkblue", fill="lightblue")
ggplot(df, aes(x=datt$MinorAxisLength))+
  geom_histogram(color="darkblue", fill="lightblue")
# MinorAxisLength ne suit pas la loi normal


ggpubr::ggqqplot(datt$Eccentricity)
shapiro.test(datt$Eccentricity)
ggplot(df, aes(x=datt$Eccentricity))+
  geom_histogram(color="darkblue", fill="lightblue")
ggplot(df, aes(x=datt$Eccentricity))+
  geom_histogram(color="darkblue", fill="lightblue")
# Eccentricity ne suit pas la loi normal

ggpubr::ggqqplot(datt$ConvexArea)
shapiro.test(datt$ConvexArea)
ggplot(df, aes(x=datt$ConvexArea))+
  geom_histogram(color="darkblue", fill="lightblue")
ggplot(df, aes(x=datt$ConvexArea))+
  geom_histogram(color="darkblue", fill="lightblue")
# ConvexArea ne suit pas la loi normal

ggpubr::ggqqplot(datt$Extent)
shapiro.test(datt$Extent)
ggplot(df, aes(x=datt$Extent))+
  geom_histogram(color="darkblue", fill="lightblue")
ggplot(df, aes(x=datt$Extent))+
  geom_histogram(color="darkblue", fill="lightblue")
# Extent ne suit  pas la loi normal

ggpubr::ggqqplot(datt$Perimeter)
shapiro.test(datt$Perimeter)
ggplot(df, aes(x=datt$Perimeter))+
  geom_histogram(color="darkblue", fill="lightblue")
ggplot(df, aes(x=datt$Perimeter))+
  geom_histogram(color="darkblue", fill="lightblue")
# Perimeter ne suit pas la loi normal


str(datt)
hist(datt$Area)
#"subset(datt, select = -c("Area_imp","MajorAxisLength_imp","MinorAxisLength_imp","Eccentricity_imp","Eccentricity_imp","ConvexArea_imp","Extent_imp","Perimeter_imp","Class_imp")
#date=subset(datt, select = -c("Area_imp","MajorAxisLength_imp","MinorAxisLength_imp","Eccentricity_imp","Eccentricity_imp","ConvexArea_imp","Extent_imp","Perimeter_imp","Class_imp")
#date "           
#datt = select(datt, -Area_imp, -MajorAxisLength_imp, -MinorAxisLength_imp)    

table(datt$Class)
tab <- table(datt$Class)
barplot(tab)
barplot(sort(tab), col="skyblue")
ggpubr::ggqqplot(as.numeric(datt$Class))

pie(tab)
pie(tab,main="Répartition des classes",col=c("blue","green","yellow","pink","orange","antiquewhite","cyan"))


x <-  table(datt$Class)
labels <-  c("Besni","Kecimen")


piepercent<- round(100*x/sum(x), 1)
# Plot the chart.
pie(x, labels = piepercent,col = rainbow(length(x)))
legend("topright", c("Besni","Kecimen"), cex = 0.8,
       fill = rainbow(length(x)))




##############################################TACHE 4

par(mfrow = c(2, 4))
for (i in 1:7) {
  boxplot(as.numeric(df[,i]) ~ df$Class,main = names(df)[i])
}
par(mfrow = c(1,1))

install.packages("reshape2")
install.packages("reshape") 
library(reshape2)
library(reshape)
library(ggplot2)
# creating correlation matrix
 
corr_mat <- round(cor(data.matrix(datt),method="spearman"),2)

# reduce the size of correlation matrix
melted_cor <- melt(corr_mat)

head(melted_cor)

# plotting the correlation heatmap
library(ggplot2)


ggplot(data = melted_cor, aes(x=X1, y=X2, fill=value)) + 
  geom_tile() +
  geom_text(aes(X2, X1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

install.packages("GGally")
library(GGally)
ggpairs(datt,                 # Data frame
        columns = 1:8,        # Columns
        aes(color = Class,  # Color by group (cat. variable)
            alpha = 0.5))     


#cor(datt, method = datt("pearson", "kendall", "spearman"))
mcor <- cor(datt$Area,datt$MajorAxisLength,method="spearman")
mcor

mcor <- cor(datt$Area,datt$MinorAxisLength,method="spearman")
mcor

mcor <- cor(datt$Area,datt$Eccentricity,method="spearman")
mcor

mcor <- cor(datt$Area,datt$ConvexArea,method="spearman")
mcor

mcor <- cor(datt$Area,datt$Extent,method="spearman")
mcor

cor <- cor(datt$Area,datt$Perimeter,method="spearman")
cor
plot(datt$Area,datt$Perimeter)

cor.test(datt$Area,datt$Perimeter,method="spearman")
cor.test(datt$Area,datt$Extent,method="spearman")
cor.test(datt$Area,datt$ConvexArea,method="spearman")
cor.test(datt$Area,datt$Eccentricity,method="spearman")
cor.test(datt$Area,datt$MinorAxisLength,method="spearman")
cor.test(datt$Area,as.numeric(datt$Class),method="spearman")

cor.test(datt$Perimeter,datt$Extent,method="spearman")
cor.test(datt$Perimeter,datt$ConvexArea,method="spearman")
cor.test(datt$Perimeter,datt$Eccentricity,method="spearman")
cor.test(datt$Perimeter,datt$MinorAxisLength,method="spearman")
cor.test(datt$Perimeter,as.numeric(datt$Class),method="spearman")

cor.test(datt$Extent,datt$ConvexArea,method="spearman")
cor.test(datt$Extent,datt$Eccentricity,method="spearman")
cor.test(datt$Extent,datt$MinorAxisLength,method="spearman")
cor.test(datt$Extent,as.numeric(datt$Class),method="spearman")


cor.test(datt$ConvexArea,datt$Eccentricity,method="spearman")
cor.test(datt$ConvexArea,datt$MinorAxisLength,method="spearman")
cor.test(datt$ConvexArea,as.numeric(datt$Class),method="spearman")


#interpretation
#on remarque entre que :

#aria /perimeter : Pvalue= 2,2 e-16 :  :: corr: 0,98

#aria/extent : pvalue=0,07 :: cor = 0,06

#aria/convexAria : Pvalue= 2,2 e-16 :: corr : 0,9982

#aria/eccentricity : Pvalue= 2,2 e-16 :: corr : 0,420

#aria/AxisLenght : Pvalue= 2,2 e-16  :: corr=0;902

#Perimeter/Extent : Pvalue= 0,6 :: corr=-0,017

#Peremeter/ConvexArea : Pvalue= 2,2 e-16 :: corr:0.98

#peremeter/eccentricity : Pvalue= 2,2 e-16 :: corr: 0,52

#peremeter/minoraxisLenght : Pvalue= 2,2 e-16 :: corr: 0,83

#Extent/ConvexArea : Pvalue=0,15 :: corr:0,04

#Extent/Eccentricity : Pvalue=1,024 e-15 :: corr:-0,27

#Extent/MinorAxisLenght : Pvalue=1,25 e-08 :: corr: 0,19

#convexarea/Eccentricity :  Pvalue=2,2 e-16 :: corr ; 0,427

#convexarea/Minoraixlenght: Pvalue=2,2 e-16 :: corr=0,898






#############################################TACHE 5

#On va  ́eliminer les variables les moins significatives qui sont les variables avec la valeur
#du pvalue la plus  ́elev ́ee, c’est le parm`etre qui est le plus susceptible d’ˆetre nul.


model=lm(datt$MinorAxisLength ~ . - Class,data =datt)
summary(model)

#Le parametre relatif a la variable "ConvexArea presente la valuer p* = 0.9409 la plus
#importante, alors la variable ConvexArea est de variabilite minimale. On  ́elimine la
#variable ConvexArea et on d ́efinit lenouveau modele :


model2=lm(datt$MinorAxisLength ~ . - Class-ConvexArea,data =datt)
summary(model2)



AIC(model)
AIC(model2)


BIC(model)
BIC(model2)


r=resid(model2)
qqnorm(r) 
qqline(r)

plot(r)
hist(r)
# Les residus ne suivent pas la loi normal

test = datt
test
test
test[ , 5] <- list(NULL) # on supprimer la colonne convexarea
test[ , 7] <- list(NULL) # on a supprimer la colonne class
test[ , 3] <- list(NULL) # on a supprimer la colonne minraxislength


test

datt


#PCA

test

install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

x1=PCA(test, scale.unit = TRUE, ncp = 3, graph = TRUE)


library("FactoMineR")
res.pca <- PCA(test, graph = FALSE)

print(res.pca)

eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


var <- get_pca_var(res.pca)
var

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969"  # Couleur des individues
)


summary(res.pca)

head(var$contrib, 4)

library("corrplot")
corrplot(var$contrib, is.corr=FALSE)


PCA(test, scale.unit = TRUE, ncp = 3, graph = TRUE)



model3=lm(MinorAxisLength ~ Extent+Eccentricity+Area,data=datt)
summary(model3)

model4=lm(MinorAxisLength ~ Extent+Eccentricity+Perimeter,data=datt)
summary(model4)

model5=lm(MinorAxisLength ~ Extent+Eccentricity+MajorAxisLength,data=datt)
summary(model5)

r=resid(model3)
qqnorm(r) 
qqline(r)
plot(r)
hist(r)


#donc on constate que apres l application de PCA le meilleur modele est celui qui contient " Extent+Eccentricity+Area"

# On remarque que les residus ne suivent pas la loi normal alors on procède à
# effectuer Generalized Linear model (GLM)

#####################TACHE 6

#Les GLM sont une extension des modèles linéaires 
#classiques qui peuvent être utilisés lorsque 
#les réponses ne sont pas de type numérique continues.

#Les GLM sont principalement utilisés dans deux situations :
  
#Lorsque les données sont de type comptage (nombre d’oeufs pondus, nombre de larve présentes etc..),
#Lorsque les données sont de type binaire (Malade/non malade ou mort/vivant)

#Comment ça marche les GLM ?
#Les modèles linéaires généralisés reposent sur 3 éléments:
  
#Un prédicteur linéaire
#Une fonction de lien
#Une structure des erreurs


"Lorsque les réponses sont de type catégoriel binaire
Lorsque les données sont catégorielles binaires, 
il est encore plus évident que les erreurs ne peuvent pas suivre une loi normale de moyenne nulle et 
de variance constante, 
puisque la réponse est “oui” ou “non”. 
Ce type de données suit une distribution Binomiale, 
de paramètres “n” et p”.
Comme nous le verrons plus loin,
dans ce cas, une transformation (de la réponse) 
va permettre de passer du “oui” / “non” à une probabilité d’être “oui”."

plot(datt$MinorAxisLength)


counts_glm1 <- glm(datt$MinorAxisLength ~ . - Class-ConvexArea, data=datt, family=gaussian(link="identity")) 
summary(counts_glm1)
#6268.2

counts_glm2 <- glm(datt$MinorAxisLength ~ . - Class-ConvexArea, data=datt, family=Gamma(link="inverse")) 
summary(counts_glm2)
#6913.8

counts_glm3 <- glm(datt$MinorAxisLength ~ . - Class-ConvexArea, data=datt, family=inverse.gaussian(link="1/mu^2")) 
summary(counts_glm3)
#7239.6

counts_glm4 <- glm(datt$MinorAxisLength ~ . - Class-ConvexArea, data=datt, family=poisson(link="log")) 
summary(counts_glm4)
#infinie

counts_glm5 <- glm(datt$MinorAxisLength ~ . - Class-ConvexArea, data=datt, family=quasi(link="identity",variance = "constant")) 
summary(counts_glm5)
#NA

counts_glm6 <- glm(datt$MinorAxisLength ~ . - Class-ConvexArea, data=datt, family=quasibinomial(link="logit")) 
summary(counts_glm6)
#....

counts_glm7 <- glm(datt$MinorAxisLength ~ . - Class-ConvexArea, data=datt, family=quasipoisson(link="log")) 
summary(counts_glm7)
#NA



# On va choisir le modele glm1 car il a une valeur AIC minimale.

# Merci pour votre attention n'hésitez pas à nous donner des questions.
















