#download packages

library(dplyr)
library(vegan)
library(ggplot2)
library(coin)
library(car)
library(emmeans)
library(multcompView)
library(jtools)
library(broom)
library(ggstance)
library(MASS)
library(ggpubr)
library(MuMIn)

theme_set(theme_classic())

forest <- read.csv('PJB_metadata_working.csv')

#seedling counts by treatment/habitat type

SumStats.seedlings <- forest %>%
  group_by(treatment) %>%
  summarize(n = n(),
            Mean = mean(seedling_per_ha),
            sd = sd(seedling_per_ha),
            se = sd/sqrt(n),
            sum = sum(seedling_per_ha))
SumStats.seedlings

SumStats.pseedlings <- forest %>%
  group_by(treatment) %>%
  summarize(n = n(),
            Mean = mean(pinyon_per_ha),
            sd = sd(pinyon_per_ha),
            se = sd/sqrt(n))
SumStats.pseedlings

SumStats.jseedlings <- forest %>%
  group_by(treatment) %>%
  summarize(n = n(),
            Mean = mean(juniper_per_ha),
            sd = sd(juniper_per_ha),
            se = sd/sqrt(n))
SumStats.jseedlings

SumStats.Gseedlings <- forest %>%
  group_by(age_treatment_revised) %>%
  summarize(n = n(),
            Mean = mean(G_per_ha),
            sd = sd(G_per_ha),
            se = sd/sqrt(n))
SumStats.Gseedlings

# Since only one burned plot contained seedlings run ANOVA on only the unburned and refugia 

#isolate refugia and unburned
forest.noburn <- forest %>%
  filter(treatment == "refugia" | treatment =="unburn")
forest.noburn

# Kruskal-Wallis test to compare seedling densities in the treatments/habitat types (age_treatment_revised groups old and recent unburned into sinngle category)
p.seedling = kruskal.test(pinyon_per_ha ~ age_treatment_revised, data=forest.noburn)
p.seedling
p.seedling = kruskal.test(pinyon_per_ha ~ age_treatment_revised, data=forest)
p.seedling

j.seedling = kruskal.test(juniper_per_ha ~ age_treatment_revised, data=forest.noburn)
j.seedling
j.seedling = kruskal.test(juniper_per_ha ~ age_treatment_revised, data=forest)
j.seedling

q.seedling = kruskal.test(G_per_ha ~ age_treatment_revised, data=forest)
q.seedling

# glm may be appropriate using a negative binomial regression

#all seedlings

#Check correlations, but first check for normality
ggqqplot(forest.noburn$BA_all_live)
ggqqplot(forest.noburn$Percent_Canopy_Cover)
ggqqplot(forest.noburn$trees_live)
ggqqplot(forest.noburn$percent_grass)
ggqqplot(forest.noburn$percent_bareground)
ggqqplot(forest.noburn$percent_shrub)
ggqqplot(forest.noburn$juniper_live)
ggqqplot(forest.noburn$pinyon_live)

# value of the Shapiro-Wilk Test is greater than 0.05, the data is normal. If it
# is below 0.05, the data significantly deviate from a normal distribution
shapiro.test(forest.noburn$BA_all_live) #normal
shapiro.test(forest.noburn$Percent_Canopy_Cover)#normal
shapiro.test(forest.noburn$trees_live) #normal
shapiro.test(forest.noburn$percent_grass) #not normal
shapiro.test(forest.noburn$percent_bareground) #normal
shapiro.test(forest.noburn$percent_shrub) #not normal
shapiro.test(forest.noburn$juniper_live) #normal
shapiro.test(forest.noburn$pinyon_live) #not normal
shapiro.test(forest.noburn$Elevation) #normal
shapiro.test(forest.noburn$Folded_aspect) #normal

#use pearson for normal, spearman for not normal
cor.test(forest.noburn$BA_all_live, forest.noburn$Percent_Canopy_Cover, method = "pearson")
cor.test(forest.noburn$BA_all_live, forest.noburn$trees_live, method = "pearson")
cor.test(forest.noburn$BA_all_live, forest.noburn$trees_live_per_ha, method = "pearson")
cor.test(forest.noburn$BA_all_live, forest.noburn$Elevation, method = "pearson")
cor.test(forest.noburn$Elevation, forest.noburn$trees_live, method = "pearson")
cor.test(forest.noburn$Percent_Canopy_Cover, forest.noburn$trees_live, method = "pearson")#p<0.05, so they are correlated
cor.test(forest.noburn$Percent_Canopy_Cover, forest.noburn$trees_live_per_ha, method = "pearson") #p<0.05, so they are correlated
cor.test(forest.noburn$percent_grass, forest.noburn$percent_bareground, method = "spearman")#p<0.05, so they are correlated
cor.test(forest.noburn$percent_grass, forest.noburn$percent_shrub, method = "spearman")
cor.test(forest.noburn$BA_all_live, forest.noburn$juniper_live, method = "pearson")
cor.test(forest.noburn$BA_live_juniper, forest.noburn$juniper_live, method = "pearson")
cor.test(forest.noburn$BA_live_pinyon, forest.noburn$trees_live, method = "pearson")
cor.test(forest.noburn$Percent_Canopy_Cover, forest.noburn$juniper_live, method = "pearson")
cor.test(forest.noburn$percent_shrub, forest.noburn$Elevation, method = "pearson")
cor.test(forest.noburn$BA_all_live, forest.noburn$pinyon_live, method = "spearman")
cor.test(forest.noburn$Percent_Canopy_Cover, forest.noburn$pinyon_live, method = "spearman")
cor.test(forest.noburn$trees_live, forest.noburn$pinyon_live, method = "spearman")
cor.test(forest$percent_shrub, forest$G_total, method = "pearson") # not correlated

## Large glm model with most important variables - use dropterm() to find best fitting model

forest.noburn$Folded_aspect <- as.numeric(forest.noburn$Folded_aspect)
class(forest.noburn$Folded_aspect)

forest$Folded_aspect <- as.numeric(forest$Folded_aspect)
class(forest$Folded_aspect)

#Possible variables:
  #BA_all_live, BA_live_j_plot, BA_live_p_plot, percent_grass, percent_shrub, trees_live, juniper_live, pinyon_live, Elevation, Folded_aspect

###### juniper seedlings
#refugia and unburned, full model
glm.seedlingj = glm.nb(juniper_total ~ Elevation + Folded_aspect + 
                         BA_live_plot + BA_live_j_plot + percent_grass + percent_shrub + 
                         trees_live + juniper_live + pinyon_live, data = forest.noburn)
summary(glm.seedlingj)

dropterm(glm.seedlingj, sorted = TRUE)

#all habitat types, full model
glm.seedlingj = glm.nb(juniper_total ~ Elevation + Folded_aspect + 
                         BA_live_plot + BA_live_j_plot + percent_grass + percent_shrub + 
                         trees_live + juniper_live + pinyon_live, data = forest)
summary(glm.seedlingj)

#Refugia and unburned, best model
glm.seedlingj = glm.nb(juniper_total ~ Elevation + 
                         BA_live_j_plot + percent_shrub + 
                         juniper_live, data = forest.noburn)
summary(glm.seedlingj)

# all habitat types, best model
glm.seedlingj = glm.nb(juniper_total ~ trees_live, data = forest)
summary(glm.seedlingj)

###### model for pinyon seedlings
#refugia and unburned, full model
glm.seedlingp = glm.nb(pinyon_total ~ Elevation + Folded_aspect + BA_live_plot + BA_live_j_plot + 
                         BA_live_p_plot + percent_grass + percent_shrub + trees_live + pinyon_live + 
                         juniper_live, data = forest.noburn)
summary(glm.seedlingp)

dropterm(glm.seedlingp, sorted = TRUE)

#all habitat types, full model
glm.seedlingp = glm.nb(pinyon_total ~ Elevation + Folded_aspect + BA_live_plot + BA_live_j_plot + 
                         BA_live_p_plot + percent_grass + percent_shrub + trees_live + pinyon_live + 
                         juniper_live, data = forest.noburn)
summary(glm.seedlingp)

dropterm(glm.seedlingp, sorted = TRUE)

#refugia and unburned, best model
glm.seedlingp = glm.nb(pinyon_total ~ Elevation + Folded_aspect + BA_live_plot + BA_live_j_plot + 
                         BA_live_p_plot + percent_grass + percent_shrub + trees_live + pinyon_live + 
                         juniper_live, data = forest)
summary(glm.seedlingp)

#all habitat types, best model
glm.seedlingp = glm.nb(pinyon_total ~ Elevation + trees_live, data = forest)
summary(glm.seedlingp)


# Gambel oak resprouts
glm.resproutg = glm.nb(G_total ~ Folded_aspect +
                         percent_grass + percent_shrub, data = forest)
summary(glm.resproutg)


######Null deviance: A low null deviance implies that the data can be modeled well 
#merely using the intercept. If the null deviance is low, you should consider
#using few features for modeling the data.

######Residual deviance: A low residual deviance implies that the model you have 
#trained is appropriate. Congratulations! For a well-fitting model, the residual 
# deviance should be close to the degrees of freedom

paste0(c("Null deviance: ", "Residual deviance: "),
       round(c(glm.seedlingp$null.deviance, deviance(glm.seedlingp)), 2))
##Null deviance: 148.19    Residual deviance: 49.03

summary(residuals(glm.seedlingp))
# Since the median deviance residual is close to zero (-0.1499), this means that our model 
#is not biased in one direction (i.e. the out come is neither over- nor underestimated).

######## Seedling plots

seedling <- read.csv('seedling_densities.csv')
head(seedling)

#Old burn = #E69F00"
#Recent burn = #F0E442"
# Old Refugia = "#56B4E9"
#Recent Refugia = "#CC79A7"
# Unburned = "#009E73"

labels <- c("seedling_per_ha" = "Conifer Seedlings",
            "juniper_per_ha" = 'Juniper Seedlings',
            'pinyon_per_ha' = 'Pinon Seedlings',
            'G_per_ha' = 'Gambel oak Stems')

p <- ggplot(seedling, aes(x = age_treatment_revised, y = pinyon_per_ha, fill = age_treatment_revised)) 
p <- p + geom_boxplot()
p <- p + labs(title = "Pinyon Seedlings",
              x = "Habitat",
              y = "Seedlings / ha")
p <- p + scale_fill_manual(values = c("#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#009E73"))
p <- p + theme(axis.text.x = element_text(size=14, angle = 45, vjust = 1, hjust=1))
p <- p + theme(axis.title.x = element_text(size=22),
               axis.title.y = element_text(size=22),
               axis.text.x = element_text(size=18),
               axis.text.y = element_text(size=18),
               plot.title=element_text(size = 20),
               legend.text = element_text(size=18),
               legend.title = element_text(size = 22))
p <- p + ylim(0,1200)
p

p <- ggplot(seedling, aes(x = age_treatment_revised, y = juniper_per_ha, fill = age_treatment_revised)) 
p <- p + geom_boxplot()
p <- p + labs(title = "Juniper Seedlings",
              x = "Habitat",
              y = "Seedlings / ha")
p <- p + scale_fill_manual(values = c("#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#009E73"))
p <- p + theme(axis.text.x = element_text(size=14, angle = 45, vjust = 1, hjust=1))
p <- p + theme(axis.title.x = element_text(size=22),
               axis.title.y = element_text(size=22),
               axis.text.x = element_text(size=18),
               axis.text.y = element_text(size=18),
               plot.title=element_text(size = 20),
               legend.text = element_text(size=18),
               legend.title = element_text(size = 22))
p <- p + ylim(0,1200)
p

p <- ggplot(seedling, aes(x = age_treatment_revised, y = G_per_ha, fill = age_treatment_revised)) 
p <- p + geom_boxplot()
p <- p + labs(title = "Gambel oak Seedlings",
              x = "Habitat",
              y = "Seedlings / ha")
p <- p + scale_fill_manual(values = c("#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#009E73"))
p <- p + theme(axis.text.x = element_text(size=14, angle = 45, vjust = 1, hjust=1))
p <- p + theme(axis.title.x = element_text(size=22),
               axis.title.y = element_text(size=22),
               axis.text.x = element_text(size=18),
               axis.text.y = element_text(size=18),
               plot.title=element_text(size = 20),
               legend.text = element_text(size=18),
               legend.title = element_text(size = 22))
p


######### Differences between groups for overstory, understory, and substrate
## levene test: equal variances (p > $\alpha$)
## SW test: normality (p > $\alpha$) 
colnames(forest)
leveneTest(percent_litter ~ age_treatment, data = forest) #equal variances
leveneTest(percent_bareground ~ age_treatment, data = forest) #equal variances
leveneTest(percent_grass ~ age_treatment, data = forest) #equal variances
leveneTest(percent_shrub ~ age_treatment, data = forest) #equal variances
leveneTest(percent_forb ~ age_treatment, data = forest) #equal variances
leveneTest(Percent_Canopy_Cover ~ age_treatment, data = forest) #equal variances
leveneTest(trees_live ~ age_treatment, data = forest) #equal variances
leveneTest(BA_all_live ~ age_treatment, data = forest) # not equal variances

# Tukey HSD

SumStats.bg <- forest %>%
  group_by(age_treatment_revised) %>%
  summarize(n = n(),
            Mean = mean(percent_bareground),
            sd = sd(percent_bareground),
            se = sd/sqrt(n))
SumStats.bg

OneWayWLFit.bareground <- aov(percent_bareground ~ age_treatment_revised, data = forest)
summary(OneWayWLFit.bareground)
shapiro.test(residuals(OneWayWLFit.bareground)) #not normal

OneWayWLFit.bareground <- lm(percent_bareground ~ age_treatment_revised, data = forest)
emout.bareground <- emmeans(OneWayWLFit.bareground, ~age_treatment_revised)
pairs(emout.bareground)
multcomp::cld(emout.bareground)

###

SumStats.litter <- forest %>%
  group_by(age_treatment_revised) %>%
  summarize(n = n(),
            Mean = mean(percent_litter),
            sd = sd(percent_litter),
            se = sd/sqrt(n))
SumStats.litter

forest

OneWayWLFit.litter <- aov(percent_litter ~ age_treatment_revised, data = forest)
summary(OneWayWLFit.litter)
shapiro.test(residuals(OneWayWLFit.litter)) #not normal

OneWayWLFit.litter <- lm(percent_litter ~ age_treatment_revised, data = forest)
emout.litter <- emmeans(OneWayWLFit.litter, ~age_treatment_revised)
pairs(emout.litter)
multcomp::cld(emout.litter)

###

SumStats.grass <- forest %>%
  group_by(age_treatment_revised) %>%
  summarize(n = n(),
            Mean = mean(percent_grass),
            sd = sd(percent_grass),
            se = sd/sqrt(n))
SumStats.grass

OneWayWLFit.grass <- aov(percent_grass ~ age_treatment_revised, data = forest)
summary(OneWayWLFit.grass)
shapiro.test(residuals(OneWayWLFit.grass)) #normal

OneWayWLFit.grass <- lm(percent_grass ~ age_treatment_revised, data = forest)
emout.grass <- emmeans(OneWayWLFit.grass, ~age_treatment_revised)
pairs(emout.grass)
multcomp::cld(emout.grass)

###

SumStats.shrub <- forest %>%
  group_by(age_treatment_revised) %>%
  summarize(n = n(),
            Mean = mean(percent_shrub),
            sd = sd(percent_shrub),
            se = sd/sqrt(n))
SumStats.shrub

OneWayWLFit.shrub <- aov(percent_shrub ~ age_treatment_revised, data = forest)
summary(OneWayWLFit.shrub)
shapiro.test(residuals(OneWayWLFit.shrub)) #normal

OneWayWLFit.shrub <- lm(percent_shrub ~ age_treatment_revised, data = forest)
emout.shrub <- emmeans(OneWayWLFit.shrub, ~age_treatment_revised)
pairs(emout.shrub)
multcomp::cld(emout.shrub)

###

SumStats.forb <- forest %>%
  group_by(age_treatment_revised) %>%
  summarize(n = n(),
            Mean = mean(percent_forb),
            sd = sd(percent_forb),
            se = sd/sqrt(n))
SumStats.forb

OneWayWLFit.forb <- aov(percent_forb ~ age_treatment_revised, data = forest)
summary(OneWayWLFit.forb)
shapiro.test(residuals(OneWayWLFit.forb)) #normal 

OneWayWLFit.forb <- lm(percent_forb ~ age_treatment_revised, data = forest)
emout.forb <- emmeans(OneWayWLFit.forb, ~age_treatment_revised)
pairs(emout.forb)
multcomp::cld(emout.forb)

###

SumStats.CanCov <- forest %>%
  group_by(age_treatment_revised) %>%
  summarize(n = n(),
            Mean = mean(Percent_Canopy_Cover),
            sd = sd(Percent_Canopy_Cover),
            se = sd/sqrt(n))
SumStats.CanCov

OneWayWLFit.CanCov <- aov(Percent_Canopy_Cover ~ age_treatment_revised, data = forest)
summary(OneWayWLFit.CanCov)
shapiro.test(residuals(OneWayWLFit.CanCov)) #normal

OneWayWLFit.CanCov <- lm(Percent_Canopy_Cover ~ age_treatment_revised, data = forest)
summary(OneWayWLFit.CanCov)
emout.CanCov <- emmeans(OneWayWLFit.CanCov, ~age_treatment_revised)
pairs(emout.CanCov)
multcomp::cld(emout.CanCov)

###

SumStats.BA <- forest %>%
  group_by(age_treatment_revised) %>%
  summarize(n = n(),
            Mean = mean(BA_all_live_ha),
            sd = sd(BA_all_live_ha),
            se = sd/sqrt(n))
SumStats.BA


OneWayWLFit.BA <- aov(BA_all_live_ha ~ age_treatment, data = forest)
summary(OneWayWLFit.BA)
shapiro.test(residuals(OneWayWLFit.BA)) #not normal

OneWayWLFit.BA <- lm(BA_all_live_ha ~ age_treatment_revised, data = forest)
emout.BA <- emmeans(OneWayWLFit.BA, ~age_treatment_revised)
pairs(emout.BA)
multcomp::cld(emout.BA)

###

SumStats.livetrees <- forest %>%
  group_by(age_treatment_revised) %>%
  summarize(n = n(),
            Mean = mean(trees_live_per_ha),
            sd = sd(trees_live_per_ha),
            se = sd/sqrt(n))
SumStats.livetrees

OneWayWLFit.livetrees <- aov(trees_live_per_ha ~ age_treatment_revised, data = forest)
summary(OneWayWLFit.livetrees)
shapiro.test(residuals(OneWayWLFit.livetrees)) #not normal

OneWayWLFit.livetrees <- lm(trees_live_per_ha ~ age_treatment_revised, data = forest)
emout.livetrees <- emmeans(OneWayWLFit.livetrees, ~age_treatment_revised)
pairs(emout.livetrees)
multcomp::cld(emout.livetrees)