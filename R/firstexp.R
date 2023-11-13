#THE LARGE AND CENTRAL Caligo martia HIND WING EYESPOT MAY REDUCE FATAL ATTACKS BY BIRDS: A CASE STUDY SUPPORTS THE DEFLECTION HYPOTHESIS IN NATURE, by Iserhard et al. (2023)

# loading packages ----
library(lme4) # glmm
library(ggplot2) # figures
library(MuMIn) # explained variance
library(visreg) # visualize regression
library(here) # load data
library(reshape2) # organise data
library(doBy) # summarise data
library(dplyr) # data filtering

# loading data -----
predation <- read.csv2(here::here("processed data", "experiment_1.csv"), h=T)

predation$block <- as.factor(predation$block)
predation$model <- as.factor(predation$model)

str(predation)

summarise <- summary_by(beak ~ block+model, data=predation, FUN=sum, keep.names = TRUE)
filter(summarise, beak >0)

# data analysis ----
# mixed model, binomial distribution, block as random effect
predation$model <- factor(predation$model, levels = c("WE","UV","CM","NC"))

m1 <- glmer(beak~model + (1|block), family=binomial(link = "cloglog"), data=predation) 
summary(m1)
visreg(m1)
predation$pred <- (predict(m1, type = "response")*100) #probability values as percentages
r.squaredGLMM(m1)
summ <- dcast(predation, model~beak, fun.aggregate = length); summ

# boxplots ----
my_theme <- theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(text = element_text(size=14))+ 
  theme(legend.key=element_blank())+ 
  theme(legend.background=element_blank())

plot_m1 <- ggplot(predation, aes(x = model, y = pred, fill=model,alpha = 0.95)) +
  ylab("Predation probability (%)") + 
  xlab("Models")+ 
  my_theme+
  #geom_jitter(alpha=0.1)+
  scale_fill_manual(values=c("#FF9933","#FFFF00", "#330000","gray90"))+
  geom_boxplot(data = predation, aes(ymin= min(pred), ymax=max(pred)), size=1) +
  theme(legend.title = element_blank())+  theme(legend.position = "none") 

plot_m1


# tiff("graphglmm.tif", width = 4, height = 3.2, units = 'in', res = 300)
# plot_m1
# dev.off()
