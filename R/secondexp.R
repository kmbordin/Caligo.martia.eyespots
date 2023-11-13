#THE LARGE AND CENTRAL Caligo martia HIND WING EYESPOT MAY REDUCE FATAL ATTACKS BY BIRDS: A CASE STUDY SUPPORTS THE DEFLECTION HYPOTHESIS IN NATURE, by Iserhard et al. (2023)

# loading packages ----
library(ggplot2) # figures
library(visreg) # visualize regressions
library(lme4) # glmm
library(dplyr) # organise dataset
library(reshape2) # organise dataset
library(gridExtra) # save figures
library(gridtext) # save figures
library(MuMIn) # explained variance

# loading data -----
predation2 <- read.table(here::here("processed data", "eyespots_plot.txt"), h=T)

predation2$block <- as.factor(predation2$block)
predation2$factor <- as.factor(predation2$factor)

str(predation2)

# data analysis ----
#mixed model, binomial distribution, block as random effect
new.df <- filter(predation2, factor !="N")
colSums(new.df[,3:6])
dcast(new.df,factor~CM)

m1 <- glmer(NC ~ factor + (1|block), family=binomial(link = "logit"),data=new.df); summary(m1);r.squaredGLMM(m1)
m2 <- glmer(CM ~ factor + (1|block), family=binomial(link = "logit"),data=new.df); summary(m2);r.squaredGLMM(m2)
m3 <- glmer(WE ~ factor + (1|block), family=binomial(link = "logit"),data=new.df); summary(m3);r.squaredGLMM(m3)
m4 <- glmer(UV ~ factor + (1|block), family=binomial(link = "logit"),data=new.df); summary(m4);r.squaredGLMM(m4)


# plots ----
my_theme <- theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(text = element_text(size=14))+ 
  theme(legend.key=element_blank())+ 
  theme(legend.background=element_blank())

# model NC
m1.p <- data.frame(fact= new.df$factor, predicted=(predict(m1,type="response"))*100)
v1 <- ggplot(m1.p, aes(x=fact, y=predicted))+
  geom_jitter(colour="gray80", size=0.2)+
  stat_summary(size=0.2)+
  my_theme+
  xlab("Model NC")+
  ylab("")+
  annotate("text", x = 1, y = 35, label = "a",
           colour = "black", size=3)+
  annotate("text", x = 2, y = 35, label = "b",
           colour = "black", size=3)

# model CM
m2.p <- data.frame(fact= new.df$factor, predicted=(predict(m2,type="response"))*100)
v2 <- ggplot(m2.p, aes(x=fact, y=predicted))+
  geom_jitter(colour="gray80", size=0.2)+
  stat_summary(size=0.2)+
  my_theme+
  xlab("Model CM")+
  ylab("")

# model WE
m3.p <- data.frame(fact= new.df$factor, predicted=(predict(m3,type="response"))*100)
v3 <- ggplot(m3.p, aes(x=fact, y=predicted))+
  geom_jitter(colour="gray80", size=0.2)+
  stat_summary(size=0.2)+
  my_theme+
  xlab("Model WE")+
  ylab("") +
  annotate("text", x = 1, y = 35, label = "a",
           colour = "black", size=3)+
  annotate("text", x = 2, y = 35, label = "b",
           colour = "black", size=3)

# model UV
m4.p <- data.frame(fact= new.df$factor, predicted=(predict(m4,type="response"))*100)
v4 <- ggplot(m4.p, aes(x=fact, y=predicted))+
  geom_jitter(colour="gray80", size=0.2)+
  stat_summary(size=0.2)+
  my_theme+
  xlab("Model UV")+
  ylab("")

yleft <- richtext_grob("Predation probability (%)", rot=+90)
# 
# tiff("plots_2nd_exp.tif", width = 5, height = 4, units = 'in', res = 300)
# grid.arrange(v3,v4,v2,v1, left=yleft)
# dev.off()

# trends in predation probability ----------
predation <- read.table(here::here("processed data", "experiment_2.txt"), h=T)
predation <- filter(predation, body_part !="N")
predation <- dcast(predation,body_part+model~body_part)
predation$count <- predation[,3]+predation[,4]
predation
predation <- predation %>% 
  mutate(model = replace(model, model == "CO", "WE")) %>%
  mutate(model = replace(model, model == "C", "NC")) %>% 
  mutate(model = replace(model, model == "SO", "CM")) %>%
  mutate(model = replace(model, model == "UV", "UV"))

predation$model <- factor(predation$model, levels = c("WE","UV","CM","NC"))


p1 <- ggplot(predation, aes(x = model, y = count, fill=body_part, color=body_part, shape = body_part)) +
  ylab("Number of predations in wings      and body   ") + xlab("Models")+
  geom_point(size=5) + my_theme+
  scale_shape_manual(values = c(23, 21))+
  scale_fill_manual(values=c("#56B4E9","#E69F00"))+
  scale_color_manual(values=c( "#56B4E9","#E69F00"))+
  scale_y_continuous(breaks = seq(1, 13,2))+
  theme(legend.position = c(0.92, 0.92))+
  theme(legend.title = element_blank())
 p1 
 
 # tiff("plots_2nd_exp2.tif", width = 5, height = 4, units = 'in', res = 300)
 # p1
 # dev.off()
