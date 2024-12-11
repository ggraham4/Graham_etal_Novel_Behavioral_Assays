#PCA by sum behaviors

#Libraries
{
library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
  library(ggsignif)
  library(sjPlot)
  library(ggpubr)
  library(lmerTest)
  library(MuMIn)
  library(tidyr)
library(factoextra)
library(ggpubr)
  library(forcats)
  library(factoextra)
  pca_distance_2 <- readRDS("R/pca_distance_2.rds")

}
#Load data
{
data <- read_excel("Assays Data/Assays Data.xlsx", na = "NA")
data$Latency <- ifelse(data$Latency == "Nan", data$Latency == data$Time.For.Assay.minutes*60, data$Latency)
data <- subset(data,Sex != "G" & (Rater == "GG"| Rater == "EP" | Rater == "IW")&  #Exclude unrated videos
                 Tank != "T12"& 
                 Tank != "B11" &
                 Tank != "B21"& 
                 Tank != "T3") #and incomplete tanks
data$Sum.Behaviors <- as.numeric(data$Sum.Behaviors)
data$Aggressive.per.minute <- as.numeric(data$Aggressive.per.minute)
data$Round <- as.factor(data$Round)
data$Latency <- as.numeric(data$Latency)
data$Latency <- ifelse(is.na(data$Latency), data$Time.For.Assay.minutes*60, data$Latency)
}

females <- unique(data$Fish[data$Sex=='F'])
males <- unique(data$Fish[data$Sex=='M'])

female_renamer <- function(data, list) { #Rename females
  data$arbitrary_id <- data$Fish 
  i <- 1
  for (j in list) {
    new_female_id <- paste0('F', i)
    data$arbitrary_id[data$Fish == j] <- new_female_id
    i <- i + 1
  }
  return(data)
}


male_renamer <- function(data, list) { #rename males
  i <- 1
  for (j in list) {
    new_male_id <- paste0('M', i)
    data$arbitrary_id[data$Fish == j] <- new_male_id
    i <- i + 1
  }
  return(data)
}
data <- female_renamer(data, females)
data <- male_renamer(data, males)

#Modify Data
{
filtered <- data%>%
  mutate(Assay = ifelse(Assay == "Crab","CrabLeashed", Assay))%>% #The two assays are not significantly different and I'm only doing this for the PCA
  dplyr::select(Tank, 
         Fish,
         arbitrary_id,
         Sex, 
         Eggs, 
         Round, 
         Assay, 
         Nips,
         Tail.Fans, 
         Pec.fans, 
         Entrances, 
         Time, 
         Latency, 
         Flee, 
         Tail.strike, 
         Bite, 
         Display, 
         Quiver, 
         Chase.Charge, 
         Sum.Behaviors)

  
  mean_all <- filtered %>%
  group_by(Tank, Fish, Sex, Assay,arbitrary_id) %>%
  summarize(Mean_Sum_Behaviors = mean(Sum.Behaviors, na.rm = TRUE), .groups = 'drop')

  mean_all$Assay <-  fct_relevel(mean_all$Assay, "Parenting", "CrabLeashed", "Damsel", "Male", "Female", "Hand", "Rock")

  
combined_data5 <- mean_all %>%
  pivot_wider(names_from = c(Assay), 
              values_from = Mean_Sum_Behaviors,
              names_sep = "_",
              values_fill = list(Mean_Sum_Behaviors = 0))%>%
  relocate(Parenting, .before = CrabLeashed)%>%
  relocate(Male, .before = Female)%>%
  rename('Parental Care' = Parenting,
         'Small Intruder' = CrabLeashed,
         'Large Intruder' = Damsel, 
         "Male-Oriented"= Male, 
         "Female-Oriented" = Female,
         'Immediate Reaction' = Hand, 
         'Nest Maintenance' = Rock)

}

#Split by sex
{
comb.male <- subset(combined_data5, Sex == "M")
comb.female <- subset(combined_data5, Sex == "F")
mal <- comb.male[,5:11]
fem <- comb.female[,5:11]

tog <- combined_data5[,5:11]

#PCAs
pca_m <- princomp(mal, cor = TRUE)
pca_f <- princomp(fem,cor = TRUE)

pca_t <-  princomp(tog,cor = TRUE)

pca_damsel_parenting <- princomp(tog[,c(1,3)], cor = TRUE)
pca_distance_2(pca_damsel_parenting, combined_data5,"Sex")
fviz_pca_ind(pca_damsel_parenting, habillage = combined_data5$Sex)

pca_distance_2(pca_t, combined_data5,"Sex")

fviz_pca_ind(pca_t)

#Component correlations
pca_f$loadings
pca_m$loadings

mat <- matrix(NA, 18, 4)
mat[,1] <- pca_t$scores[,1]
mat[,2] <- combined_data5$Sex
mat[,3] <- ifelse(mat[,2] == 'M', 0, 1)

mat.df <- as.data.frame(mat)
mat.df$PC1 <- as.numeric(mat.df$V1)
mat.df$Sex <- mat.df$V2
mat.df$Sex.class <- as.numeric(mat.df$V3)


model <- glm(Sex.class ~PC1,
             data = mat.df, 
             family = binomial('logit')
)

summary(model)


library(ggrepel)

#Plots
male.pca <- fviz_pca_ind(pca_m, label = "none")+
  theme_classic()+
    geom_point(color = "blue")+
  geom_text_repel(label = comb.male$Fish)+
  labs(x = "PC_1" , y = "PC_2", title = "Males by Mean of Sum Behaviors")
male.pca


female.pca <- fviz_pca_ind(pca_f, label = "none")+
  theme_classic()+
  geom_point(color = "red")+
  geom_text_repel(label = comb.female$Fish, hjust = 1.05)+
  labs(x = "PC_1" , y = "PC_2", title = "Females by Mean of Sum Behaviors")
female.pca

tog.pca <-  fviz_pca_ind(pca_t, habillage = combined_data5$Sex, label = '', pointsize = 1.5, linetype = 3)+
  theme_classic()+
  geom_text_repel(label = combined_data5$arbitrary_id, aes(color = combined_data5$Sex), size =2.5, color = 'black', hjust = 0.2)+
  labs(x = "PC_1" , y = "PC_2", title = "")+
  theme_classic()+    
  theme(text = element_text(size = 7.5),         
        legend.title=element_text(size = 7.5),        
        legend.text = element_text(size = 7.5),         
        axis.text =element_text(size = 7.5))
tog.pca

tog.pca.var <-  fviz_pca_var(pca_t, label = '', alpha = 0.3)+
  labs(x = "PC_1" , y = "PC_2", title = "")+
    geom_text(label = rownames(pca_t$loadings),size = 2.5, hjust = 0.25)+
  theme_classic()+    
  theme(text = element_text(size = 7.5),         
        legend.title=element_text(size = 7.5),        
        legend.text = element_text(size = 7.5),         
        axis.text =element_text(size = 7.5))

tog.pca.var


fig.4.1 <- ggarrange(tog.pca, labels = 'A', legend = 'bottom')

fig.4.2 <- ggarrange(tog.pca.var, labels = 'B')



fig4 <- ggarrange(tog.pca, tog.pca.var,
                     ncol= 2,
                          labels = c("A", 'B'), 
                          common.legend = TRUE, 
                          legend = 'bottom')
fig4

ggsave(plot = fig4,
       file = "Fig4 .jpg",
       device = "jpg",
       units = "in",
       width = 6.5,
       height = 3.25,
       path = "Plots/Fig 4",
       limitsize = FALSE
)







