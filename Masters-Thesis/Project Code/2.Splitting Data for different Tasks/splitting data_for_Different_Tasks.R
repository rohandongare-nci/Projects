#create seperate datasets
library(caTools)
library(minDiff)
devtools::install_github("m-Py/minDiff")
library(minDiff)
groups_sdss<-read.csv("C:/Users/rohan/OneDrive/Desktop/SDSS/sdss_data/x18120199_research_project_final.csv")
#boxplot to check for outliers
library(cowplot)
theme1<- theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                          legend.position="top")

theme2<- theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                          legend.position="none")
plot_grid(
ggplot(groups_sdss, aes(x=class, y=g, fill=class)) + geom_boxplot()+theme1,
ggplot(groups_sdss, aes(x=class, y=u, fill=class)) + geom_boxplot()+theme1,
ggplot(groups_sdss, aes(x=class, y=r, fill=class)) + geom_boxplot()+theme1,
ggplot(groups_sdss, aes(x=class, y=i, fill=class)) + geom_boxplot()+theme2,
ggplot(groups_sdss, aes(x=class, y=z, fill=class)) + geom_boxplot()+theme2
#ggplot(groups_sdss, aes(x=class, y=redshift, fill=class)) + geom_boxplot()+theme2
)
#summary of the dataframe suggests u,g,r,i and z have an unusual value -9999
summary(groups_sdss)
#test which groups have those values
library(plyr)
length(groups_sdss[which(groups_sdss$i < 0),])
f<-groups_sdss[which(groups_sdss$z < 0),]
summary(test)
#eliminating -9999 value (146 observations have -9999 value for u,g,r,i and z)
groups_sdss<-groups_sdss[!(groups_sdss$i=="-9999" | groups_sdss$g=='-9999'| groups_sdss$u=='-9999' | groups_sdss$r == '-9999' | groups_sdss$z == '-9999'),]
summary(groups_sdss)
summary(groups_sdss)
numeric_group<-dplyr::select_if(groups_sdss, is.numeric)
sd(numeric_group$u)
#Split data for training models and feature selection+optimization
set.seed(18120199) 
sample <- sample.split(groups_sdss$class, SplitRatio = .50)
train_models <- subset(groups_sdss, sample == TRUE)
selection_optimization  <- subset(groups_sdss, sample == FALSE)
summary(train_models)
summary(selection_optimization)
#split training data for training of base learners and optimization of the meta learner
sample_1<-sample.split(train_models$class, SplitRatio = .50)
optimize_meta_learner<-subset(train_models,sample_1 == TRUE)
train_base<-subset(train_models, sample_1 == FALSE)
#converting both to csv files
write.csv(optimize_meta_learner, file = "C:/Users/rohan/OneDrive/Desktop/SDSS/sdss_data/Optimize Meta Learner.csv",row.names=FALSE)
write.csv(train_base, file = "C:/Users/rohan/OneDrive/Desktop/SDSS/sdss_data/Train entire stack.csv",row.names=FALSE)
#split selection_optimization into feature selection and optimization of base learners
sample_2<-sample.split(selection_optimization$class, SplitRatio = .50)
feature_selection<-subset(selection_optimization, sample_2 == TRUE)
base_learner_optimization<- subset(selection_optimization, sample_2 == FALSE)
#converting to csv files
write.csv(feature_selection, file = "C:/Users/rohan/OneDrive/Desktop/SDSS/sdss_data/Feature Selection.csv",row.names=FALSE)
write.csv(base_learner_optimization, file = "C:/Users/rohan/OneDrive/Desktop/SDSS/sdss_data/Base Learners Optimization.csv",row.names=FALSE)
