library('haven')
library('ggplot2')
library('tidyverse')
library('dplyr')
library('gtsummary')
library('corrplot')
library('Hmisc')

load("~/Documents/coping/coping_pcl.RData") 

db <- read_sav('~/Documents/coping/Data.sav')

db['ReexpInternal'] <- db$PM_PCL_1+db$PM_PCL_2+db$PM_PCL_3
db['ReexpExternal'] <- db$PM_PCL_4+db$PM_PCL_5

keep <- c('Age', 	'Male_Gender', 	'CollegeGraduate_or_higher', 	'Married_Partnered', 	'Income_60k_plus', 	'ACES_TOTAL', 'TOTAL_TRAUMAS',
          'MDD_SYMPTOMS_CURRENT_', 	'GAD_SYMPTOMS_CURRENT_', 	'Current_Psych_Medication', 	'Current_Psychotherapy', 	
          'EXTRAVERSION', 	'AGREEABLENESS', 	'EMOTIONAL_STABILITY', 	'CONSCIENTIOUSNESS', 	'OPENNESS_TO_EXPERIENCES',  	
          'ReexpInternal', 'ReexpExternal', 	'SUM_PM_AVOID', 	'SUM_PM_NEGAFFECT', 	'SUM_PM_ANHEDONIA', 	'SUM_PM_EXTBEHAVS', 	'SUM_PM_ANXAROUS', 	'SUM_PM_DYSAROUS', 	
          'Avoidant_Coping_3cat')


newnames <- c('Age', 	'Sex', 	'CollegeGraduate', 	'Partner', 	'High_Income', 	'ACES', 'Traumas', 
              'MDD_Symptoms', 'GAD_Symptoms', 'Psych_Medication', 'Psychotherapy', 	
              'Extraversion', 	'Agreeableness', 	'Emotional_stability', 	'Conscientiousness', 	'Openness',	
              'Reexp-Internal', 'Reexp-External', 	'Avoidance', 	'NegativeAffect', 	'Anhedonia', 	'ExtBehavs', 	'AnxArousal', 	'DysArousal', 	
              'Avoidant_Coping')

df <- subset(db, select = keep)
colnames(df) <- newnames
# Number of participants

dim(df)[1]
# [1] 4069

# remove people with no coping data
df1 <- na.omit(df)
dim(df1)[1]
#[1] 3121

pcl <- df1$`Reexp-Internal`+df1$`Reexp-External`+df1$Avoidance+df1$NegativeAffect+df1$Anhedonia+df1$ExtBehavs+df1$AnxArousal+df1$DysArousal

summary(data.frame(df1))


df1_1 <- df1 %>% select(Age, Sex, High_Income, Partner, Avoidant_Coping)
df1_1 %>% tbl_summary(by = Avoidant_Coping) %>% add_p()


# Characteristic	0, N = 2,3731	1, N = 5781	2, N = 1661	p-value2
# Age	            70 (60, 75)	  64 (52, 72)	61 (51, 71)	<0.001
# Sex	            2,084 (88%)	  469 (81%)	  135 (81%)	<0.001
# High_Income	    1,399 (59%)	  320 (55%)	  83 (50%)	0.033
# Partner	        1,690 (71%)	  398 (69%)	  99 (60%)	0.005
# 1 Median (IQR); n (%)
# 2 Kruskal-Wallis rank sum test; Pearson's Chi-squared test

corr <- rcorr(as.matrix(df1))
corrplot(corr[["r"]], method = "square", type="lower", diag = FALSE)

sapply(df1, class)

########################
### Network analysis ###
########################

library('mgm')
library('qgraph')

set.seed(2015)

types <-  c('g', 'c', 'c', 'c', 'c', 'g', 'g', 'g', 'g', 'c', 'c', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'p')
levels <- c(1,     2,   2,   2,   2,   1,   1,   1,   1,   2,   2,   1,   1,   1,   1,    1,   1,  1,  1,   1,   1,   1,   1,   1,  1)
groups <- c('demographic', 'demographic', 'demographic', 'demographic', 'demographic', 'demographic', 'demographic', 
            'Medical', 'Medical', 'Medical', 'Medical', 
            'Personality', 'Personality', 'Personality', 'Personality', 'Personality', #'Personality', 'Personality', 'Personality', 
            'PCL', 'PCL', 'PCL', 'PCL', 'PCL', 'PCL', 'PCL', 'PCL', 
            'Coping')

mgm_obj <- mgm(data = df1,
               type = types,
               level = levels,
               lambdaSel = "CV",
               lambdaFolds = 10,
               k = 2,
               pbar = FALSE,
               scale = TRUE)

qgraph(input = mgm_obj$pairwise$wadj,  # now weighted adjacency matrix
       layout = "circle",
       nodeNames = colnames(df1), 
       groups = groups,
       palette = "colorblind",
       threshold = 0.1,
       edge.color = mgm_obj$pairwise$edgecolor) # incorporate sign as color


###########################
### Relative importance ###
###########################

library('relaimpo')

model_coping <- lm(Avoidant_Coping ~ Age + 
                     ACES + Traumas +
                     MDD_Symptoms + GAD_Symptoms + Psych_Medication + Psychotherapy +
                     Extraversion + Agreeableness + Emotional_stability + Conscientiousness + Openness + 
                     `Reexp-External` + `Reexp-Internal` + Avoidance + NegativeAffect + Anhedonia + ExtBehavs + AnxArousal + DysArousal, data = df1)


summary(model_coping)

boot <- boot.relimp(model_coping, type = 'lmg', b = 1000)
booteval <- booteval.relimp(boot)
plot(booteval)

label = c('Age', 
          'ACES', 'Trauma',
          'MDD symptoms #', 'GAD symptoms #', 'Current psych-medication', 'Current psychotherapy', 	
          'Extraversion', 	'Agreeableness', 	'Emotional stability', 	'Conscientiousness', 	'Openness to experience',  	
          'Re-experiencing External','Re-experiencing Internal', 	'Avoidance', 	'Negative affect', 	'Anhedonia', 	'Externalizing behaviors', 	'Anxious arousal', 	'Dysphoric arousal')

lgmValues <- booteval@lmg # mean of each variable
lgmLow <- booteval@lmg.lower[1,] # create as vector instead of 1D matrix
lgmHigh <- booteval@lmg.upper[1,]
lmgData <- tibble(lgmValues, lgmLow, lgmHigh, label)
# sort data by value
lmgOrdered <- lmgData[order(lgmValues, decreasing = F),]
# set label as factor with ordered levels
lmgOrdered$label <- factor(lmgOrdered$label, levels = lmgOrdered$label)
#plot
pdf('Documents/coping/Figure_relimp.pdf')
ggplot(lmgOrdered, aes(x=label, y=(lgmValues*100))) +  geom_bar(stat="identity", fill='lightblue',
                                                                 position=position_dodge()) +  geom_errorbar(aes(ymin=(lgmLow*100), ymax=(lgmHigh*100)), width=.2,
                                                                                                             position=position_dodge(.9)) + coord_flip() + ylab("Values [LMG in %]") + xlab('PTSD Symptom')  +theme_minimal()
dev.off()

##############################################################
### Negative affect seems to carry most of the weight.     ###
### But which one of the PCL items has the highest impact? ###
##############################################################

keep_pcl <- c('Age', 'Male_Gender', 'CollegeGraduate_or_higher', 'Married_Partnered', 'Income_60k_plus', 	
              'ACES_TOTAL', 'TOTAL_TRAUMAS',
              'ReexpInternal', 'ReexpExternal', 'SUM_PM_AVOID', 'SUM_PM_ANHEDONIA', 'SUM_PM_EXTBEHAVS', 'SUM_PM_ANXAROUS', 	'SUM_PM_DYSAROUS',
              'PM_PCL_8', 'PM_PCL_9', 'PM_PCL_10', 'PM_PCL_11', 
              'EXTRAVERSION', 'AGREEABLENESS', 	'EMOTIONAL_STABILITY', 	'CONSCIENTIOUSNESS', 	'OPENNESS_TO_EXPERIENCES',
              'MDD_SYMPTOMS_CURRENT_', 'GAD_SYMPTOMS_CURRENT_', 'Current_Psych_Medication', 'Current_Psychotherapy',
              'Avoidant_Coping_3cat')
newnames_pcl <- c('Age', 	'Sex', 	'CollegeGraduate', 	'Partner', 	'High_Income', 	
                  'ACES', 	'Traumas', 
                  'Reexp-Internal', 'Reexp-External',  	'Avoidance', 	'Anhedonia', 	'ExtBehavs', 	'AnxArousal', 	'DysArousal', 	
                  'PCL8', 'PCL9', 'PCL10','PCL11',
                  'Extraversion', 	'Agreeableness', 	'Emotional_stability', 	'Conscientiousness', 	'Openness',
                  'MDD_Symptoms', 'GAD_Symptoms', 'Psych_Medication', 'Psychotherapy',	
                  'Avoidant_Coping')



df_pcl <- subset(db, select = keep_pcl)
colnames(df_pcl) <- newnames_pcl
# Number of participants

dim(df_pcl)[1]
# [1] 4069

df_pcl <- na.omit(df_pcl)
dim(df_pcl)[1]
#[1] 3121

sapply(df_pcl, class)


########################
### Network analysis ###
########################


types <-  c('g', 'c', 'c', 'c', 'c', 'g', 'g', 
            'g', 'g', 'g', 'g', 'g', 'g', 'g', 
            'g', 'g', 'g', 'g', 
            'g', 'g', 'g', 'g', 'g', 
            'g', 'g', 'c', 'c', 
            'p')
levels <- c(1, 2, 2, 2, 2, 1, 1,
            1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1,
            1, 1, 1, 1, 1,
            1, 1, 2, 2,
            1)
groups <- c('demographic', 'demographic', 'demographic', 'demographic', 'demographic', 'demographic', 'demographic', 
            'PCL', 'PCL', 'PCL', 'PCL', 'PCL', 'PCL', 'PCL',
            'Negative Affect', 'Negative Affect', 'Negative Affect', 'Negative Affect', 
            'Personality', 'Personality', 'Personality', 'Personality', 'Personality', 
            'Medical', 'Medical', 'Medical', 'Medical', 
            'Coping')

mgm_obj <- mgm(data = df_pcl,
               type = types,
               level = levels,
               lambdaSel = "CV",
               lambdaFolds = 10,
               k = 2,
               pbar = FALSE,
               scale = TRUE)

qgraph(input = mgm_obj$pairwise$wadj,  # now weighted adjacency matrix
       layout = "circle",
       nodeNames = colnames(df_pcl), 
       groups = groups,
       palette="colorblind",
       #threshold = 0.1,
       edge.color = mgm_obj$pairwise$edgecolor) # incorporate sign as color

###########################
### Relative importance ###
###########################

model_coping_pcl <- lm(Avoidant_Coping ~ Age + #Sex + CollegeGraduate + Partner+ High_Income +
                         ACES + Traumas +
                         MDD_Symptoms + GAD_Symptoms + Psych_Medication + Psychotherapy +
                         Extraversion + Agreeableness + Emotional_stability + Conscientiousness + Openness + 
                         `Reexp-External` + `Reexp-Internal` + Avoidance + Anhedonia + ExtBehavs + AnxArousal + DysArousal+
                         PCL8 + PCL9 + PCL10 + PCL11, 
                       data = df_pcl)


summary(model_coping_pcl)

boot_pcl <- boot.relimp(model_coping_pcl, type = 'lmg', b = 1000)
booteval_pcl <- booteval.relimp(boot_pcl)
plot(booteval_pcl)



label = c('Age', 'Sex',	 'CollegeGraduate', 'Partner', 'High_Income',
          'ACES', 'Trauma',
          'MDD symptoms #', 'GAD symptoms #', 'Current psych-medication', 'Current psychotherapy', 	
          'Extraversion', 	'Agreeableness', 	'Emotional stability', 	'Conscientiousness', 	'Openness to experience', 
          'Re-experiencing', 	'Avoidance', 'Anhedonia', 	'Externalizing behaviors', 	'Anxious arousal', 	'Dysphoric arousal',
          'PCL8', 'PCL9', 'PCL10', 'PCL11')

lgmValues <- booteval_pcl@lmg # mean of each variable
lgmLow <- booteval_pcl@lmg.lower[1,] # create as vector instead of 1D matrix
lgmHigh <- booteval_pcl@lmg.upper[1,]
lmgData <- tibble(lgmValues, lgmLow, lgmHigh, label)
# sort data by value
lmgOrdered <- lmgData[order(lgmValues, decreasing = F),]
# set label as factor with ordered levels
lmgOrdered$label <- factor(lmgOrdered$label, levels = lmgOrdered$label)
#plot
pdf('Documents/coping/Figure_relimp_pcl.pdf')
ggplot(lmgOrdered, aes(x=label, y=(lgmValues*100))) +  geom_bar(stat="identity", fill='lightblue',
                                                                position=position_dodge()) +  geom_errorbar(aes(ymin=(lgmLow*100), ymax=(lgmHigh*100)), width=.2,
                                                                                                            position=position_dodge(.9)) + coord_flip() + ylab("Values [LMG in %]") + xlab('Predictors of Avoidance coping mechanism')  +theme_minimal()
dev.off()



save.image('Documents/coping/coping.RData')
