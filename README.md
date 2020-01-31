---
title: "STA 207 Project 2, Analysis of Class Type Effect and School ID on Teacher Math Test Scores"
output: 
  pdf_document: default
  html_document:
    df_print: paged
    fig_caption: yes
    number_sections: true
---

<style type="text/css">

body{ /* Normal  */
      font-size: 12px;
  }
math {
  font-size: tiny;
}  
</style>

```{r setup, include=FALSE}
knitr::opts_chunk$set(message=FALSE,warning=FALSE)
```

Team ID: 12

Name (responsibilities): Joseph Gonzalez (Proofread, Introduction, Background)

Name (responsibilities): Yanhao Jin (Main analysis, Causal statement, Conclusion and discussion)

Name (responsibilities): Ruichen Xu (Descriptive analysis, Plots and tables, Data processing)

Name (responsibilities): Bohao Zou (Main analysis, Model diagnostics)

# 1. Introduction

For Project 2, we continue to analyze the STAR dataset. At this time, we treat the teachers involved in Project STAR as the individual units. From the dataset, We assign a specific statistic (for example, mean and standard deviation) to their students’ math scores in each particular class type and school id. Specifically, we will use the mean math scores of one class student as a criterion to estimate the ability of the teaching of one teacher. This analysis is important because it can help teachers evaluate their performance and, if necessary, adjust their teaching style depending on class type.

## 1.1 Statistical questions of interest

* Is there a difference in math scaled score in 1st grade across teachers in different class types ?
* Is there a difference in math scaled score in 1st grade across teachers in different schools ?

## 1.2 Population and study design

In the STAR dataset from Harvard dataverse, we use school ID to indicate that the teachers are from different schools. In each school, students and teachers were randomly assigned to one of the three different class types. Therefore, the difference in the students’ math scores could be confidently attributed to the class type in different schools. Due to the study’s randomization design, it is not accurate to assert that all the smart children or best teachers were placed within a particular class type. It is also not accurate to claim that all the smart children or best teachers were placed in a particular school. So, it is reasonable to judge the teaching ability of one teacher by their students' mean math score from the specific class type.

In this project, we are interested in the effect of class type(small, regular, and regular+aide) on the teachers’ teaching ability in different schools. Specifically, this is a randomized block experiment, where school ID is treated as the block, and class type is treated as a factor. In the STAR dataset, we found that there is only one replicate observation for certain class types in some particular schools. We also interpreted the interaction effect between class type and school ID as the effect of class type in one particular school. This is not helpful because we care about all schools. Therefore, the interaction effect between class type and school ID is not essential in our project, and we drop the interaction effect from our model.  

# 2. Statistical Analysis

For all the variables of interest in the data set (teacher ID, school ID, class type, and math score of 1st-grade students), we first dropped all the missing values. Then, we used the two-way ANOVA model to analyze the effect of class type and school ID on the response variable($\small Y_{ij}$), which we define as the mean math score of all students in the same class size with the same school ID. We interpret this scaled math score as a measure of teaching performance.
```{r echo=FALSE}
library(foreign)
library(MASS)
library(plyr)
library(dplyr)
library(car)
library(AER)

#setwd("C:/Users/jinyanhao/Downloads/STA207 Statistical Methods for Research")
star = read.spss("STAR_Students.sav", to.data.frame = TRUE)

Data<- data.frame(star[,'g1tmathss'],star[,'g1classtype'],star[,'g1schid'], star[,'g1tchid']  )
Data$star....g1schid..<- as.factor(Data$star....g1schid..)
Data$star....g1tchid..<- as.factor(Data$star....g1tchid..)


Data<- na.omit(Data)
colnames(Data)<- c("mathscore", "classtype", "schoolid", "teacherid")

Data.teacher<- data.frame(tAbility=tapply(Data[,1], Data[,4],mean))
class_size<- vector()
school_id<- vector()

for(i in 1:6598)
  class_size[Data[i,4]]=Data[i,2]


for(i in 1:6598)
  school_id[Data[i,4]]=Data[i,3]

Data.teacher<- cbind(Data.teacher, class_size = class_size,school_id )
Data.teacher$class_size <- as.factor(Data.teacher$class_size)
Data.teacher$school_id <- as.factor(Data.teacher$school_id)
boxTrans = function(x,lam){
  return((x^lam - 1) / lam)
}

### log trans 
newData = data.frame(tAbility = Data.teacher$tAbility, classSize = class_size,shcoolID = school_id)
tAbilityV = c()
classSizeV = c()
schoolIDV = c()
for (i in c(1:76)){
  unitTAV = c()
  unitCSV = c()
  unitSIV = c()
  for (j in c(1:3)){
    for (k in c(1:length(Data.teacher$tAbility))){
      if (class_size[k] == j && school_id[k] == i){
        unitTAV = c(unitTAV,newData$tAbility[k])
        unitSIV = c(unitSIV,school_id[k])
        unitCSV = c(unitCSV,class_size[k])
      }
    }
  }
  if (length(unitCSV)>=3 && 1 %in% unitCSV && 2 %in% unitCSV && 3 %in% unitCSV) {
    tAbilityV = c(tAbilityV,unitTAV)
    classSizeV = c(classSizeV,unitCSV)
    schoolIDV = c(schoolIDV,unitSIV)
  }
}

minMax = function(x){
  minN = min(x)
  maxN = max(x)
  return((x - minN) / (maxN - minN) + 0.0001)
}

deletedNewData = data.frame(tAbility = minMax(tAbilityV),class_size = factor(classSizeV), schoolID = factor(schoolIDV))
```
Since the mean math scores are quite similar between many teachers, the resulting pairwise comparisons of math scores between these teachers are likely to be small. To make it intuitive, we apply the following transformation
$$\small \tilde{Y}_{ijk}=\frac{Y_{ijk}-\min_{i,j,k}\{Y_{ijk}\}}{\max_{i,j,k}\{Y_{ijk}\}-\min_{i,j,k}\{Y_{ijk}\}}$$

The normalized data will be our new response variable $\small \tilde{Y}_{ijk}$ in the model.

To determine whether there are significant main effects of class type and school ID on the scaled math score for each teacher, we propose following two-way ANOVA model for this project
$$\small \tilde{Y}_{i j k}=\mu+\alpha_{i}+\beta_{j}+\varepsilon_{i j k}$$

* $\small \tilde{Y}_{ijk}$ is the normalized mean math score of the $\small k$-th unit in $\small i$-th class type and $\small j$-th school ID. Also, $\small i=1,2,3$, $j=1,2,\dots,76$ and $k=1,\dots,n_{ij}$. $\small n_{ij}$ is the number of teachers in $\small i$-th class type and $\small j$-th school ID.
* $\small \alpha_{i}$ is the main effect of the $\small i$-th level of class type(small, regular and regular+aide). $\small \beta_{j}$ is the main effect of the $\small j$-th school ID. 
* $\small \varepsilon_{ijk}$ follows the normal distribution with zero as the mean and $\small \sigma^{2}$ as the variance.

In our project, the math scaled score assigned to each teacher is a statistic of their students' math scores. We evaluate $\small Y_{ijk}$ as a measure of teaching ability when the teacher is assigned to a certain class type and school. To answer our questions of the interest, we will investigate the following hypothesis tests:
$$\begin{aligned}
\small H_{10}:\text{all }\alpha_{i}\text{ equal to zero}\quad & \small v.s.\quad \small H_{1a}:\text{not all }\alpha_{i}\text{ equal to zero}\\
\small H_{20}:\text{all }\beta_{i}\text{ equal to zero}\quad & \small v.s.\quad\small H_{2a}:\text{not all }\beta_{i}\text{ equal to zero}\end{aligned}$$

Following Assumptions are made in our model:

* Normality: The error terms are identically and independently distributed from a normal distribution with a mean of zero and a variance of $\small \sigma^{2}$.
* Independence and Homogeneity of variance: The outcomes are independent with each other and the variance of the error terms are equal in each class type and school ID.
* There is no interaction effect between class type and school ID on the math scaled score.

# 3. Results

## 3.1 Descriptive Analysis

The histogram of math score, boxplot of math score for each teacher by class type, boxplot of math score for each teacher by school ID and interaction diagram for class type and school ID are all shown in Figure 3.1.1.  

* The histogram of math score (top left) suggests that the distribution of math scores for teachers is approximately symmetric. Most math scores are distributed between 520 to 550. We need to apply data transformation to our response variable. 
* The boxplot of math score for each teacher by class type shows that the math scores of teachers in small class tend to higher than those in the other two class types. The boxplot indicates that there might be a significant effect of class type on the math score scale for teachers. 
* The boxplot of math scores for teachers by different school ID (bottom left in Figure 3.1.1) shows that the math scores of teachers are contrasting in different schools. 
* The interaction plot(bottom right in Figure 3.1.1) shows the paralleling patterns across three lines for different class types. This further supports that the interaction effect can be ignored in our project.

```{r echo=FALSE, fig.height=4}
library(ggpubr)
##density Plot
Data.Teacher<- Data.teacher
colnames(Data.Teacher)<- c("tAbility","class_type","School_id")
library(ggplot2)
plot1<-ggplot(Data.Teacher, aes(x=tAbility)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + ggtitle("overview of teaching ability")   +scale_x_continuous(name="The teaching ability of teacher")+scale_y_continuous(name="Density")+theme(,plot.title = element_text(hjust = 0.5)) 


##boxplot
plot2<-ggplot(Data.Teacher, aes(x=class_type, y=tAbility,color=class_type, fill = class_type)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=7,outlier.size=4)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme(legend.position="right")+
  labs(title="Teaching ability by class type",x="Class type taught by teacher", y = "Teaching ability")+theme(,plot.title = element_text(hjust = 0.5))
##boxplot
Data.Teacher$School_id<- as.factor(Data.Teacher$School_id)
plot3<-ggplot(Data.Teacher, aes(x=School_id, y=tAbility)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=7,outlier.size=4)+labs(title="Teaching ability by school id",x="School id taught by teacher", y = "Teaching ability") + scale_x_discrete(breaks = seq(1,76,10))
Data.Teacher$School_id<-as.numeric(Data.Teacher$School_id)
plot4<-ggplot(data=Data.Teacher,aes(x=School_id,y=tAbility,group=class_type)) + geom_line(size=0.6,aes(colour=class_type)) + geom_point(size=.6,aes(shape=class_type,colour=class_type)) + xlab("school id")+ylab("Teaching ability")+labs("Interaction plot")

ggarrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2)


```

Figure 3.1.1. The multiplot for descriptive analysis. Top left: histogram of math score; Top right: boxplot of math score for each teacher by class type; Bottom left: boxplot of math score for each teacher by school ID; Bottom right: interaction plot for class type and school ID. (Legend: 1 for small class(red), 2 for regular class(green) and 3 for regular class with aide(blue))

## 3.2 Inferential Analysis

|  | DF | Sum Sq | Sum mean | F value | Pr(>F) | Significance |  
|-----------------------|:---:|-------:|----------|---------|---------|--------------|  
|Anova model with interaction | | | | | | |
| Class type | 2 | 0.902 | 0.451 | 20.660 | 2.47e-8 | *** |  
| School ID | 71 | 10.152 | 0.143 | 6.552 | 2.2e-16 | *** |  
| Class type: School Id | 142 | 2.9042 | 0.020 | 0.9371 | 0.6438 |  |  
| Residuals | 109 | 2.3790 | 0.022 |  |  |  |  
| | | | | | | |
|Anova model without interaction | | | | | | |
| Class type | 2 | 0.881 | 0.441 | 20.915 | 3.97e-9 | *** |  
| School ID | 71 | 10.153 | 0.143 | 6.796 | 2.2e-16 | *** |  
| Residuals  | 251 | 5.283 | 0.021 | 0.9371 | 0.6438 |  |  

Table 3.2.1 ANOVA Table Result for Inferential Analysis. Top: ANOVA Table for Full Model (Y~Class Type*School ID). Bottom: ANOVA Table for Reduced Model  (Y~Class Type+School ID).

After the data transformation, the result for checking the interaction effect is given by Table 3.2.1(Top). The test results show that interaction effects between class type and school ID are most likely to be absent from this data set. As a result, we drop the interaction term in the ANOVA model. Table 3.2.1(Bottom) shows the results for the fitted model without the interaction term. 

Based on the F-test associated with the ANOVA model, the result shows that there are significant differences in the mean math scores of all students in 1st grade for each teacher in different class types and with different school IDs.

```{r include=FALSE}
### test interaction term
interBox = boxcox(tAbility~class_size + schoolID + schoolID : class_size,data=deletedNewData)
transNumber = interBox$x[order(interBox$y)[length(interBox$y)]]
```
```{r include=FALSE}
fullModel = lm(boxTrans(tAbility,transNumber)~class_size + schoolID + schoolID : class_size,data=deletedNewData)
anova(fullModel)
```

```{r include=FALSE}
### test school id
schoolidBox = boxcox(tAbility~class_size + schoolID,data = deletedNewData)
transNumber = schoolidBox$x[order(schoolidBox$y)[length(schoolidBox$y)]]
#reduce_schoolid = lm(boxTrans(tAbility,transNumber)~class_size + schoolID,data = deletedNewData)
#anova(reduce_schoolid)

### test class size
reduce_classSize = lm(boxTrans(tAbility,transNumber)~schoolID + class_size,data = deletedNewData)
#anova(reduce_classSize)

### use car package to check again
Anova(reduce_classSize)
```

```{r echo=FALSE, fig.height=3}
alpha = 0.05
Ttest=TukeyHSD(aov(reduce_classSize),conf.level = 1-alpha)
par(mfrow=c(1,2))
plot(Ttest, las=1, col="brown", which=1)
```

Figure 3.2.2 Plot of Tukey's Process. Left: Difference of transformed data in mean levels of School ID; Right: Difference of transformed data in mean levels of Class type.

Under significance level $\small \alpha=0.05$, Tukey's test shows that

* There is a difference between the small class type and regular class type. The mean math score of the small class type is larger than the regular class.
* There is a difference between the small class type and regular with aide class type. The mean math score of the small class type is larger than the regular + aide class.
* There is no difference between the regular class and regular + aide class.

## 3.4 Model Diagnostics

|  | Diagnostic Table Result |  |  |  
|------------------------------------------------|:-----------------------:|----------------:|-----------------|  
| Lilliefors normality test |D=0.036||p-value=0.385|  
| Bartlett test of homogeneity of variance |K-squared=14.632|df=8|p-value=0.067|  

Table 3.4.1 Model Diagnostics Table Results for Normality Assumption and Homogeneity of Variance. Top: Lilliefors Test for Normality Test; Bottom: Bartlett Test for Homogeneity of Variance.

### 3.4.1 Assumption for ANOVA and Randomized Block Experiment

* Normal Assumption: The results of Lillie's test suggests suggests that our response variables are approximately normally distributed when the response variable is the students' mean math score from the same class type and school ID.

* Homogeneity of Variance: While there might be only one or two replications that influence the test severely for some particular class types and school IDs, the result of Bartlett's test shows that discarding these cases will roughly satisfy homogeneity of variance.

* Independence: In the experimental design, the teachers and students are randomly assigned to different class types. Furthermore, investigators followed the standard procedures for confidentiality and human subjects’ research. There is no extensive communication between teachers in different schools that will affect the students’ math test scores. This randomization reduces confounding factors, like copying answers from a friend's test, and implies that the outcome of one student’s math score does not depend on another student’s math score. Therefore, the independence assumption is roughly satisfied. 

* There is no interaction between the block(school ID) and treatments(class types). Based on the Table 3.2.1 (Top), there is no interaction between school ID and class type.

### 3.4.2 Possibility of Making Causal Statements

In the STAR data set, each school has enough students to form at least one of the three class types. All teachers and students were randomly assigned to their class type, and this suggests that there was no underlying interventions other than class type and teacher aides. Considering the assumptions based on the causal inference framework, we can make a causal statement regarding class types and teachers’ performance based on their students’ math scores.

1. Stable unit treatment value assumption: This assumption is satisfied for two reasons: (a) The scaled math score, which measures a certain kind of teaching ability for each teacher, is a well defined potential outcome in this experiment. The treatment assignment of one teacher does not affect the potential outcomes of another teacher. In the STAR project, it is assumed that teachers do not interfere with each other’s work. Therefore, the teachers assigned to a certain school ID and class type do not interfere with another teacher’s outcome in the experiment. (b) In this experiment, the students assigned into each class type will receive the education for that class type and in that school consistently in the future. This implies the treatment factor class type and block factor school ID are stable.

2. Strong ignorability assumption: When we consider the effect of class type on teachers’ performance(based on their students’ scaled math score) in different schools, it is called a stratified randomized experiment. In this case, the stratum or subset of the population is the school ID. In each stratum, the students and teachers are randomly assigned to each class type. The randomization of the experiment in STAR project indicates that other factors such as gender, the race of the students, or the degree level of the teacher will not affect the class type or school ID.

# 4. Conclusion and Discussion

Based on the analysis, under significance level $\small \alpha=0.05$, we are $\small 95\%$ confident that class type has a significant effect on the mean math score for each teacher in different schools. Therefore, the general ability to teach students differs in each class type across different schools. Teachers in the small class type are more likely to teach better than those in the other two class types. This makes sense because, in smaller classes, teachers can provide to the students' needs more effectively. Besides, the school ID also has a significant effect on mean math scores for each teacher because different schools have different policies and equipment.

In this project, we consider each teacher with a different teacher ID as an individual unit. Therefore, the response variable measures some kinds of teaching ability of the teacher. However, in the previous project, we treat the students in different class types as the individual unit. The response variable will measure the learning ability of the students. The number of factors in project 1 and 2 is different. We consider the school ID and class type in this project and treat the school ID as a block in the experiment.


\newpage

# 5. Appendix

## 5.1 Session Information

```{r}
print(sessionInfo(), local = FALSE)
```

## 5.2 Reference

[1]. Virgilio Gómez Rubio. Book review: ggplot2 – Elegant Graphics for Data Analysis (2nd Edition)[J]. Journal of statistical software, 2017, 77(Book review 2).

[2]. ER. The State of Tennessee's Student/Teacher Achievement Ratio (STAR) Project: Technical Report (1985-1990).[J]. 1990.

[3]. Shin Y , Raudenbush S W . The Causal Effect of Class Size on Academic Achievement: Multivariate Instrumental Variable Estimators With Data Missing at Random[J]. Journal of Educational and Behavioral Statistics, 2011, 36(2):154-185.

[4]. Boyd-Zaharias, Jayne. PROJECT STAR The Story of the Tennessee Class - Size Study. PROJECT STAR The Story of the Tennessee Class - Size Study, American Educator, 1999.

[5]. C.M. Achilles; Helen Pate Bain; Fred Bellott; Jayne Boyd-Zaharias; Jeremy Finn; John Folger; John Johnston; Elizabeth Word, 2008, "Tennessee's Student Teacher Achievement Ratio (STAR) project", https://doi.org/10.7910/DVN/SIWH9F, Harvard Dataverse, V1; PROJECT STAR.zip 

## 5.3 Resources
[1]. https://www.aft.org/sites/default/files/periodicals/STARSummer99.pdf

[2]. https://dataverse.harvard.edu/file.xhtml?fileId=666713&version=1.0

[3]. https://dataverse.harvard.edu/file.xhtml?fileId=666707&version=1.0

[4]. https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SIWH9F

[5]. https://www.classsizematters.org/wp-content/uploads/2016/09/STAR-Technical-Report-Part-I.pdf

[6]. https://www.classsizematters.org/wp-content/uploads/2016/09/STAR-Technical-Report-Part-II.pdf



