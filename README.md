---
title: "STA 207 Project 2, Analysis of Class Size Effect and School ID on Teacher Math Test Scores"
date: "2020/1/27"
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

Name (responsibilities): Joseph Gonzalez (proofread)

Name (responsibilities): Yanhao Jin (main analysis, causal statement, conclusion and discussion, model diagnostics)

Name (responsibilities): Ruichen Xu (descriptive analysis)

Name (responsibilities): Bohao Zou

Bohao: generate the elegant table with caption for the results for ANOVA model

Ruichen: generate the elegant figures with captions for model diagnostics and descriptive analysis.
Together: read the report carefully to check if there are any mistakes in the analysis or typos.

Remember to submit the blinded version!

# 1. Introduction

 For Project 2, we will continue to analyze the Project STAR dataset and, this time, we will treat the teachers involved in Project STAR as the individual units. From the dataset, we use the school id and class size to identify different teachers. We assign a certain statistic (for example, mean and standard deviation) to their students’ math scores in each specific class size and school id. Specifically, we will use the mean math scores as our response variables, which allows our analysis to be more concise and meaningful. This analysis is important because it can help teachers evaluate their performance and, if necessary, adjust their teaching style depending on class size.

## 1.1 Statistical questions of interest

* Is there an interaction between class types and school IDs?
* Is there a difference in math scaled score in 1st grade across teachers in different class types?

## 1.2 Population and study design

In the STAR dataset from Harvard dataverse, we use school ID to indicate teachers from different schools. In each school, students and teachers were randomly assigned to three different class sizes. Therefore, the difference in the students’ math scores could be confidently attributed to class size in different schools. Due to the study’s randomization design, it is not accurate to assert that all the smart children or best teachers were placed within a particular class type. It is also not accurate to assert that all the smart children or best teachers were placed in a particular school.

In this project, we are interested in the effect of class type (small, regular and regular+aide) on the teachers’ teaching ability(reflected by a certain statistic of the math score of students in each class) in different schools. It is a randomized block experiment. The school ID is treated as the block and class type is treated as a factor.

# 2. Statistical Analysis

For this analysis, we will use a two-way ANOVA model to determine whether there are significant main effects for each class type and whether there is a significant interaction between class type and school ID. The general two-way ANOVA model is given by
$$\small Y_{i j k}=\mu+\alpha_{i}+\beta_{j}+(\alpha \beta)_{i j}+\varepsilon_{i j k}$$
where

* $\small Y_{ijk}$ is the math scaled score of the $\small k$-th teacher in $\small i$-th class type and $\small j$-th school. Also, $\small i=1,2,3$, $j=1,2,\dots,76$ and $k=1,\dots,n_{ij}$. $\small n_{ij}$ is the number of teachers in $\small i$-th class type for the $\small j$-th school.
* $\small \alpha_{i}$ is the main effect of $\small i$-th level of class type (small, regular and regular+aide). $\small \beta_{j}$ is the main effect of $\small j$-th school ID. And $\small (\alpha\beta)_{ij}$ is the interaction effect of the $\small i$-th class type and $\small j$-th school ID.
* $\small \varepsilon_{ijk}$ follows normal distribution with zero as the mean and $\small \sigma^{2}$ as the variance.

In our project, the math scaled score for each teacher is a statistic of the math score of the students taught by the teacher. $\small Y_{ijk}$ can be evaluated as a measure of teaching ability when the teacher is assigned to a certain class type and school. To answer our questions of the interest, we will investigate the following hypothesis tests:

$$\begin{aligned}
H_{10}:\text{All class size main effects(}\alpha_{i}\text{) are equal to zero}\quad & v.s.\quad H_{1a}:\text{Not all class size main effects(}\alpha_{i}\text{) are equal to zero}\\
H_{20}:\text{All school ID main effects(}\beta_{i}\text{) are equal to zero}\quad & v.s.\quad H_{2a}:\text{Not all school ID main effects(}\beta_{i}\text{) are equal to zero}\\
H_{30}:\text{All interaction effects(}(\alpha\beta)_{ij}\text{) are equal to zero}\quad & v.s.\quad H_{3a}:\text{Not interaction effects(}(\alpha\beta)_{ij}\text{) are equal to zero}\end{aligned}$$

# 3. Results

## 3.1 Data Processing

In the data set, the students in different class types in each school were taught by a unique teacher with a different teacher ID. For all the variables of interest in the data set (teacher ID, school ID, class type, and math score of 1st-grade students), we first dropped all the missing values. Then, we used the two-way ANOVA model to analyze the effect of class type and school ID on the response variable($\small Y_{ij}$), which we define as

* The mean math score of all students in the same class size with the same school ID. We interpret this scaled math score as a measure of teaching performance.
* The mean math score for the top ten percent of students taught by teachers in the same class size with the same school ID. We consider this scaled math score as a measure of teaching performance for extraordinary students.

```{r echo=FALSE}
library(foreign)
library(MASS)
library(plyr)
library(dplyr)
library(car)
library(AER)
setwd("C:/Users/RuichenXu/Desktop/Project 02")
star = read.spss("STAR_Students.sav", to.data.frame = TRUE)
Data<- data.frame(star[,'g1tmathss'],star[,'g1classtype'],star[,'g1schid'], star[,'g1tchid']  )
Data$star....g1schid..<- as.factor(Data$star....g1schid..)
Data$star....g1tchid..<- as.factor(Data$star....g1tchid..)


Data<- na.omit(Data)
colnames(Data)<- c("mathscore", "classtype", "schoolid", "teacherid")


Max10<-function(x){
  Temp<- quantile(x, 0.9)
  sum(x[x>Temp])/length(x[x>Temp])
}

Data.teacher<- data.frame(mean =tapply(Data[,1], Data[,4],mean) )
Data.teachervar<- data.frame(var =tapply(Data[,1], Data[,4],var))
Data.teachermed<- data.frame(med =tapply(Data[,1], Data[,4],median))
Data.teachermax<- data.frame(max =tapply(Data[,1], Data[,4],max))
Data.teachersd<- data.frame(sd =tapply(Data[,1], Data[,4],sd))
Data.teachermax10<- data.frame(max10 =tapply(Data[,1], Data[,4],Max10))
class_size<- vector()
school_id<- vector()

for(i in 1:6598)
  class_size[Data[i,4]]=Data[Data[i,4],2]
for(i in 1:6598)
  school_id[Data[i,4]]=Data[Data[i,4],3]

Data.teacher<- cbind(Data.teacher, class_size = class_size,school_id )
Data.teacher$class_size <- as.factor(Data.teacher$class_size)
Data.teacher$school_id <- as.factor(Data.teacher$school_id)

Data.teachermax10<- cbind(Data.teachermax10, class_size = class_size,school_id )
Data.teachermax10$class_size <- as.factor(Data.teachermax10$class_size)
Data.teachermax10$school_id <- as.factor(Data.teachermax10$school_id)
```

## 3.2 Descriptive Analysis

* Draw the boxplot of scaled math scores (selected statistic) in different class type and school ID.
* Draw the interaction plot.
* Give descriptive analysis on the effect of class type and interaction effect.
* Dont forgot to add the caption for all figures.

Evaluate the main effect of class types in each different school ID.

The Figure (???) suggests that there is no interaction effect between class type and school ID.

## 3.3 Inferential Analysis

(We will substitute the R output by elegant table later.)

We first check the significance of the interaction effect, and the result is given by Table (??)

```{r echo=FALSE}
full_model1=lm(mean~class_size+school_id+class_size:school_id,data=Data.teacher)
reduced_model1=lm(mean~class_size+school_id,data=Data.teacher)
anova(reduced_model1,full_model1)


full_model8=lm(max10~class_size+school_id+class_size:school_id,data=Data.teachermax10)
reduced_model8=lm(max10~class_size+school_id,data=Data.teachermax10)
anova(reduced_model8,full_model8)
```

The test results show that interaction effects between class type and school ID are very likely to be absent from this data set. As a result, we drop the interaction term in the ANOVA model. Table ?? shows the results for the fitted model without the interaction term.

```{r echo=FALSE}
Anova(reduced_model1, type=2)
Anova(reduced_model8, type=2)

anova.fit1<-aov(reduced_model1)
anova.fit8<-aov(reduced_model8)

summary(anova.fit1)
summary(anova.fit8)
```

Table ?? suggests that based on the F-test associated with ANOVA model:

* There is no significant difference in the mean math scores of all students in 1st grade for each teacher in different class types and with different school IDs.
* Under the significance level of $\small \alpha=0.1$, there is a significant difference in the mean math score of the top ten percent of students in 1st grade for each teacher in different class types. However, there is no significant difference for the school IDs.
```{r echo=FALSE}
alpha=0.1;
T.ci1=TukeyHSD(anova.fit1,conf.level = 1-alpha)
T.ci8=TukeyHSD(anova.fit8,conf.level = 1-alpha)
par(mfrow=c(2,2))
plot(T.ci1, las=1, col="brown", which=1)
plot(T.ci8, las=1, col="brown", which=1)
```

## 3.4 Model Diagnostics
The diagnostic plots of our ANOVA models are given in Figure (???).

```{r echo=FALSE}
par(mfrow=c(2,2))
plot(anova.fit1)
plot(anova.fit8)
```

### 3.4.1 Assumption for ANOVA and Randomized Block Experiment

Normal Assumption: Based on the QQ-plot, 

(a) When the response variable is the mean math score of all the students in the same class size and with the same school ID, the QQ-plot suggests that the data satisfies the normal assumption quite well.

(b) When the response variable is the mean math score of all students with a math score in the top ten percent, in the same class size, and have the same school ID, the QQ-plot suggests that the data roughly satisfies the normal assumption.

* Homogeneity of Variance: (Boxplot of the scaled math scores) In Figure (???), we see that the size of each box is roughly equal. Therefore, we can conclude that the variance in different class types and different schools is roughly the same.

* Independence: In the experimental design, The teachers and students are randomly assigned into different class types. Furthermore, investigators followed the standard procedures for confidentiality and human subjects’ research. There is no extensive communication between teachers in different schools that will affect the students’ math test scores. This  randomization reduces confounding factors, like cheating on a test, and implies that the outcome of one student’s math score does not depend on another student’s math score. Therefore, the independence assumption is roughly satisfied. 

* There is no interaction between the Block(school ID) and Treatments(class types). Based on the Table in Section 3.3, there is no interaction between school ID and class type.


### 3.4.2 Possibility of Making Causal Statements

In the STAR data set, each school has enough students to form at least one of the three class types. All teachers and students were randomly assigned to their class type and this suggests that there was no underlying interventions other than class size and teacher aides. Considering the assumptions based on the causal inference framework, we determine if we can make a causal statement regarding class types and teachers’ performance based on their students’ math scores..

1. Stable unit treatment value assumption (SUTVA).
SUTVA assumptions are satisfied for two reasons:
* The scaled math score which measures the teachers’ teaching ability, has well defined potential outcomes in this experiment. 
* The treatment assignment of one teacher does not affect potential outcomes of other another teacher. In STAR project, it is assumed that teachers do not interfere with each other’s work. Therefore, the teachers assigned to a certain school ID and class type does not interfere with another teacher’s outcome in the experiment.

2. Strong ignorability assumption: 
When we consider the effect of class type on teachers’ performance(based on their students’ scaled math score) in different schools, it is called a stratified randomized experiment. In this case, the stratum is the school ID. In each stratum, the students and teachers are randomly assigned into each class type. The randomization of the experiment in STAR project indicates that other factors such as gender, the race of the students or the degree level of the teacher will not affect the class type or school ID.


# 4. Conclusion and Discussion

At significance level $\small \alpha=0.1$, we can conclude that class type has a significant effect on the mean math scores of students with the top ten percent math scores from different schools. Therefore, the ability of teaching outstanding students is different across each class type in different schools. The top students in class type 3 (Need to be check!) tends to have higher grades than the students in class type 2 (Also need to check!). However, the analysis suggests that there is no difference in mean math score of all students taught by a certain teacher across each class type in different schools. It means that in some degree that there is no difference in teacher's general teaching ability across different class type in different schools.

In this project, we consider each teacher, with a different teacher ID, as an individual unit. Therefore, the response variable can be considered a  measurement for a teacher’s performance. However, in previous project, we treated the students in different class types as the individual unit. In that case, the response variable was considered as a measure of a student’s learning ability. We also note that the number of factors in project 1 and 2 is different. Specifically, we considered the school ID and class type in this project and examined the school ID as a block in the experiment.


\newpage

# 5. Appendix

## R Code
```{r eval=FALSE}
library(foreign)
library(MASS)
library(plyr)
library(dplyr)
library(car)
library(AER)
setwd("C:/Users/jinyanhao/Downloads/STA207 Statistical Methods for Research")
star = read.spss("STAR_Students.sav", to.data.frame = TRUE)
Data<- data.frame(star[,'g1tmathss'],star[,'g1classtype'],star[,'g1schid'], star[,'g1tchid']  )
Data$star....g1schid..<- as.factor(Data$star....g1schid..)
Data$star....g1tchid..<- as.factor(Data$star....g1tchid..)


Data<- na.omit(Data)
colnames(Data)<- c("mathscore", "classtype", "schoolid", "teacherid")


Max10<-function(x){
  Temp<- quantile(x, 0.9)
  sum(x[x>Temp])/length(x[x>Temp])
}

Data.teacher<- data.frame(mean =tapply(Data[,1], Data[,4],mean) )
Data.teachermax10<- data.frame(max10 =tapply(Data[,1], Data[,4],Max10))
class_size<- vector()
school_id<- vector()

for(i in 1:6598)
  class_size[Data[i,4]]=Data[Data[i,4],2]
for(i in 1:6598)
  school_id[Data[i,4]]=Data[Data[i,4],3]

Data.teacher<- cbind(Data.teacher, class_size = class_size,school_id )
Data.teacher$class_size <- as.factor(Data.teacher$class_size)
Data.teacher$school_id <- as.factor(Data.teacher$school_id)


Data.teachermax10<- cbind(Data.teachermax10, class_size = class_size,school_id )
Data.teachermax10$class_size <- as.factor(Data.teachermax10$class_size)
Data.teachermax10$school_id <- as.factor(Data.teachermax10$school_id)

full_model1=lm(mean~class_size+school_id+class_size:school_id,data=Data.teacher)
reduced_model1=lm(mean~class_size+school_id,data=Data.teacher)
anova(reduced_model1,full_model1)


full_model8=lm(max10~class_size+school_id+class_size:school_id,data=Data.teachermax10)
reduced_model8=lm(max10~class_size+school_id,data=Data.teachermax10)
anova(reduced_model8,full_model8)

Anova(reduced_model1, type=2)
Anova(reduced_model8, type=2)

anova.fit1<-aov(reduced_model1)
anova.fit8<-aov(reduced_model8)

summary(reduced_model1)
summary(reduced_model8)
summary(anova.fit1)
summary(anova.fit8)

plot(anova.fit1)
plot(anova.fit8)

alpha=0.1;
T.ci1=TukeyHSD(anova.fit1,conf.level = 1-alpha)
T.ci8=TukeyHSD(anova.fit8,conf.level = 1-alpha)
T.ci8
par(mfrow=c(2,2))
plot(T.ci1, las=1, col="brown")
plot(T.ci8, las=1, col="brown")

```

## Session Information

```{r}
print(sessionInfo(), local = FALSE)
```
