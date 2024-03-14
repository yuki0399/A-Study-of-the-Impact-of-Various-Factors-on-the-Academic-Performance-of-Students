install.packages('tidyverse')
install.packages('ggbeeswarm')
library(tidyverse)
library(ggplot2)

#两样本比较
#设置实验组是lunch为standard的数学成绩，对照组是lunch为free/reduced的数学成绩
lunch_standard<-data$math.score[data$lunch=='standard']
lunch_free<-data$math.score[data$lunch=='free/reduced']
#用Welch't检验
t.test(lunch_free,lunch_standard)
#用均值和直方图对结论检验
mean(lunch_free)
mean(lunch_standard)
hist(lunch_free)
hist(lunch_standard)
#得出结论，不吃午饭会对数学成绩有负面影响
#绘制柱状图
barplot(c(mean(lunch_standard), mean(lunch_free)), names.arg = c("Standard", "Free/Reduced"),
        col = c("red", "green"), beside = TRUE, ylim = c(0, max(c(lunch_standard, lunch_free))), 
        main = "Bar Plot - Math Scores by Lunch Type", ylab = "Mean Math Scores")

#设置实验组是lunch为standard的阅读成绩，对照组是lunch为free/reduced的阅读成绩
lunch_standard2<-data$reading.score[data$lunch=='standard']
lunch_free2<-data$reading.score[data$lunch=='free/reduced']
#用Welch't检验
t.test(lunch_free2,lunch_standard2)
#用均值和直方图对结论检验
mean(lunch_free2)
mean(lunch_standard2)
hist(lunch_free2)
hist(lunch_standard2)
#得出结论，不吃午饭会对阅读成绩有负面影响
#绘制柱状图
barplot(c(mean(lunch_standard2), mean(lunch_free2)), names.arg = c("Standard", "Free/Reduced"),
        col = c("red", "green"), beside = TRUE, ylim = c(0, max(c(lunch_standard2, lunch_free2))), 
        main = "Bar Plot - Reading Scores by Lunch Type", ylab = "Mean Reading Scores")


#设置实验组是lunch为standard的写作成绩，对照组是lunch为free/reduced的写作成绩
lunch_standard3<-data$writing.score[data$lunch=='standard']
lunch_free3<-data$writing.score[data$lunch=='free/reduced']
#用Welch't检验
t.test(lunch_free3,lunch_standard3)
#用均值和直方图对结论检验
mean(lunch_free3)
mean(lunch_standard3)
hist(lunch_free3)
hist(lunch_standard3)
#得出结论，不吃午饭会对写作成绩有负面影响
#绘制柱状图
barplot(c(mean(lunch_standard3), mean(lunch_free3)), names.arg = c("Standard", "Free/Reduced"),
        col = c("red", "green"), beside = TRUE, ylim = c(0, max(c(lunch_standard3, lunch_free3))), 
        main = "Bar Plot - Writing Scores by Lunch Type", ylab = "Mean Writing Scores")




install.packages('ggbeeswarm')
library(tidyverse)
df<-read.csv('StudentsPerformance.csv')
head(df)
#方差分析
#研究父母教育水平对数学成绩的影响
anova(aov(math.score~parental.level.of.education,df))
#画小提琴图
library(ggplot2)
library(ggbeeswarm)
ggplot(data =df,aes(x=parental.level.of.education,y= math.score))+
  geom_violin(aes(color =parental.level.of.education))+
  geom_beeswarm(aes(color =parental.level.of.education))+
  xlab("家长教育水平")+ylab("数学成绩")+
  theme_bw()
#Tukey法进行多重比较
TukeyHSD(aov(math.score~parental.level.of.education,df),conf.level = 0.95)
#计算均值
mean_scores <- aggregate(math.score ~ parental.level.of.education, data = df, FUN = mean)
mean_scores



#用回归研究数学成绩、阅读成绩和写作成绩之间的关系
# 首先，在R中加载所需的包
library(stats)
model <- lm(math.score ~ reading.score + writing.score, data = df)
summary(model)
#绘制数学成绩和阅读成绩的回归图
ggplot(df, aes(x = reading.score, y = math.score)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "Reading Score", y = "Math Score", title = "Regression of Math Score on Reading Score")
#绘制数学成绩与写作成绩的回归图
ggplot(df, aes(x = writing.score, y = math.score)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(x = "Writing Score", y = "Math Score", title = "Regression of Math Score on Writing Score")

