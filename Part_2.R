library(dplyr)
library(rafalib)
df <-  read.csv('StudentsPerformance.csv')
dim(df)
str(df)
sum(is.na(df))

df <- df %>% 
  mutate(gender = case_when(gender == 'male' ~ 1,
                            gender == 'female' ~ 2))

df <- df %>% 
  mutate(race.ethnicity = case_when(race.ethnicity == 'group A' ~ 1,
                                    race.ethnicity == 'group B' ~ 2,
                                    race.ethnicity == 'group C' ~ 3,
                                    race.ethnicity == 'group D' ~ 4,
                                    race.ethnicity == 'group E' ~ 5))

mypar(1,3)
qqnorm(df$math.score, main="оценки по математике df")
qqline(df$math.score, col="red", lwd=2)
hist(df$math.score)
boxplot(df$math.score)
"Визуально данные выборки имеют распределение, близкое к нормальному"

#берем выборки, соблюдая условия случайности и независимости
s1.r1 <- sample(df$math.score[df$race.ethnicity==1&df$gender==1],20)
s1.r2 <- sample(df$math.score[df$race.ethnicity==2&df$gender==1],20)
s1.r3 <- sample(df$math.score[df$race.ethnicity==3&df$gender==1],20)
s1.r4 <- sample(df$math.score[df$race.ethnicity==4&df$gender==1],20)
s1.r5 <- sample(df$math.score[df$race.ethnicity==5&df$gender==1],20)

s2.r1 <- sample(df$math.score[df$race.ethnicity==1&df$gender==2],20)
s2.r2 <- sample(df$math.score[df$race.ethnicity==2&df$gender==2],20)
s2.r3 <- sample(df$math.score[df$race.ethnicity==3&df$gender==2],20)
s2.r4 <- sample(df$math.score[df$race.ethnicity==4&df$gender==2],20)
s2.r5 <- sample(df$math.score[df$race.ethnicity==5&df$gender==2],20)

set.seed(1)

gender.new <- c(rep(1,20),rep(2,20),rep(1,20),rep(2,20),rep(1,20),rep(2,20),rep(1,20),rep(2,20),rep(1,20),rep(2,20))
gender.new

race.new <- c(rep(1,40),rep(2,40),rep(3,40),rep(4,40),rep(5,40))

score_math <- c(s1.r1,s2.r1,s1.r2, s2.r2,s1.r3, s2.r3,s1.r4, s2.r4,s1.r5, s2.r5)

anovaframe <- data.frame(score_math, gender.new,race.new)
head(anovaframe, 25)

table(anovaframe$gender.new, anovaframe$race.new)


# разведочный анализ
boxplot(score_math ~ gender.new, data = anovaframe,
        boxwex = 0.15, at = 1:2-0.3, 
        subset = race.new == '1', col = '5',
        main = 'EDA ANOVA',
        xlab = 'пол',
        ylab = 'оценка математика',
        xlim = c(0.5, 2.7), ylib = c(0,200))
boxplot(score_math ~ gender.new, data = anovaframe, add = TRUE, 
        boxwex = 0.15, at = 1:2-0.1, 
        subset = race.new == '2', col = '2' )

boxplot(score_math ~ gender.new, data = anovaframe, add = TRUE, 
        boxwex = 0.15, at = 1:2+0.15, 
        subset = race.new == '3', col = 'brown' )

boxplot(score_math ~ gender.new, data = anovaframe, add = TRUE, 
        boxwex = 0.15, at = 1:2+0.35, 
        subset = race.new == '4', col = 'blue' )

boxplot(score_math ~ gender.new, data = anovaframe, add = TRUE, 
        boxwex = 0.15, at = 1:2+0.55, 
        subset = race.new == '5', col = 'green' )

legend('bottomleft', c('race=1','race=2','race=3','race=4'),
       fill = c('5','2', 'brown','blue', 'green'))

# наблюдаем неоднородность дисперсий
install.packages('car')

library(car)

res <- leveneTest(score_math ~ factor(gender.new)*factor(race.new), data = anovaframe)
res
"Levene's Test for Homogeneity of Variance (center = median)
       Df F value Pr(>F)
group   9  1.5698 0.1268
      190           "
res_b <-  bartlett.test(list(s1.r1,s2.r1,s1.r2, s2.r2,s1.r3, s2.r3,s1.r4, s2.r4,s1.r5, s2.r5))
res_b
"
data:  list(s1.r1, s2.r1, s1.r2, s2.r2, s1.r3, s2.r3, s1.r4, s2.r4, s1.r5, s2.r5)
Bartlett's K-squared = 14.69, df = 9, p-value = 0.09981
"
# принимается нулевая гипотема на уровне значимости 0.05
# статистически значимых различий между дисперсиями выборок нет, так как p-value 
# для критерия Левенэ и Бартлетта превышает 0.05

mypar(2,5)
qqnorm(s1.r1)
qqline(s1.r1)
qqnorm(s1.r2)
qqline(s1.r2)
qqnorm(s1.r3)
qqline(s1.r3)
qqnorm(s1.r4)
qqline(s1.r4)
qqnorm(s1.r5)
qqline(s1.r5)

qqnorm(s2.r1)
qqline(s2.r1)
qqnorm(s2.r2)
qqline(s2.r2)
qqnorm(s2.r3)
qqline(s2.r3)
qqnorm(s2.r4)
qqline(s2.r4)
qqnorm(s2.r5)
qqline(s2.r5)
# учитывая, что выборки одинаковые, отклонения которые мы видим, очень малы
summary(aov(score_math ~ gender.new*race.new, data = anovaframe))

'                     Df Sum Sq Mean Sq F value   Pr(>F)    
gender.new            1   2492    2492  11.209 0.000976 ***
race.new              1   4316    4316  19.414 1.73e-05 ***
gender.new:race.new   1   1429    1429   6.426 0.012024 *  
Residuals           196  43578     222                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1'


'Получено взаимодействие факторов "пол и раса", 
то есть они оказывают влияние на балл по математике на уровне значимости 0.05
Оба фактор: пол и раса оказывают значимый эффект на полученный балл по математике'

