library(dplyr)
library(rafalib)
install.packages('effsize')
library(effsize)
install.packages('pwr')
library(pwr)

#H0 - средние баллы по математике для женщин и мужчин равны
#H1 - средние баллы по математике для женщин и мужчин различны

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

head(df,5)
# Оценим средний балл по математике для мужчин и женщин
.women <- df$math.score[df$gender==2]
.men <- df$math.score[df$gender==1]
length(.women)
length(.men)

mypar(1,2)
qqnorm(.men,main='мужчины')
qqline(.men)
qqnorm(.women,main='женщины')
qqline(.women)
#Заданная мощность теста 90% и необходимость обнаружить сильный эффект d = 0.8

#Так как выборки разного размера  используем, для расчета мощности теста функцию pwr.t2n.test 

pwr.t2n.test(n1 = 50, power = 0.9, d = 0.8, sig.level = 0.05, alternative = 'two.sided')

't test power calculation 

             n1 = 50
             n2 = 25.43503
              d = 0.8
      sig.level = 0.05
          power = 0.9
    alternative = two.sided'
#n2 = 26 

t.test(sample(.men,50),sample(.women,26))
'
Welch Two Sample t-test

data:  sample(.men, 50) and sample(.women, 26)
t = 0.61901, df = 57.966, p-value = 0.5383
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -4.367874  8.278643
sample estimates:
mean of x mean of y 
 71.84000  69.88462
'
#p-value > 0.05(alpha) различий нет

#H0 верна, средние баллы для мужин и женщин равны

'Расчитаем доверительный интервал в процентах для размера эффекта'

((71.84 - 69.88462)/69.88462) * 100
#2.798012
(mean(.men) - mean(.women))/mean(.women)*100
"8.006844"


ci <- t.test(.men,.women,alternative = 'two.sided')$conf.int
(ci / 69.88462) * 100

"[1] 4.640239 9.940970
attr(,conf.level)
[1] 0.95"

(ci / mean(.women)) * 100
'[1]  5.096103 10.917584
attr(,"conf.level")
[1] 0.95'


infer <- df%>%group_by(gender)%>%summarise(
  mu = mean(math.score),
  k = qt(0.975,length(math.score)-1),
  se = sd(math.score)/sqrt(length(math.score)),
  lowlevel = mean(math.score) - k * se,
  hilevel = mean(math.score) + k *se
)
infer
ci_m <- c(infer[1,5],infer[1,6])
ci_m <- as.numeric( c(infer[1,5],infer[1,6]))
ci_m
"67.65055 69.80588"
ci_w <- c(infer[2,5],infer[2,6])
ci_w <- as.numeric( c(infer[2,5],infer[2,6]))
ci_w
'62.29601 64.97040'
mypar(1,1)
plot(mean(.women), col=2, lwd=2, xlim=c(0.5,2.5),ylim=c(60,75),ylab='средний балл по математике',main=
       'интервальная оценка среднего балл по математике для мужчин и женщин')
interval = c(62.29, 64.97)
lines(x=c(1,1),y=interval, col = 'red',lwd=3)
points(1.5,mean(.men),col=3,lwd=2)
interval_1=c(67.44, 70.01)
lines(x=c(1.5,1.5),y=interval_1, col = 'blue',lwd=3)
legend('topleft',c('women','men'),fill=c('red','blue'))


"можно сделать вывод, что не получено статестически значимое различие"



