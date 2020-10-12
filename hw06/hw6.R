#!/usr/bin/env Rscript
setwd("~/Documents/study/msu/prog/prac/course3/msu_math_stat_prac_autumn_2020/hw06")

#1. Ознакомиться с процедурой бутстреп (bootstrap).

library(boot)
library(nortest)

#оценим матожидание по выборочному среднему
#для этого возьмем 10000 подвыборок нашей выборки
#и для каждой посчитаем среднее
Boot <- boot(norm100_1,
             #index - массив индексов выбранных для
             #подвыборки элементов. Берем по этим
             #элементам среднее
             function(sample, index) mean(sample[index]),
             R = 100000)

#полученное значение
mean(Boot$t)

#посмотрим, насколько отличаются результаты
mean(Boot$t) - mean(norm100_1)

#доверительные интервалы для матожидания
boot.ci(Boot, index = 1)

#2. Сгенерировать данные из нормального распределения с различными параметрами
#   и провести анализ с помощью графиков квантилей, метода огибающих, а также 
#   стандартных процедур проверки гипотез о нормальности, рассмотренных 
#   на семинаре (6 тестов). Рассмотреть выборки малого 
#   (не более 50-100 элементов) и умеренного (1000-5000 наблюдений) объемов.

norm100_2 = rnorm(100, -10, 7)
norm5000_1 = rnorm(5000, 9, 123)
norm5000_2 = rnorm(5000, -111, 0.0001)

png(file = 'qqplot1.png')
qqnorm(norm100_1)
qqline(norm100_1)
dev.off()

png(file = 'qqplot2.png')
qqnorm(norm5000_2)
qqline(norm5000_2)
dev.off()

shapiro.test(norm100_1)
ad.test(norm100_1)
cvm.test(norm100_1)
lillie.test(norm100_1)
sf.test(norm100_1)

shapiro.test(norm100_2)
ad.test(norm100_2)
cvm.test(norm100_2)
lillie.test(norm100_2)
sf.test(norm100_2)

shapiro.test(norm5000_1)
ad.test(norm5000_1)
cvm.test(norm5000_1)
lillie.test(norm5000_1)
sf.test(norm5000_1)

shapiro.test(norm5000_2)
ad.test(norm5000_2)
cvm.test(norm5000_2)
lillie.test(norm5000_2)
sf.test(norm5000_2)

#3. Сгенерировать данные из комбинаций, реализованных в R распределений, а 
#   затем провести анализ с помощью графиков квантилей, метода огибающих, а 
#   также стандартных процедур проверки гипотез о нормальности. 
#   Рассмотреть выборки малого и умеренного объемов. 
#   Сравнить эффективность методов.

rand_rate = rexp(100,5)
gamma_small = rgamma(100, 5, rand_rate)

rand_shape = rbinom(5000, 10, 0.2)
gamma_large = rgamma(100, rand_shape, 0.5)

png(file = 'qqplot3.png')
qqnorm(gamma_small)
qqline(gamma_small)
dev.off()

shapiro.test(gamma_small)
ad.test(gamma_small)
cvm.test(gamma_small)
lillie.test(gamma_small)
sf.test(gamma_small)

png(file = 'qqplot4.png')
qqnorm(gamma_large)
qqline(gamma_large)
dev.off()

shapiro.test(gamma_large)
ad.test(gamma_large)
cvm.test(gamma_large)
lillie.test(gamma_large)
sf.test(gamma_large)

