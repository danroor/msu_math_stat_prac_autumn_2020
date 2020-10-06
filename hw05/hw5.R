#!/usr/bin/env Rscript
setwd("~/Documents/study/msu/prog/prac/course3/hw/05")

df = read.csv('COVID-19-in-Italy.csv')

infected = df[df$region == 'Abruzzo',]$new_positive

install.packages('mitools')
install.packages('pan')
install.packages('mix')

library(mitools)
library(pan)
library(mix)
library(outliers)
library(Hmisc)

### 1. С помощью функций пакета outliers с помощью формальных критериев 
### для идентификации выбросов проверить в данных, является ли 
### максимальное наблюдение выбросом.

grubbs.test(infected)
### pvalue мало (< 0.05), принимаем гипотезу о том, что выброса нет

### требуется выборка малого размера (не более 30 наблюдений)
dixon.test(infected[1:30])
### pvalue велико (> 0.05), отвергаем гипотезу о том, что выброса нет,
### в пользу альтернативы: максимальное наблюдение является выбросом

chisq.out.test(infected)
### pvalue мало (< 0.05), принимаем гипотезу о том, что выброса нет

