#!/usr/bin/env Rscript
setwd("~/Documents/study/msu/prog/prac/course3/hw/03")

#LIBRARIES
#############################################
library(dplyr)

#FUNCTIONS
#############################################
filterByColumnValue = function (df, column, value) {
  dplyr::filter(df, !!as.symbol(column)==value) 
  # !! discards quotes
  # from column string
}

plotToFile = function(x, y, xl, yl, title, File, file_func, Mfrow) {
  file_func(file = File, width = 1700, height = 1000)
  par(mfrow=Mfrow)
  xx = as.numeric(x)
  plot(x,  y, xlab = xl, ylab = yl, main = paste(title, '(points)'), sub = 'Just a subtitle', type = 'p', col = 'red')
  plot(xx, y, xlab = xl, ylab = yl, main = paste(title, '(line)'), type = 'l', col = 'green')
  plot(xx, y, xlab = xl, ylab = yl, main = paste(title, '(both, plot_height / plot_width = 0.2)'), type = 'b', asp = 0.2)
  plot(xx, y, xlab = xl, ylab = yl, main = paste(title, '("both" plot with dots dropped)'), type = 'c')
  plot(xx, y, xlab = xl, ylab = yl, main = paste(title, '(overplotted)'), type = 'o')
  plot(xx, y, xlab = xl, ylab = yl, main = paste(title, '(histogram-like)'), type = 'h')
  plot(xx, y, xlab = xl, ylab = yl, main = paste(title, '(stair steps)'), type = 's')
  plot(xx, y, xlab = xl, ylab = yl, main = paste(title, '(stair steps)'), type = 'S')
  plot(xx, y, xlab = xl, ylab = yl, main = paste(title, '(no plotting)'), type = 'n')
  dev.off() #closing file
}

histToFile = function(x, xl, yl, title, File, file_func, Mfrow) {
  file_func(file = File)
  par(mfrow=Mfrow)
  hist(x, xlab = xl, ylab = yl, main = title)
  dev.off()
}

#MAIN PROGRAM
#############################################
df = read.csv('COVID-19-in-Italy.csv')

Abruzzo = filterByColumnValue(df, 'region', 'Abruzzo')
july29  = filterByColumnValue(df, 'date', '2020-07-29 17:00:00')

Abruzzo_date = Abruzzo$date
Abruzzo_new = Abruzzo$new_positive

plotToFile(Abruzzo_date, Abruzzo_new, 
           xl = 'date', 
           yl = 'The amount of people with positive test results (daily)', 
           title = 'Positive tests in Abruzzo',
           File = 'Abrruzo_tests.png', 
           file_func = png, 
           Mfrow = c(3,3))

histToFile(july29$intensive_therapy, 
           xl = 'People', yl = 'Regions', 
           title = 'The amount of people undergoing intensive therapy',
           File = '29.07_intensive_therapy.pdf',
           file_func = pdf,
           Mfrow = c(1,1))

int_ther  = array(july29$intensive_therapy)
int_ther2 = rbind(int_ther)
'The mean amount of people undergoing intensive therapy'
apply(int_ther2, 1, mean)

isThereIntTher = function(x) {
  x > 0
}

sapply(int_ther, isThereIntTher)

c = rbind(array(c(1,2,0)), array(c(4,0,6)))
c
mapply(isThereIntTher, c)

l = list(3,'a')
L = list(23,'asd', l)
rapply(L, function(x) x * x, classes = 'numeric', how = 'replace')
