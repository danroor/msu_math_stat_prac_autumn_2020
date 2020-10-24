# Однофакторный дисперсионный анализ

# Создадим таблицу с данными:
tomato <- data.frame(weight =
c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5, # water
1.5, 1.2, 1.2, 2.1, 2.9, 1.6, # nutrient
1.9, 1.6, 0.8, 1.15, 0.9, 1.6), # nutrient+24D
trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
c(6, 6, 6)))

# Таблица со средними значениями:
Means <- data.frame(weight = as.numeric(tapply(tomato$weight,
tomato$trt, mean)),
trt = rep("Means", 3))

# Добавляем таблицу Means к таблице tomato:
tomato <- rbind(tomato, Means)

# Изменяем базовый уровень фактора trt на Water:
tomato$trt <- relevel(tomato$trt, ref = "Water")

# Рисуем исходную диаграмму (все точки из группы Means будут при этом
# автоматически залиты черным цветом):
stripchart(weight ~ trt, data = tomato, pch = 19,
col = c("blue", "red", "black"),
ylab = "Условия", xlab = "Вес (кг)")

# Добавляем поверх точек из группы Means точки нужного цвета
# (эти новые точки имеют координаты (средняя_1, 4), (средняя_2, 4)
# и (средняя_3, 4); их цвет прописываем "вручную"):
points(x = Means$weight, y = c(1, 1, 1), pch = 19,
col = c("blue", "red", "black"))

# Измененные данные
tomato2 <-
data.frame(weight =
c(1.25, 1.9, 1.3, 1.5, 2.3, 1.45, # water
2.1, 2.2, 2.4, 2.9, 2.8, 3.1, # nutrient
0.9, 1.2, 0.8, 1.3, 0.7, 1.4), # nutrient+24D
trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
c(6, 6, 6)))

# Таблица со средними значениями:
Means2 <- data.frame(weight = as.numeric(tapply(tomato2$weight,
tomato2$trt, mean)),
trt = rep("Means", 3))

# Добавляем таблицу Means к таблице tomato:
tomato2 <- rbind(tomato2, Means2)

# Изменяем базовый уровень фактора trt на Water:
tomato2$trt <- relevel(tomato2$trt, ref = "Water")

# Рисуем исходную диаграмму (все точки из группы Means будут при этом
# автомтически залиты черным цветом):
stripchart(weight ~ trt, data = tomato2, pch = 19,
col = c("blue", "red", "black"),
ylab = "Условия", xlab = "Вес (кг)")

# Добавляем поверх точек из группы Means точки нужного цвета
# (эти новые точки имеют координаты (средняя_1, 4), (средняя_2, 4)
# и (средняя_3, 4); их цвет прописываем "вручную"):
points(x = Means2$weight, y = c(1, 1, 1), pch = 19,
col = c("red", "black", "blue"))

# Вид распределения Фишера при заданных степенях свободы
x = seq(0, 10, 0.1)
plot(x, df(x, 2, 15), type = "l")
abline(v = qf(0.95, 2, 15), lty = 2)

#Данные tomato нужны без учета Means
# Выполнение однофакторного дисперсионного анализа, aov() или lm():
summary(aov(weight ~ trt, data = tomato[1:18,1:2]))


# Двухфакторный дисперсионный анализ
library(HSAUR2)
data(weightgain)
str(weightgain)

library(ggplot2)
ggplot(data = weightgain, aes(x = type, y = weightgain)) +
geom_boxplot(aes(fill = source))

require(doBy)
summaryBy(weightgain ~ type + source, data = weightgain,
FUN = c(mean, sd, length))

#План эксперимента
plot.design(weightgain)

# График взаимодействий
with(weightgain, interaction.plot(x.factor = type,
trace.factor = source,
response = weightgain))

# Двухфакторный дисперсионный анализ:
M1 <- aov(weightgain ~ source + type + source:type,
data = weightgain)
# M1 <- aov(weightgain ~ source*type,
# data = weightgain)
summary(M1)