df <-
  read.csv(
    "/home/setet/Документы/Big_Data_Processing/Help_fo_Laba_2.csv",
    sep = ";" ,
    header = TRUE
  )

fill_name <- colnames(df)
fill_name <- fill_name[-1]

# 2
# Макс оценка
fill_max <- c()
for (i in 2:11) {
  p <- df[, i]
  fill_max <- c(fill_max, max(p))
}

df_max = data.frame(number = fill_name,
                    data = fill_max)

# Мин оценка
fill_min <- c()
for (i in 2:11) {
  p <- df[, i]
  fill_min <- c(fill_min, min(p))
}

df_min = data.frame(number = fill_name,
                    data = fill_min)
df_min

# Средн оценка
fill_mean <- c()
for (i in 2:11) {
  p <- df[, i]
  fill_mean <- c(fill_mean, mean(p))
}

df_mean = data.frame(number = fill_name,
                     data = fill_mean)
df_mean
# 2
# Кол-во оценок больше 3 и меньше 7
fill_all <- c()
for (i in 1:10) {
  p <- df[, i]
  fill_all <- c(fill_all, length(p[which(p > 3 | p < 7)]))
}
sum(fill_all)

# 3
# Рейтинг по убыванию
fill_average <- c()
a <- order(df_mean[, 2], decreasing = TRUE)
for (i in 1:length(df_mean[, 2])) {
  fill_average[i] <- df_mean[, 2][a[i]]
}
fill_average

# 4
# График
fill_colors <- c()
for (i in 1:length(df_mean[, 2])) {
  if (fill_average [i] > 6) {
    fill_colors <- c(fill_colors, "#00FF00")
  } else {
    fill_colors <- c(fill_colors, "#FF0000")
  }
}

p <- df_mean[a, 1]
p
barplot(
  fill_average ,
  names.arg = p,
  col = fill_colors,
  xlab = "Название",
  ylab = "Рейтинг",
  main = "Оценка цифр",
  width = sqrt(fill_average)
)
