weight= (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(weight)

weight$program <- as.factor(weight$program)

head(weight)

#load dplyr package
library(dplyr)

weight %>%
  group_by(program) %>%
  summarise(mean = mean(weight_loss),
            sd = sd(weight_loss))

#create boxplots
boxplot(weight_loss ~ program,
        data = weight, # data2,
        main = "Pérdida de peso por programa",
        xlab = "Programa",
        ylab = "Pérdida de peso",
        col = "steelblue",
        border = "black")

#fit the one-way ANOVA model
model <- aov(weight_loss ~ program, data = weight)

summary(model)


# Se toman los residuales del modelo
residuos <- residuals(model)
# Se crea un gráfico de Residuales vs Tiempo (variable obs)
plot(obs, residuos,
     main = "Residuales vs Tiempo",
     xlab = "Tiempo",
     ylab = "Residuales",
     pch = 20, # Tipo de punto
     col = "blue") # Color de los puntos
# Se añade una línea horizontal en y = 0 para facilitar la interpretación
abline(h = 0, col = "red", lty = 2)

# Se crea un gráfico de Residuales vs Tiempo (variable obs)
plot(residuos,
     main = "Residuales vs Tiempo",
     xlab = "Tiempo",
     ylab = "Residuales",
     pch = 20, # Tipo de punto
     col = "blue") # Color de los puntos

shapiro.test(model$residuals)

# Bartlett
bartlett.test(weight_loss ~ program, data = weight)

#load car package
library(car)
leveneTest(weight_loss ~ program, data = weight)

TukeyHSD(model, conf.level=.95)

plot(TukeyHSD(model, conf.level=.95))

library(ggplot2)
library(multcomp)
# Convertir TukeyHSD a data frame
tukey_df <- as.data.frame(TukeyHSD(model, conf.level=.95)[[1]])
tukey_df$comparison <- rownames(tukey_df)
# Crear el plot con ggplot2
ggplot(tukey_df, aes(x = comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Comparaciones múltiples de Tukey",
       x = "Comparación",
       y = "Diferencia") +
  theme_minimal()

#install.packages(lsr)
library(lsr)
etaSquared(model, anova=TRUE)

power.anova.test(groups = 3, n = 30, between.var = 98.92, within.var = 139.56, sig.level = 0.05)

power.anova.test(groups = 3, between.var = 98.92, within.var = 139.56, power = 0.8, sig.level =
                   0.05)