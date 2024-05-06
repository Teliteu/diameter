library(dplyr)
library(lubridate)

#organização e definição dos dados
dados_ordenados <- base_completa %>%
  arrange(i_arbre, tempo) %>%
  group_by(i_arbre) %>%
  mutate(
    intervalo_dias = difftime(lag(tempo), tempo, units = "days"),
    intervalo_anos = as.numeric(-1 * (intervalo_dias) / 365.25),
    diametro_anterior = lag(diametromm),
    diferenca_diametro = diametromm - diametro_anterior,
    incremento_anual = ifelse(!is.na(diferenca_diametro), diferenca_diametro / intervalo_anos, NA)
  ) %>%
  ungroup()

#cálculo/indivíduo
individuo <- dados_ordenados %>%
  group_by(i_arbre) %>%
  summarize(
    media_incremento = round(mean(incremento_anual, na.rm = TRUE)),
    desvio_padrao = round(sd(incremento_anual, na.rm = TRUE)),
    .groups = 'drop'
  )

#cálculo/espécie
especie <- dados_ordenados %>%
  group_by(gen_especie) %>%
  summarize(
    media_incremento = round(mean(incremento_anual, na.rm = TRUE)),
    desvio_padrao = round(sd(incremento_anual, na.rm = TRUE)),
    .groups = 'drop'
  )

#cálculo/sítio
sitio <- dados_ordenados %>%
  group_by(Base) %>%
  summarize(
    media_incremento = round(mean(incremento_anual, na.rm = TRUE)),
    desvio_padrao = round(sd(incremento_anual, na.rm = TRUE)),
    .groups = 'drop'
  )

#cálculo/espécie_sítio
especie_sitio <- dados_ordenados %>%
  group_by(gen_especie, Base) %>%
  summarize(
    media_incremento = round(mean(incremento_anual, na.rm = TRUE)),
    desvio_padrao = round(sd(incremento_anual, na.rm = TRUE)),
    .groups = 'drop'
  )

#gráficos

barplot(especie$media_incremento,names.arg = especie$gen_especie,
        main = "Média de Incremento Anual por Espécie",
        xlab = "Espécie",
        ylab = "Média de Incremento Anual (mm)",
        col = "blue",
        las = 2, cex.axis = 0.6, cex.names = 0.6)

barplot(sitio$media_incremento, names.arg = sitio$Base,
        main = "Média de Incremento Anual por Sítio",
        xlab = "Sítio",
        ylab = "Média de Incremento Anual (mm)",
        col = "blue",
        las = 2, cex.axis = 0.6, cex.names = 0.6) 

boxplot(dados_ordenados$incremento_anual ~ dados_ordenados$gen_especie,
        main = "Boxplot de Incremento Anual por Espécie",
        xlab = "Indivíduo (Árvore)",
        ylab = "Incremento Anual (mm)",
        col = "lightblue",
        las = 2, cex.axis = 0.6, cex.names = 0.6) 

boxplot(dados_ordenados$incremento_anual ~ dados_ordenados$Base,
        main = "Boxplot de Incremento Anual por Sítio",
        xlab = "Indivíduo (Árvore)",
        ylab = "Incremento Anual (mm)",
        col = "lightblue",
        las = 2, cex.axis = 0.6, cex.names = 0.6) 

