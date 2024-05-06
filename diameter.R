library(dplyr)
#cálculo do crescimento médio por árvore
dados_crescimento <- dados %>%
  group_by(i_arbre, gen_especie, Base) %>%
  summarise(
    dap_inicial = first(diametromm[order(p23_cdmedicao)]),
    dap_final = last(diametromm[order(p23_cdmedicao)]),
    ano_inicial = min(p23_cdmedicao),
    ano_final = max(p23_cdmedicao),
    .groups = 'drop'
  ) %>%
  mutate(
    crescimento_anual = (dap_final - dap_inicial) / (ano_final - ano_inicial)
  )

#reagrupando os dados
dados_individuo_c <- dados_crescimento %>%
  select(i_arbre, crescimento_anual)

#função do cálculo de crescimento médio
calcula_estatisticas_grupo <- function(data, group_vars) {
  data %>%
    group_by(!!!group_vars) %>%
    summarise(
      crescimento_anual_medio = mean(crescimento_anual, na.rm = TRUE),
      .groups = 'drop'
    )
}
#cálculo das estatisticas para cada solicitação
dados_especie_c <- calcula_estatisticas_grupo(dados_crescimento, vars(gen_especie))
dados_sitio_c <- calcula_estatisticas_grupo(dados_crescimento, vars(Base))
dados_especie_sitio_c <- calcula_estatisticas_grupo(dados_crescimento, vars(gen_especie, Base))

#------------------------------------------------------------------------------------------------------------

#cálculo da média e desvio padrao por árvore
dados_individuo <- dados %>%
  group_by(i_arbre) %>%
  summarise(
    media_dap = mean(diametromm),
    sd_dap = sd(diametromm),
    .groups = 'drop'
  )

#função do cálculo da média e do desvio padrão
calcula_estatisticas_grupo <- function(data, group_vars) {
  data %>%
    group_by(!!!group_vars) %>%
    summarise(
      media_dap = mean(diametromm, na.rm = TRUE),
      sd_dap = sd(diametromm, na.rm = TRUE),
      .groups = 'drop'
    )
}
#cálculo das estatisticas para cada solicitação
dados_especie <- calcula_estatisticas_grupo(dados, vars(gen_especie))
dados_sitio <- calcula_estatisticas_grupo(dados, vars(Base))
dados_especie_sitio <- calcula_estatisticas_grupo(dados, vars(gen_especie, Base))
#concatenar as tabelas e drop das auxiliares
dados_especie <- left_join(dados_especie_c, dados_especie, by = ("gen_especie"))
dados_sitio <- left_join(dados_sitio_c, dados_sitio, by = ("Base"))
dados_especie_sitio <- left_join(dados_especie_sitio_c, dados_especie_sitio, by = c("Base", "gen_especie"))
dados_individuo <- left_join(dados_individuo_c, dados_individuo, by = ("i_arbre"))
rm(dados_individuo_c)
rm(dados_especie_c)
rm(dados_sitio_c)
rm(dados_especie_sitio_c)
rm(dados_crescimento)

#------------------------------------------------------------------------------------------------------------

par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1)

barplot(dados_especie$crescimento_anual_medio, names.arg = dados_especie$gen_especie,
        main = "Crescimento Anual Médio por Espécie",
        xlab = "Espécie", ylab = "Crescimento Anual Médio",
        las = 2, cex.names = 0.6)

barplot(dados_especie$media_dap, names.arg = dados_especie$gen_especie,
        main = "Média de Diâmetro por Espécie",
        xlab = "Espécie", ylab = "Média de Diâmetro",
        las = 2, cex.names = 0.6)

barplot(dados_especie$sd_dap, names.arg = dados_especie$gen_especie,
        main = "Desvio Padrão de Diâmetro por Espécie",
        xlab = "Espécie", ylab = "Desvio Padrão de Diâmetro",
        las = 2, cex.names = 0.6)

boxplot(diametromm ~ gen_especie, data = dados,
        main = "Distribuição de Diâmetro por Espécie",
        xlab = "Espécie", ylab = "Diâmetro",
        las = 2, cex.axis = 0.6, cex.names = 0.6)

#------------------------------------------------------------------------------------------------------------

par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1)

barplot(dados_sitio$crescimento_anual_medio, names.arg = dados_sitio$Base,
        main = "Crescimento Anual Médio por Sítio",
        xlab = "Sítio", ylab = "Crescimento Anual Médio",
        las = 2, cex.names = 0.6)

barplot(dados_sitio$media_dap, names.arg = dados_sitio$Base,
        main = "Média de Diâmetro por Sítio",
        xlab = "Sítio", ylab = "Média de Diâmetro",
        las = 2, cex.names = 0.6)

barplot(dados_sitio$sd_dap, names.arg = dados_sitio$Base,
        main = "Desvio Padrão de Diâmetro por Sítio",
        xlab = "Sítio", ylab = "Desvio Padrão de Diâmetro",
        las = 2, cex.names = 0.6)

boxplot(diametromm ~ Base, data = dados,
        main = "Distribuição de Diâmetro por Sítio",
        xlab = "Sítio", ylab = "Diâmetro",
        las = 2, cex.axis = 0.6, cex.names = 0.6)

