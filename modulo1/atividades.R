# ==============================================================================
# QUESTÃO 1: CLASSIFICAÇÃO ETÁRIA (Solução do exercício)
# ==============================================================================

df_csv <- read_csv("sim_salvador_2023_processado.csv")

# 1.1 CRIAÇÃO DA VARIÁVEL FAIXA ETÁRIA
# Usamos mutate para criar e case_when para definir as regras lógicas
df_csv <- df_csv %>%
  mutate(
    faixa_etaria = case_when(
      idade_anos <= 12 ~ "Criança",             # 0 a 12 anos
      idade_anos >= 13 & idade_anos <= 17 ~ "Adolescente", # 13 a 17 anos
      idade_anos >= 18 & idade_anos <= 59 ~ "Adulto",      # 18 a 59 anos
      idade_anos >= 60 ~ "Idoso",               # 60 anos ou mais
      TRUE ~ "Idade Ignorada"                   # Captura casos onde idade_anos é NA (vazio)
    )
  )

# Dica: Vamos transformar em "fator" para garantir que os gráficos
# sigam a ordem lógica (Criança -> Idoso) e não alfabética.
df_csv <- df_csv %>% 
  mutate(
    faixa_etaria = factor(faixa_etaria, 
                          levels = c("Criança", "Adolescente", "Adulto", "Idoso", "Idade Ignorada"))
  )

# 1.2 CONTAGEM DE ÓBITOS POR FAIXA ETÁRIA
# O count() já agrupa e conta automaticamente
resumo_faixa_etaria <- df_csv %>%
  count(faixa_etaria, name = "total_obitos") %>% # 'name' define o nome da coluna de contagem
  mutate(percentual = round((total_obitos / sum(total_obitos)) * 100, 1)) # BÔNUS: Adiciona %

# Exibir o resultado no console
print(resumo_faixa_etaria)

# ------------------------------------------------------------------------------
# BÔNUS: VISUALIZAÇÃO GRÁFICA RÁPIDA
# ------------------------------------------------------------------------------
ggplot(resumo_faixa_etaria, aes(x = faixa_etaria, y = total_obitos)) +
  geom_col(fill = "#2c3e50") +
  geom_text(aes(label = total_obitos), vjust = -0.5) + # Coloca o número em cima da barra
  labs(
    title = "Óbitos por Faixa Etária - Salvador 2023",
    x = "Ciclo de Vida",
    y = "Total de Óbitos"
  ) +
  theme_minimal()



