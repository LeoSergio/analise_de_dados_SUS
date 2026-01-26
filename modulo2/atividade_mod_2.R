#https://github.com/CampusVirtualFiocruz/curso-analise-de-dados/tree/main
# =============================================================================
# MÓDULO 2 - AULA 2: Formas de Visualização de Dados e Métodos Analíticos
# Atividades Complementares
# Curso: Introdução à Análise de Dados para Pesquisa no SUS
# =============================================================================

# -----------------------------------------------------------------------------
# INSTALAÇÃO E CARREGAMENTO DE PACOTES
# -----------------------------------------------------------------------------
# tidyverse: coleção de pacotes para manipulação e visualização de dados 
# datasauRus: contém datasets que demonstram a importância da visualização
#             (Quarteto de Anscombe expandido)


install.packages("tidyverse") 
install.packages("datasauRus")

library(tidyverse)
library(datasauRus)


# =============================================================================
# ATIVIDADE 1: Quarteto de Anscombe - A importância da visualização
# =============================================================================
# O Quarteto de Anscombe (1973) é um exemplo clássico que demonstra:
# "Nunca confie apenas em estatísticas - SEMPRE visualize seus dados!"
#
# São 4 conjuntos de dados com estatísticas IDÊNTICAS:
# - Mesma média de X e Y
# - Mesmo desvio-padrão de X e Y
# - Mesma correlação
# - Mesma linha de regressão
#
# Porém, os gráficos revelam padrões COMPLETAMENTE DIFERENTES!

# -----------------------------------------------------------------------------
# 1.1 Carregar e explorar o Quarteto de Anscombe
# -----------------------------------------------------------------------------
# O R já possui este dataset embutido (objeto 'anscombe')

anscombe  # Visualizar dados originais (formato wide)

# Reorganizar para formato "tidy" (longo) - melhor para análise com tidyverse
# pivot_longer(): transforma colunas em linhas
# names_pattern: usa regex para separar "x1" em "x" e "1"

anscombe_tidy <- anscombe %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "conjunto"),
    names_pattern = "(.)(.)" #padrão do nome das colunas
  ) %>%
  mutate(conjunto = paste("Conjunto", conjunto))

anscombe_tidy


# -----------------------------------------------------------------------------
# 1.2 Calcular estatísticas descritivas por conjunto
# -----------------------------------------------------------------------------
# Vamos verificar que todos os conjuntos têm estatísticas praticamente iguais

estatisticas_anscombe <- anscombe_tidy %>%
  group_by(conjunto) %>%
  summarise(
    media_x = mean(x),
    media_y = mean(y),
    dp_x = sd(x),
    dp_y = sd(y),
    correlacao = cor(x, y)  # Coeficiente de correlação de Pearson
  )

estatisticas_anscombe
# OBSERVE: Todos têm ~mesma média, desvio-padrão e correlação (~0.82)!


# -----------------------------------------------------------------------------
# 1.3 Visualizar os quatro conjuntos
# -----------------------------------------------------------------------------
# A mágica acontece quando VEMOS os dados!
# facet_wrap(): cria um painel separado para cada conjunto

grafico_anscombe <- anscombe_tidy %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = "steelblue", size = 2, alpha=0.8) +
  geom_smooth(method = "lm", se = FALSE,   # Linha de regressão linear
              color = "red", linewidth = 0.8) +
  facet_wrap(~conjunto, ncol = 2) +
  labs(
    title = "Quarteto de Anscombe",
    subtitle = "Mesmas estatísticas, comportamentos completamente diferentes!",
    x = "X",
    y = "Y"
  ) +
  theme_minimal()

grafico_anscombe
# Conjunto 1: relação linear "normal"
# Conjunto 2: relação não-linear (curva)
# Conjunto 3: linear com um outlier
# Conjunto 4: sem relação real (outlier define a correlação)


# =============================================================================
# ATIVIDADE 2: Datasaurus Dozen - Expansão moderna do conceito
# =============================================================================
# O Datasaurus Dozen (Matejka & Fitzmaurice, 2017) expande o Quarteto de Anscombe
# para 13 conjuntos com estatísticas idênticas - incluindo um dinossauro!

# -----------------------------------------------------------------------------
# 2.1 Explorar os datasets disponíveis
# -----------------------------------------------------------------------------

datasaurus_dozen %>%
  distinct(dataset)  # Listar os 13 datasets


# -----------------------------------------------------------------------------
# 2.2 Calcular estatísticas por dataset
# -----------------------------------------------------------------------------
# Todos terão praticamente os mesmos valores!

estatisticas_datasaurus <- datasaurus_dozen %>%
  group_by(dataset) %>%
  summarise(
    media_x = round(mean(x), 2),
    media_y = round(mean(y), 2),
    dp_x = round(sd(x), 2),
    dp_y = round(sd(y), 2),
    correlacao = round(cor(x, y), 2)
  )

estatisticas_datasaurus
# Todos têm média ~54, dp ~16-27, correlação ~-0.06!


# -----------------------------------------------------------------------------
# 2.3 Visualizar todos os conjuntos
# -----------------------------------------------------------------------------

grafico_datasaurus <- datasaurus_dozen %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.6, size = 1, color = "steelblue") +
  facet_wrap(~dataset, ncol = 4) +
  labs(
    title = "Datasaurus Dozen",
    subtitle = "Todos os conjuntos têm as mesmas estatísticas descritivas!",
    x = "X",
    y = "Y"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

grafico_datasaurus


# -----------------------------------------------------------------------------
# 2.4 O dinossauro escondido nos dados
# -----------------------------------------------------------------------------
# O dataset "dino" é o mais famoso - um T-Rex completo!

dino <- datasaurus_dozen %>% filter(dataset == "dino") ##use filter para selecionar apenas o dataset dino

grafico_dino <- dino %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "O Dinossauro escondido nos dados",
    subtitle = paste0("Média X: ", round(mean(dino$x), 2), 
                      " | Média Y: ", round(mean(dino$y), 2),
                      " | Correlação: ", round(cor(dino$x, dino$y), 2)),
    x = "X",
    y = "Y"
  ) +
  theme_minimal() +
  coord_fixed()  # Manter proporção 1:1 nos eixos

grafico_dino


# =============================================================================
# ATIVIDADE 3: Boas Práticas na Visualização de Dados
# =============================================================================
# Dados do IPCA (exemplo da aula sobre gráficos enganosos)
ipca <- tibble(
  ano = 2009:2013,
  valor = c(4.31, 5.92, 6.5, 5.84, 5.91)
)
# -----------------------------------------------------------------------------
# 3.1 Gráfico CORRETO
# -----------------------------------------------------------------------------
# REGRA: Em gráficos de barras, o eixo Y SEMPRE deve começar em zero!
# Isso permite comparação proporcional correta.

grafico_ipca_correto <- ipca %>%
  ggplot(aes(x = factor(ano), y = valor)) +
  geom_col(fill = "gray70") +
  geom_hline(yintercept = 4.5, linetype = "dashed", color = "red") +
  annotate("text", x = 5.4, y = 4.7, label = "Meta: 4.5%", size = 3) +
  labs(
    title = "IPCA no Brasil - EXEMPLO CORRETO",
    subtitle = "Eixo Y iniciando em zero permite comparação proporcional",
    x = "Ano",
    y = "IPCA (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

grafico_ipca_correto
# Agora vemos que as diferenças são relativamente pequenas


# =============================================================================
# ATIVIDADE 4: Uso Adequado de Cores
# =============================================================================
# Cores devem ter PROPÓSITO: distinguir grupos, destacar padrões ou indicar valores
# Cores aleatórias ou excessivas PREJUDICAM a interpretação

# Dados de eventos cardiovasculares após dengue (simplificados da aula)
# IRR = Incidence Rate Ratio (valores > 1 indicam aumento de risco)
eventos_dengue <- tibble(
  desfecho = rep(c("AVC hemorrágico", "AVC isquêmico", 
                    "Infarto agudo", "Insuficiência cardíaca"), 2),
  periodo = rep(c("Dias 1-7", "Dias 8-14"), each = 4),
  IRR = c(10.90, 15.56, 13.53, 27.24,   # Risco muito elevado logo após infecção
          4.33, 3.17, 1.16, 2.45)        # Risco diminui com o tempo
)


# -----------------------------------------------------------------------------
# 4.1 Gráfico com cores que têm propósito
# -----------------------------------------------------------------------------
# Usamos vermelho para período mais crítico (Dias 1-7) e azul para o posterior

grafico_dengue <- eventos_dengue %>%
  mutate(desfecho = fct_reorder(desfecho, IRR, .fun = max, .desc = TRUE)) %>%
  ggplot(aes(x = desfecho, y = IRR, color = periodo)) +
  geom_point(position = position_dodge(width = .5),size=3) +  # pontos lado a lado
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_fill_manual(
    values = c("Dias 1-7" = "#E74C3C", "Dias 8-14" = "#3498DB"),
    name = "Período após infecção"
  ) +
  labs(
    title = "Razão da taxa de incidência por período após dengue",
    subtitle = "Linha tracejada indica IRR = 1 (sem aumento de risco)",
    x = "",
    y = "IRR (Razão da Taxa de Incidência)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.position = "bottom"
  )

grafico_dengue


# -----------------------------------------------------------------------------
# 4.2 Cores sequenciais para variáveis contínuas
# -----------------------------------------------------------------------------
# Gradientes de cor são ideais para representar intensidade/magnitude

consumo_montadoras <- mtcars %>%
  rownames_to_column("modelo") %>%
  mutate(montadora = word(modelo, 1)) %>%
  group_by(montadora) %>%
  summarise(mpg_medio = mean(mpg)) %>%
  arrange(mpg_medio) %>%
  head(10)

grafico_consumo <- consumo_montadoras %>%
  mutate(montadora = fct_reorder(montadora, mpg_medio)) %>%
  ggplot(aes(x = montadora, y = mpg_medio, fill = mpg_medio)) +
  geom_col() +
  scale_fill_gradient(low = "#2C3E50", high = "#3498DB", guide = "none") +
  coord_flip() +
  labs(
    title = "Consumo médio por montadora",
    subtitle = "Cores sequenciais destacam a diferença entre grupos",
    x = "",
    y = "Milhas por galão (média)"
  ) +
  theme_minimal()

grafico_consumo


# =============================================================================
# ATIVIDADE 5: Tipos de Gráficos Adequados
# =============================================================================
# Cada tipo de gráfico é adequado para um tipo específico de dado e objetivo.

# -----------------------------------------------------------------------------
# 5.1 Gráfico de pizza vs alternativas
# -----------------------------------------------------------------------------
# Gráficos de pizza são PROBLEMÁTICOS porque humanos são ruins em comparar ângulos
# REGRA: Se usar pizza, SEMPRE inclua os valores numéricos

dados_grupos <- tibble(
  grupo = c("A", "B", "C"),
  n = c(45, 35, 20)
)

# Pizza (evitar quando possível)
grafico_pizza <- dados_grupos %>%
  ggplot(aes(x = "", y = n, fill = grupo)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(n, "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Gráfico de Pizza",
       subtitle = "Sempre incluir as porcentagens!") +
  theme_void() +
  theme(legend.position = "bottom")

grafico_pizza

# ALTERNATIVA MELHOR: Barras horizontais
grafico_barras_h <- dados_grupos %>%
  mutate(grupo = fct_reorder(grupo, n)) %>%
  ggplot(aes(x = grupo, y = n, fill = grupo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "%")), hjust = -0.2, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Alternativa ao gráfico de pizza",
       subtitle = "Barras horizontais facilitam a comparação",
       x = "", y = "Percentual (%)") +
  theme_minimal() +
  ylim(0, 55)

grafico_barras_h


# -----------------------------------------------------------------------------
# 5.2 Série temporal - gráfico de linha
# -----------------------------------------------------------------------------
# Para dados ao longo do TEMPO, gráficos de linha são ideais.
# A linha conecta os pontos e facilita ver tendências e sazonalidade.

set.seed(42)
serie_influenza <- tibble(
  semana = 1:52,
  ano = 2023,
  casos = round(50 + 30 * sin((semana - 10) * 2 * pi / 52) + rnorm(52, 0, 10))
) %>%
  mutate(casos = pmax(casos, 5))

grafico_serie <- serie_influenza %>%
  ggplot(aes(x = semana, y = casos)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 1.5) +
  labs(
    title = "Casos de Influenza por semana epidemiológica - 2023",
    subtitle = "Gráfico de linha é ideal para séries temporais",
    x = "Semana epidemiológica",
    y = "Número de casos"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 52, by = 4))+
  scale_y_continuous(limits = c(0,NA)) #sempre incluir o 0

grafico_serie


# =============================================================================
# ATIVIDADE 6: Contextualização Completa dos Gráficos
# =============================================================================
# Um bom gráfico deve ser AUTO-EXPLICATIVO, contendo:
# - Título claro e informativo
# - Subtítulo com contexto adicional
# - Rótulos dos eixos COM UNIDADES
# - Legenda clara (quando aplicável)
# - Fonte dos dados (caption)

recem_nascidos <- tibble(
  individuo = 1:20,
  peso_g = c(3265, 3260, 3245, 3484, 4146, 3323, 3649, 3200, 3031, 2069,
             2581, 2841, 3609, 2838, 3541, 2759, 3248, 3314, 3101, 2834)
)

grafico_completo <- recem_nascidos %>%
  ggplot(aes(x = peso_g)) +
  geom_histogram(bins = 8, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(peso_g)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(recem_nascidos$peso_g) + 10, y = 4.5,
           label = paste0("Média: ", round(mean(recem_nascidos$peso_g), 0), " g"),
           hjust = 0, color = "black", size = 3.5) +
  labs(
    title = "Distribuição do peso ao nascer",
    subtitle = "Amostra de 20 recém-nascidos de uma maternidade de São Paulo",
    x = "Peso ao nascer (gramas)",
    y = "Frequência (n)",
    caption = "Fonte: Dados simulados para fins didáticos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray60")
  )

grafico_completo


# =============================================================================
# FIM DO SCRIPT - AULA 2
# =============================================================================
# Consulte o arquivo modulo2aula2_atividades.pdf para os exercícios propostos.
#
# RECURSOS PARA APROFUNDAMENTO:
# - Escolha de gráficos: datavizproject.com, datavizcatalogue.com
# - Tutoriais ggplot2: r-charts.com, r-graph-gallery.com
# - Paletas acessíveis: davidmathlogic.com/colorblind/
# - Livro gratuito: clauswilke.com/dataviz/