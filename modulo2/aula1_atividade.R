# =============================================================================
# MÓDULO 2 - AULA 1: Análise Exploratória e Descritiva
# Atividades Complementares
# Curso: Introdução à Análise de Dados para Pesquisa no SUS
# =============================================================================

# -----------------------------------------------------------------------------
# CONFIGURAÇÃO INICIAL
# -----------------------------------------------------------------------------
# Defina o diretório de trabalho para a pasta onde estão os arquivos do curso.
# Substitua o caminho abaixo pelo caminho correto no seu computador.

# setwd("C:/caminho/para/pasta/do/curso")  # Windows
# setwd("/home/usuario/pasta/do/curso")    # Linux/Mac

# Verificar o diretório atual
getwd()
# -----------------------------------------------------------------------------
# INSTALAÇÃO E CARREGAMENTO DE PACOTES
# -----------------------------------------------------------------------------
# O tidyverse é uma coleção de pacotes para ciência de dados que inclui:
# - dplyr: manipulação de dados (filter, select, mutate, summarise, group_by)
# - ggplot2: criação de gráficos
# - tidyr: organização de dados
# - readr: importação de dados

install.packages("tidyverse")
library(tidyverse)


# =============================================================================
# ATIVIDADE 1: Trabalhando com Variáveis Aleatórias
# =============================================================================
# Variáveis aleatórias são medições de características de interesse em um estudo.
# Elas podem ser classificadas como:
# - QUALITATIVAS: descrevem atributos (nominal ou ordinal)
# - QUANTITATIVAS: representam quantidades (discreta ou contínua)

# -----------------------------------------------------------------------------
# 1.1 Criando o dataset dos recém-nascidos (dados utilizados na aula)
# -----------------------------------------------------------------------------
# A função tibble() cria um data frame moderno com melhor visualização.
# Estamos criando dados de 20 recém-nascidos com seus pesos em gramas.

recem_nascidos <- tibble(
  individuo = 1:20,
  peso_g = c(3265, 3260, 3245, 3484, 4146, 3323, 3649, 3200, 3031, 2069,
             2581, 2841, 3609, 2838, 3541, 2759, 3248, 3314, 3101, 2834)
)

# Visualizar estrutura dos dados
# glimpse() mostra: nº de linhas, colunas, tipos e primeiros valores
glimpse(recem_nascidos)

View(recem_nascidos)

# -----------------------------------------------------------------------------
# 1.2 Criando variáveis categóricas a partir de variáveis contínuas
# -----------------------------------------------------------------------------
# É comum categorizar variáveis quantitativas para facilitar análises.
# Aqui transformamos o peso (contínuo) em faixas (ordinal).
#
# mutate(): adiciona ou modifica colunas
# case_when(): aplica condições múltiplas (como if-else encadeado)
# factor(): converte para fator com níveis ordenados

recem_nascidos <- recem_nascidos %>% 
  mutate(
    # Criar faixas de peso baseadas em critérios clínicos
    faixa_peso = case_when(
      peso_g < 2500 ~ "Baixo peso",
      peso_g >= 2500 & peso_g < 3500 ~ "Peso adequado",
      peso_g >= 3500 ~ "Acima do esperado"
    ),
    # Converter para fator ordenado (define a ordem lógica das categorias)
    faixa_peso = factor(faixa_peso, 
                        levels = c("Baixo peso", "Peso adequado", "Acima do esperado"),
                        ordered = TRUE)
  )

# Verificar a distribuição das faixas
# count(): conta frequência de cada categoria
recem_nascidos %>% count(faixa_peso)


# =============================================================================
# ATIVIDADE 2: Medidas de Locação (Tendência Central)
# =============================================================================
# Medidas de locação indicam onde os dados estão "centralizados".
# São fundamentais para resumir e comparar conjuntos de dados.

# -----------------------------------------------------------------------------
# 2.1 Média aritmética
# -----------------------------------------------------------------------------
# A média é a soma de todos os valores dividida pelo número de observações.
# Fórmula: x̄ = Σxi / n
# ATENÇÃO: A média é sensível a valores extremos (outliers)!

media_peso <- recem_nascidos %>% 
  summarise(media = mean(peso_g)) %>%   # summarise() calcula estatísticas
  pull(media)                            # pull() extrai o valor como número

media_peso  # Exibir resultado: 3166.9g


# -----------------------------------------------------------------------------
# 2.2 Mediana
# -----------------------------------------------------------------------------
# A mediana é o valor central dos dados ordenados.
# - 50% dos valores estão abaixo e 50% acima da mediana
# - VANTAGEM: É robusta a valores extremos (outliers)

mediana_peso <- recem_nascidos %>%
  summarise(mediana = median(peso_g)) %>%
  pull(mediana)

mediana_peso  # Exibir resultado: 3246.5g


# -----------------------------------------------------------------------------
# 2.3 Comparação entre média e mediana
# -----------------------------------------------------------------------------
# A relação entre média e mediana indica a forma da distribuição:
# - média > mediana: assimetria positiva (cauda à direita)
# - média < mediana: assimetria negativa (cauda à esquerda)
# - média ≈ mediana: distribuição aproximadamente simétrica

diferenca <- media_peso - mediana_peso
diferenca  # Resultado negativo indica leve assimetria negativa


# -----------------------------------------------------------------------------
# 2.4 Quantis: percentis e quartis
# -----------------------------------------------------------------------------
# Quantis dividem os dados ordenados em partes iguais:
# - Quartis (Q): dividem em 4 partes (Q1=25%, Q2=50%=mediana, Q3=75%)
# - Percentis (P): dividem em 100 partes (P10=10%, P90=90%, etc.)
#
# quantile(x, probs): calcula o quantil na probabilidade especificada

quantis_peso <- recem_nascidos %>%
  summarise(
    P10 = quantile(peso_g, 0.10),  # 10% dos dados estão abaixo deste valor
    Q1 = quantile(peso_g, 0.25),   # Primeiro quartil (25%)
    Q2 = quantile(peso_g, 0.50),   # Segundo quartil = mediana (50%)
    Q3 = quantile(peso_g, 0.75),   # Terceiro quartil (75%)
    P90 = quantile(peso_g, 0.90)   # 90% dos dados estão abaixo deste valor
  )

quantis_peso


# -----------------------------------------------------------------------------
# 2.5 Média aritmética ponderada
# -----------------------------------------------------------------------------
# Na média ponderada, cada valor tem um "peso" diferente.
# Fórmula: x̄w = Σ(wi × xi) / Σwi
# Útil quando alguns valores são mais representativos que outros.

# Adicionar pesos (w) aos dados conforme exemplo da aula
recem_nascidos_ponderado <- recem_nascidos %>%
  mutate(
    w = c(22, 40, 33, 22, 5, 31, 24, 35, 48, 58, 
          61, 20, 45, 22, 41, 35, 36, 11, 10, 25)
  )

# Calcular média ponderada manualmente
media_ponderada <- recem_nascidos_ponderado %>%
  summarise(
    media_ponderada = sum(peso_g * w) / sum(w)
  ) %>%
  pull(media_ponderada)

media_ponderada  # Resultado: 3062.28g

# Comparar com média simples
media_peso - media_ponderada  # Diferença de ~104.6g


# =============================================================================
# ATIVIDADE 3: Medidas de Dispersão
# =============================================================================
# Medidas de dispersão indicam o "espalhamento" dos dados.
# Complementam as medidas de locação para descrição completa.

# Criar dataset de colesterol com dois métodos de medição (dados da aula)
# Este exemplo ilustra como métodos diferentes podem ter variabilidades distintas
colesterol <- tibble(
  metodo = rep(c("AutoAnalyzer", "Microenzimatic"), each = 5),
  valor = c(177, 193, 195, 209, 226,  # AutoAnalyzer (mais disperso)
            192, 197, 200, 202, 209)   # Microenzimatic (mais concentrado)
)


# -----------------------------------------------------------------------------
# 3.1 Amplitude
# -----------------------------------------------------------------------------
# Amplitude = valor máximo - valor mínimo
# É a medida mais simples, mas muito sensível a outliers.

amplitude <- colesterol %>%
  group_by(metodo) %>%  # Calcular para cada método separadamente
  summarise(
    minimo = min(valor),
    maximo = max(valor),
    amplitude = max(valor) - min(valor)
  )

amplitude


# -----------------------------------------------------------------------------
# 3.2 Variância e Desvio-padrão
# -----------------------------------------------------------------------------
# VARIÂNCIA: média dos quadrados dos desvios em relação à média
# Fórmula: s² = Σ(xi - x̄)² / (n-1)
#
# DESVIO-PADRÃO: raiz quadrada da variância
# Fórmula: s = √s²
# Vantagem: está na mesma unidade dos dados originais

dispersao <- colesterol %>%
  group_by(metodo) %>%
  summarise(
    media = mean(valor),
    variancia = var(valor),    # var() calcula variância amostral
    desvio_padrao = sd(valor)  # sd() calcula desvio-padrão
  )

dispersao
# Note: AutoAnalyzer tem maior variância (340) vs Microenzimatic (39.5)


# -----------------------------------------------------------------------------
# 3.3 Coeficiente de Variação (CV)
# -----------------------------------------------------------------------------
# O CV expressa o desvio-padrão como percentual da média.
# Fórmula: CV = (s / x̄) × 100
# VANTAGEM: Permite comparar dispersão entre variáveis de escalas diferentes
# INTERPRETAÇÃO: Quanto MENOR o CV, mais homogêneos são os dados

cv <- dispersao %>%
  mutate(
    cv_percentual = (desvio_padrao / media) * 100
  ) %>%
  select(metodo, cv_percentual)

cv
# AutoAnalyzer: 9.22% vs Microenzimatic: 3.14%
# O método Microenzimatic é mais preciso (menor variabilidade relativa)


# -----------------------------------------------------------------------------
# 3.4 Intervalo Interquartil (IQ ou IQR)
# -----------------------------------------------------------------------------
# IQ = Q3 - Q1 (diferença entre terceiro e primeiro quartis)
# Representa a amplitude dos 50% centrais dos dados.
# VANTAGEM: Mais robusto que amplitude total (ignora os 25% extremos)

iq <- colesterol %>%
  group_by(metodo) %>%
  summarise(
    Q1 = quantile(valor, 0.25),
    Q3 = quantile(valor, 0.75),
    IQ = Q3 - Q1
  )

iq


# =============================================================================
# ATIVIDADE 4: Função para Resumo Estatístico Completo
# =============================================================================
# Criar funções personalizadas ajuda a padronizar análises e evitar repetição.
# A sintaxe {{ variavel }} permite passar nomes de colunas como argumentos.

resumo_estatistico <- function(dados, variavel) {
  dados %>% 
    summarise(
      n = n(),                                                    # Contagem
      media = mean({{ variavel }}),                               # Média
      mediana = median({{ variavel }}),                           # Mediana
      desvio_padrao = sd({{ variavel }}),                         # Desvio-padrão
      cv_percent = (sd({{ variavel }}) / mean({{ variavel }})) * 100,  # CV%
      minimo = min({{ variavel }}),                               # Mínimo
      Q1 = quantile({{ variavel }}, 0.25),                        # 1º quartil
      Q3 = quantile({{ variavel }}, 0.75),                        # 3º quartil
      maximo = max({{ variavel }}),                               # Máximo
      IQ = Q3 - Q1                                                # Intervalo IQ
    )
}

# Aplicar a função aos dados dos recém-nascidos
resumo_estatistico(recem_nascidos, peso_g)


# =============================================================================
# ATIVIDADE 5: Métodos Gráficos
# =============================================================================
# Gráficos são essenciais para visualizar padrões que números não revelam.
# O ggplot2 usa a "gramática dos gráficos" - construção em camadas.

# -----------------------------------------------------------------------------
# 5.1 Gráfico de barras - para variáveis categóricas
# -----------------------------------------------------------------------------
# Ideal para mostrar frequências ou proporções de categorias.
# geom_col(): cria barras com alturas especificadas pelos dados

grafico_barras <- recem_nascidos %>%
  count(faixa_peso) %>%                     # Contar frequência por faixa
  mutate(percentual = n / sum(n) * 100) %>% # Calcular percentual
  ggplot(aes(x = faixa_peso, y = percentual)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentual, 1), "%")), 
            vjust = -0.5, size = 4) +       # Adicionar rótulos
  labs(
    title = "Distribuição dos recém-nascidos por faixa de peso",
    x = "Faixa de peso",
    y = "Percentual (%)"
  ) +
  theme_minimal() +
  ylim(0, 100)

grafico_barras


# -----------------------------------------------------------------------------
# 5.2 Box plot - para comparar distribuições entre grupos
# -----------------------------------------------------------------------------
# O box plot mostra simultaneamente:
# - Mediana (linha central)
# - Quartis Q1 e Q3 (limites da caixa)
# - Valores extremos (bigodes)
# - Outliers (pontos isolados)

grafico_boxplot <- colesterol %>% 
  ggplot(aes(x = metodo, y = valor, fill = metodo)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Comparação dos métodos de medição de colesterol",
    x = "Método",
    y = "Colesterol (mg/dL)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "coral"))

grafico_boxplot


# -----------------------------------------------------------------------------
# 5.3 Histograma - para distribuição de variáveis contínuas
# -----------------------------------------------------------------------------
# O histograma divide os dados em intervalos (bins) e mostra a frequência.
# Permite visualizar a forma da distribuição (simétrica, assimétrica, etc.)

grafico_histograma <- recem_nascidos %>%
  ggplot(aes(x = peso_g)) +
  geom_histogram(bins = 6, fill = "steelblue", color = "white") +
  # Linha vertical para a média (vermelha tracejada)
  geom_vline(aes(xintercept = mean(peso_g)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  # Linha vertical para a mediana (verde pontilhada)
  geom_vline(aes(xintercept = median(peso_g)), 
             color = "darkgreen", linetype = "dotted", linewidth = 1) +
  labs(
    title = "Distribuição dos pesos dos recém-nascidos",
    subtitle = "Linha vermelha = média | Linha verde = mediana",
    x = "Peso (g)",
    y = "Frequência"
  ) +
  theme_minimal()

grafico_histograma
