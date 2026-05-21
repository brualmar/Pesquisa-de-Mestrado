# =============================================================
# Script 2a: Calculo do Indice de Favorabilidade a Conservacao
# Projeto: Indice de Favorabilidade a Conservacao de Tubaroes
#          em Surfistas de Florianopolis
# Autor: Bruna Morais
# Data: 2026-05-21
#
# LOGICA DO INDICE (Versao Otimizada - Alpha = 0.60):
# 3 variaveis em escala Likert (1 a 5):
#   - Diretas (escala original):
#        sentimento_floripa (Atitude emocional)
#   - Inversas (escala invertida antes de calcular):
#        impacto_surf (Percepção de conflito)
#        risco_seguranca (Percepção de risco)
#
# Cada variavel e normalizada de 0 a 1.
# O indice final e a media das 3 variaveis normalizadas.
# Indice proximo de 1 = alta favorabilidade a conservacao.
# =============================================================

# 1. PACOTES --------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)

# 2. DIRETORIO ------------------------------------------------
# Definido automaticamente pelo RStudio Project.

# 3. LEITURA DOS DADOS ----------------------------------------
dados <- read_excel("Data/L1/dados_limpos.xlsx")

# 4. CALCULO DO INDICE ----------------------------------------

# Funcao de normalizacao min-max (0 a 1)
normalizar <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Funcao de inversao de escala Likert 1-5
inverter_likert <- function(x) {
  (5 + 1) - x  # transforma 1->5, 2->4, 3->3, 4->2, 5->1
}

dados_indice <- dados |>
  mutate(
    # Inverter variaveis com relacao negativa com favorabilidade
    impacto_surf_inv    = inverter_likert(impacto_surf),
    risco_seguranca_inv = inverter_likert(risco_seguranca),
    
    # Normalizar as 3 variaveis selecionadas para o indice (0 a 1)
    n_sentimento     = normalizar(sentimento_floripa),
    n_impacto        = normalizar(impacto_surf_inv),
    n_risco          = normalizar(risco_seguranca_inv),
    
    # Indice final: media das 3 variaveis estruturais e validadas
    indice_favorabilidade = rowMeans(
      cbind(n_sentimento, n_impacto, n_risco),
      na.rm = TRUE
    )
  )

# Classificar o indice em categorias
dados_indice <- dados_indice |>
  mutate(
    categoria_indice = case_when(
      indice_favorabilidade >= 0.67 ~ "Alta favorabilidade",
      indice_favorabilidade >= 0.34 ~ "Media favorabilidade",
      TRUE                          ~ "Baixa favorabilidade"
    )
  )

# Resumo estatistico do indice
cat("=== RESUMO DO INDICE DE FAVORABILIDADE (3 VARIAVEIS) ===\n")
cat("N de entrevistados:", nrow(dados_indice), "\n")
cat("Media:", round(mean(dados_indice$indice_favorabilidade, na.rm = TRUE), 3), "\n")
cat("Mediana:", round(median(dados_indice$indice_favorabilidade, na.rm = TRUE), 3), "\n")
cat("Desvio padrao:", round(sd(dados_indice$indice_favorabilidade, na.rm = TRUE), 3), "\n")
cat("Min:", round(min(dados_indice$indice_favorabilidade, na.rm = TRUE), 3), "\n")
cat("Max:", round(max(dados_indice$indice_favorabilidade, na.rm = TRUE), 3), "\n\n")
print(table(dados_indice$categoria_indice))

# 5. EXPORTACAO -----------------------------------------------
write_xlsx(dados_indice, "Data/L1/dados_com_indice.xlsx")
message("Indice otimizado calculado e exportado para Data/L1/dados_com_indice.xlsx")
