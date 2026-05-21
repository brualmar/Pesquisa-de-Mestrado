# =============================================================
# Script 1a: Limpeza dos dados brutos
# Projeto: Indice de Favorabilidade a Conservacao de Tubaroes
#          em Surfistas de Florianopolis
# Autor: Bruna Morais
# Data: 2026-05-21
# =============================================================

# 1. PACOTES --------------------------------------------------
library(dplyr)
library(readr)
library(writexl)

# 2. DIRETORIO ------------------------------------------------
# Definido automaticamente pelo RStudio Project.
# Todos os caminhos sao relativos a raiz do projeto.

# 3. LEITURA DOS DADOS ----------------------------------------
dados_brutos <- read_delim(
  "Data/L0/Dados_Surf_Fixed_DADOS_GERAIS.csv",
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ","),
  show_col_types = FALSE
)

# Renomear colunas para nomes simples e sem caracteres especiais
colnames(dados_brutos) <- c(
  "nome",
  "idade",
  "escolaridade",
  "tempo_floripa_meses",
  "tempo_floripa_anos",
  "tempo_surfe_total",
  "tempo_surfe_floripa",
  "contato_midia",
  "retrato_midia",
  "aprendeu_escola",
  "palavra_tubarao",
  "viaja_surfar",
  "localidades",
  "avistou_tubarao",
  "sentimento_encontro",
  "sabe_incidente",
  "relato_incidente",
  "acredita_coexistencia",
  "sugestao_conservacao",
  "sentimento_floripa",        # escala numerica
  "importante_ecossistema",    # escala numerica
  "impacto_surf",              # escala numerica (inversa)
  "bem_informado",             # escala numerica (contexto)
  "educacao_riscos",           # escala numerica
  "risco_seguranca",           # escala numerica (inversa)
  "responsabilidade_surfista"  # escala numerica
)

# 4. LIMPEZA DOS DADOS ----------------------------------------

# Remover linhas completamente vazias
dados_limpos <- dados_brutos |>
  filter(!is.na(nome) & nome != "")

# Colunas do indice
colunas_indice <- c(
  "sentimento_floripa",
  "importante_ecossistema",
  "impacto_surf",
  "educacao_riscos",
  "risco_seguranca",
  "responsabilidade_surfista"
)

# Limpar quebras de linha e converter para numerico
dados_limpos <- dados_limpos |>
  mutate(across(all_of(colunas_indice), ~ {
    x <- gsub("[\n\r\\s]", "", .x)
    x <- gsub(",", ".", x)
    as.numeric(x)
  }))

# Remover linhas sem nenhuma resposta nas colunas do indice
dados_limpos <- dados_limpos |>
  filter(rowSums(!is.na(select(dados_limpos, all_of(colunas_indice)))) > 0)

# Padronizar colunas de texto
dados_limpos <- dados_limpos |>
  mutate(
    acredita_coexistencia = tolower(trimws(acredita_coexistencia)),
    avistou_tubarao       = tolower(trimws(avistou_tubarao)),
    aprendeu_escola       = tolower(trimws(aprendeu_escola)),
    sentimento_encontro   = tolower(trimws(sentimento_encontro))
  )

# Verificar NAs nas colunas do indice
cat("=== NAs nas colunas do indice ===\n")
print(
  dados_limpos |>
    select(all_of(colunas_indice)) |>
    summarise(across(everything(), ~sum(is.na(.))))
)

# Verificar resultado
cat("\nLinhas apos limpeza:", nrow(dados_limpos), "\n")
cat("Colunas:", ncol(dados_limpos), "\n")

# 5. EXPORTACAO -----------------------------------------------
write_xlsx(dados_limpos, "Data/L1/dados_limpos.xlsx")
message("Dados limpos exportados para Data/L1/dados_limpos.xlsx")
