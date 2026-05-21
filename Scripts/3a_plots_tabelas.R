# =============================================================
# Script 3: Visualizacoes e graficos
# Projeto: Indice de Favorabilidade a Conservacao de Tubaroes
#          em Surfistas de Florianopolis
# Autor: Bruna Morais
# Data: 2026-05-21
# =============================================================

# 1. PACOTES --------------------------------------------------
library(dplyr)
library(readxl)
library(ggplot2)

# 2. DIRETORIO ------------------------------------------------
# Definido automaticamente pelo RStudio Project.

# 3. LEITURA DOS DADOS ----------------------------------------
dados <- read_excel("Data/L1/dados_com_indice.xlsx")

# Paleta de cores do projeto
cor_principal <- "#2E5D9E"
cor_alta      <- "#1a7a4a"
cor_media     <- "#e8a020"
cor_baixa     <- "#c0392b"

# 4. GRAFICOS -------------------------------------------------

# --- Grafico 1: Distribuicao do indice (histograma) ----------
p1 <- ggplot(dados, aes(x = indice_favorabilidade)) +
  geom_histogram(binwidth = 0.05, fill = cor_principal,
                 color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean(dados$indice_favorabilidade, na.rm = TRUE),
             linetype = "dashed", color = "gray30", linewidth = 0.8) +
  annotate("text",
           x = mean(dados$indice_favorabilidade, na.rm = TRUE) + 0.03,
           y = Inf, vjust = 1.5,
           label = paste0("Media = ",
                          round(mean(dados$indice_favorabilidade, na.rm = TRUE), 2)),
           size = 3.5, color = "gray30") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Distribuicao do Indice de Favorabilidade",
    subtitle = "Surfistas de Florianopolis",
    x = "Indice de Favorabilidade a Conservacao (0-1)",
    y = "Numero de entrevistados"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Results/01_histograma_indice.png",
       plot = p1, width = 8, height = 5, dpi = 300)


# --- Grafico 2: Categorias de favorabilidade (barras) --------
contagem_cat <- dados |>
  count(categoria_indice) |>
  mutate(
    categoria_indice = factor(
      categoria_indice,
      levels = c("Alta favorabilidade",
                 "Media favorabilidade",
                 "Baixa favorabilidade")
    ),
    pct = round(n / sum(n) * 100, 1)
  )

p2 <- ggplot(contagem_cat,
             aes(x = categoria_indice, y = n,
                 fill = categoria_indice)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "\n(", pct, "%)")),
            vjust = -0.4, size = 4) +
  scale_fill_manual(values = c(
    "Alta favorabilidade"  = cor_alta,
    "Media favorabilidade" = cor_media,
    "Baixa favorabilidade" = cor_baixa
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Categorias de Favorabilidade a Conservacao",
    subtitle = "Surfistas de Florianopolis",
    x = NULL,
    y = "Numero de entrevistados"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Results/02_categorias_favorabilidade.png",
       plot = p2, width = 7, height = 5, dpi = 300)


# --- Grafico 3: Indice por escolaridade (boxplot) ------------
dados_esc <- dados |>
  filter(!is.na(escolaridade)) |>
  mutate(escolaridade = case_when(
    grepl("Fundamental", escolaridade) ~ "Fund.",
    grepl("Medio|Médio", escolaridade) ~ "Medio",
    grepl("Superior Incompleto", escolaridade) ~ "Sup. Inc.",
    grepl("Superior Completo", escolaridade) ~ "Sup. Comp.",
    grepl("Pos|Pós", escolaridade) ~ "Pos-grad.",
    TRUE ~ escolaridade
  ))

p3 <- ggplot(dados_esc,
             aes(x = reorder(escolaridade, indice_favorabilidade,
                             FUN = median),
                 y = indice_favorabilidade)) +
  geom_boxplot(fill = cor_principal, alpha = 0.6,
               outlier.colour = "gray40") +
  geom_jitter(width = 0.15, alpha = 0.4, color = "gray30", size = 1.5) +
  coord_flip() +
  labs(
    title = "Indice de Favorabilidade por Escolaridade",
    subtitle = "Surfistas de Florianopolis",
    x = NULL,
    y = "Indice de Favorabilidade (0-1)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Results/03_indice_por_escolaridade.png",
       plot = p3, width = 8, height = 5, dpi = 300)


# --- Grafico 4: Indice por sentimento sobre encontro ---------
dados_sent <- dados |>
  filter(!is.na(sentimento_encontro))

p4 <- ggplot(dados_sent,
             aes(x = reorder(sentimento_encontro,
                             indice_favorabilidade, FUN = median),
                 y = indice_favorabilidade)) +
  geom_boxplot(fill = cor_principal, alpha = 0.6,
               outlier.colour = "gray40") +
  geom_jitter(width = 0.15, alpha = 0.4, color = "gray30", size = 1.5) +
  coord_flip() +
  labs(
    title = "Indice de Favorabilidade por Sentimento sobre Encontro",
    subtitle = "Surfistas de Florianopolis",
    x = "Sentimento ao encontrar tubarao",
    y = "Indice de Favorabilidade (0-1)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Results/04_indice_por_sentimento.png",
       plot = p4, width = 8, height = 5, dpi = 300)

# 5. EXPORTACAO -----------------------------------------------
message("Todos os graficos exportados para a pasta Results/")