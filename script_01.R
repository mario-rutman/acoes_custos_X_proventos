# 1. Carregando os pacotes. -----------------------------------------------
# Lista de pacotes necessários
pacotes_necessarios <- c(
  "dplyr", "ggplot2", "tidyr", "readxl", "lubridate", "glue", "stringr",
  "styler"
)

# Função para instalar pacotes que ainda não estão instalados
instalar_pacotes <- function(pacotes) {
  # Verifica quais pacotes não estão instalados
  pacotes_nao_instalados <- pacotes[!pacotes %in% installed.packages()[, "Package"]]

  # Se houver pacotes não instalados, instala-os
  if (length(pacotes_nao_instalados) > 0) {
    install.packages(pacotes_nao_instalados)
  }
}

# Instala os pacotes necessários
instalar_pacotes(pacotes_necessarios)

# Carrega os pacotes
lapply(pacotes_necessarios, library, character.only = TRUE)


# 2. Importando o excel, faxinando e salvando em rds.  -------------------------------

custos_X_proventos <- read_excel("data-raw/csv_excel/ações_ custo X proventos.xlsx",
  col_types = c(
    "date", "text", "numeric",
    "numeric", "numeric", "numeric"
  )
) %>%
  # substituindo NA por 0.
  mutate_all(~ replace(., is.na(.), 0)) %>%
  # criando a coluna ano_mes
  mutate(ano_mes = format(data, "%y-%m")) %>%
  # criando a coluna ano
  mutate(ano = format(data, "%Y")) %>%
  # criando as colunas dos acumulados e do percentual dos proventos sobre os custos.
  mutate(
    custos_acumul = cumsum(custo_compra),
    proventos_acumul = cumsum(proventos),
    dividend_yield_percentual = round((proventos_acumul * 100 / custos_acumul), 2)
  )

saveRDS(custos_X_proventos, "data-raw/rds/custos_X_proventos.rds")


# 3. Fazendo o proventos por mês e salvando em rds.  ----------------------------------------

proventos_por_mes <- custos_X_proventos %>%
  group_by(ano_mes) %>%
  summarize(custos_no_mes = sum(custo_compra, na.rm = TRUE),
            proventos_no_mes = sum(proventos, na.rm = TRUE)) %>%
  mutate(ano = paste0("20", str_sub(ano_mes, 1, 2)))

saveRDS(proventos_por_mes, "data-raw/rds/proventos_por_mes.rds")


# 4. Fazendo o gráfico dos proventos por mês. -----------------------------

ggplot(proventos_por_mes, aes(x = ano_mes, y = proventos_no_mes)) +
  geom_point(size = 3, alpha = 1) +
  geom_hline(yintercept = 1000, color = "green3", linewidth = 2.5, alpha = 1) +
  # geom_hline(yintercept = 2000, color = "green3", linewidth=2.5, alpha=1) +
  # geom_hline(yintercept = 3000, color = "green3", linewidth=2.5, alpha=1) +
  # geom_hline(yintercept = 4000, color = "green3", linewidth=2.5, alpha=1) +
  # geom_hline(yintercept = 5000, color = "green3", linewidth=2.5, alpha=1) +
  labs(x = "ano-mes", y = "proventos") +
  theme_bw()



# 5. Fazendo boxplot dos proventos de cada ano. --------

ggplot(proventos_por_mes, aes(x = ano, y = proventos_no_mes)) +
  geom_boxplot(color = "black", size = 1) +
  geom_jitter(width = 0.2, size = 3, color = "#E95420", alpha = 0.6) + # Adiciona pontos individuais
  labs(
    title = "Distribuição dos Proventos por Ano",
    x = "Ano",
    y = "Proventos no Mês (R$)"
  ) +
  theme_bw()


# 6. Fazendo a média mensal nos últimos 12 mesesde custos e proventos. -------------------------

calcular_media_custos_ultimos_12_meses <- function(df) {
    # Verifica se o dataframe tem pelo menos 12 linhas
    if (nrow(df) < 12) {
        stop("O dataframe precisa ter pelo menos 12 linhas para calcular a média dos últimos 12 meses.")
    }
    
    # Seleciona os valores da coluna 'custos_no_mes' dos últimos 12 meses
    ultimos_12_meses <- tail(df$custos_no_mes, 12)
    
    # Calcula a média dos valores selecionados
    media_ultimos_12_meses <- mean(ultimos_12_meses) %>% round(0)
    
    # Formata a média como moeda brasileira
    media_formatada <- glue("R$ {format(media_ultimos_12_meses, decimal.mark = ',', big.mark = '.')}.")
    
    # Retorna a frase com a média formatada.
    return(paste("O gasto médio na compra de ações nos últimos 12 meses foi de", media_formatada))
}

# Exemplo.
calcular_media_custos_ultimos_12_meses(proventos_por_mes)



calcular_media_proventos_ultimos_12_meses <- function(df) {
  # Verifica se o dataframe tem pelo menos 12 linhas
  if (nrow(df) < 12) {
    stop("O dataframe precisa ter pelo menos 12 linhas para calcular a média dos últimos 12 meses.")
  }

  # Seleciona os valores da coluna 'proventos_no_mes' dos últimos 12 meses
  ultimos_12_meses <- tail(df$proventos_no_mes, 12)

  # Calcula a média dos valores selecionados
  media_ultimos_12_meses <- mean(ultimos_12_meses) %>% round(0)

  # Formata a média como moeda brasileira
  media_formatada <- glue("R$ {format(media_ultimos_12_meses, decimal.mark = ',', big.mark = '.')}.")
  
  # Retorna a frase com a média formatada.
  return(paste("O provento médio nos últimos 12 meses foi de", media_formatada))
}

# Exemplo.
calcular_media_proventos_ultimos_12_meses(proventos_por_mes)
