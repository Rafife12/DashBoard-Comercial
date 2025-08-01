# Pacotes necessários
library(dplyr)
library(lubridate)

# 1. Receita Total
receita_total <- sum(df_vendas$Faturamento_na_Venda, na.rm = TRUE)

# 2. Produtos Vendidos
produtos_vendidos <- sum(df_vendas$Qtd_Devolvida, na.rm = TRUE)
# ou: sum(df_vendas$Quantidade_Vendida, na.rm = TRUE)

# 3. Ticket Médio
ticket_medio <- ifelse(produtos_vendidos != 0,
                       receita_total / produtos_vendidos,
                       0)

# 4. Quantidade de Clientes
qtd_clientes <- n_distinct(df_vendas$Id_Cliente)

# 5. Faturamento por Mês
df_vendas <- df_vendas %>%
  mutate(AnoMes = floor_date(Data_Venda, "month"))

faturamento_mensal <- df_vendas %>%
  group_by(AnoMes) %>%
  summarise(Receita = sum(Faturamento_na_Venda, na.rm = TRUE))

# 6. Receita por Gênero
df_genero <- df_vendas %>%
  left_join(df_clientes, by = "Id_Cliente") %>%
  group_by(Genero) %>%
  summarise(Receita = sum(Faturamento_na_Venda, na.rm = TRUE))

# 7. Receita por Marca
df_marca <- df_vendas %>%
  left_join(df_produtos, by = "Id_Produto") %>%
  group_by(Marca) %>%
  summarise(Receita = sum(Faturamento_na_Venda, na.rm = TRUE))

# 8. Receita por Continente
df_continente <- df_vendas %>%
  left_join(df_lojas, by = "Id_Loja") %>%
  group_by(Continente) %>%
  summarise(Receita = sum(Faturamento_na_Venda, na.rm = TRUE))
