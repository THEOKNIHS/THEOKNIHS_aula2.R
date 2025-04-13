# AULA 7 

#  A PARTIR DA VARIÁVEL ESCOLHIDA NAS OUTRAS AULAS,
# REFAÇA OS TRÊS GRÁFICOS:
# 1) DADOS EM PAINEL 
# 2) SÉRIE TEMPORAL
# 3) CORTE TRANSVERSAL

library(WDI) # CARREGAR BIBLIOTECA/PACOTE 

options(scipen = 999) # REMOVER A NOT. CIENT.
# DADOS EM PAINEL
dados_redesdesegurança <- WDI(country = 'all',
                indicator = 'per_sa_allsa.adq_pop_tot')

segurançabrusa <- WDI(country = 'all',
                      indicator = 'per_sa_allsa.adq_pop_tot')

grafpainel <- ggplot(dados_redesdesegurança,
                     mapping = aes(y = per_sa_allsa.adq_pop_tot,
                                   x = year)) +
  geom_point()

print(grafpainel)

library(WDI)
library(ggplot2)
library(dplyr)
library(tidyr)

# Indicadores para adequação e cobertura
indicadores <- c(
  "Adequação (%)" = "per_sa_allsa.adq_pop_tot",
  "Cobertura (%)" = "per_sa_allsa.cov_pop_tot"
)

# Países de interesse
paises <- c("BR", "US")
dados <- WDI(country = paises,
             indicator = indicadores,
             extra = FALSE)

# Formatar para formato longo
dados_long <- dados %>%
  pivot_longer(cols = starts_with("per_"), 
               names_to = "Indicador",
               values_to = "Valor") %>%
  mutate(
    Indicador = recode(Indicador,
                       "per_sa_allsa.adq_pop_tot" = "Adequação (%)",
                       "per_sa_allsa.cov_pop_tot" = "Cobertura (%)")
  )

# Gráfico com bolinhas coloridas
grafico_bolinhas <- ggplot(dados_long,
                           aes(x = year,
                               y = Valor,
                               fill = country,
                               shape = Indicador)) +
  geom_point(size = 4, color = "black", stroke = 0.8) +
  scale_fill_manual(
    values = c("BR" = "#1E90FF", "US" = "#FF6347"),
    labels = c("Brasil", "Estados Unidos")
  ) +
  scale_shape_manual(
    values = c("Adequação (%)" = 21, "Cobertura (%)" = 24)
  ) +
  labs(
    title = "Adequação vs. Cobertura dos Programas de Rede de Segurança Social",
    subtitle = "Comparação entre Brasil e Estados Unidos",
    x = "Ano",
    y = "Valor (%)",
    fill = "País",
    shape = "Indicador"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#222222"),
    plot.subtitle = element_text(size = 14, color = "#444444"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "#555555"),
    legend.position = "top",
    panel.grid.major = element_line(color = "gray90")
  )

print(grafico_bolinhas)

# CORTE TRANSVERSAL

segurançabrusaCORTE <- WDI(country = 'all',
                    indicator = 'per_sa_allsa.adq_pop_tot',
                    start = 2023, end = 2023)


GRAFCORTE <- ggplot(segurançabrusaCORTE,
                    mapping = aes(y = 'per_sa_allsa.adq_pop_tot',
                                  x = year)) + 
  geom_point()

# CAHTGPT

library(WDI)
library(ggplot2)
library(dplyr)

# Baixar dados de 2023
segurançabrusaCORTE <- WDI(country = "all",
                           indicator = "per_sa_allsa.adq_pop_tot",
                           start = 2023,
                           end = 2023)

# Filtrar países com dados disponíveis e remover regiões agregadas
segurançabrusaCORTE <- segurançabrusaCORTE %>%
  filter(!is.na(per_sa_allsa.adq_pop_tot), !region %in% "Aggregates")

# Ordenar países por valor para gráfico
segurançabrusaCORTE <- segurançabrusaCORTE %>%
  arrange(desc(per_sa_allsa.adq_pop_tot))

# Gráfico estilizado
GRAFCORTE <- ggplot(segurançabrusaCORTE,
                    aes(x = reorder(country, per_sa_allsa.adq_pop_tot),
                        y = per_sa_allsa.adq_pop_tot,
                        fill = per_sa_allsa.adq_pop_tot)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +  # vira o gráfico pra facilitar leitura dos países
  scale_fill_gradient(low = "#FFA07A", high = "#0066CC") +
  labs(
    title = "Adequação dos Programas de Rede de Segurança Social em 2023",
    x = "País",
    y = "Adequação dos programas de rede de segurança social (%)",
    fill = "Adequação (%)"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#222222"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "#333333"),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90")
  )

print(GRAFCORTE)

# SÉRIE TEMPORAL

dados_redesdesegurança <- WDI(country = 'all',
                              indicator = 'per_sa_allsa.adq_pop_tot')

grafpainel <- ggplot(dados_redesdesegurança,
                     mapping = aes(y = per_sa_allsa.adq_pop_tot,
                                   x = year)) +
  geom_point()

print(grafpainel) 

library(WDI)
library(ggplot2)
library(dplyr)

# Coleta de dados
dados_redesdesegurança <- WDI(country = 'all',
                              indicator = 'per_sa_allsa.adq_pop_tot')

# Cria um novo dataframe apenas com dados do Brasil
dados_brasil <- dados_redesdesegurança %>% 
  filter(country == "Brazil")

# Gráfico modificado
grafpainel <- ggplot(dados_redesdesegurança,
                     aes(x = year, y = per_sa_allsa.adq_pop_tot)) +
  geom_point(alpha = 0.4, color = "#2C3E50") +  # Pontos em cinza azulado moderno
  geom_line(data = dados_brasil, aes(x = year, y = per_sa_allsa.adq_pop_tot),
            color = "red", size = 1.2) +         # Linha do Brasil em vermelho
  geom_point(data = dados_brasil, aes(x = year, y = per_sa_allsa.adq_pop_tot),
             color = "red", size = 2) +          # Pontos do Brasil em vermelho
  labs(title = "Cobertura de Programas de Segurança Social ao Longo dos Anos",
       x = "Ano",
       y = "Segurança") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#34495E"),
    axis.title = element_text(size = 13, face = "bold", color = "#2C3E50"),
    axis.text = element_text(color = "#2C3E50")
  )

# Exibir gráfico
print(grafpainel)

# CORTE TRANSVERSAL

dados_redesdesegurança23 <- WDI(country = 'all',
                    indicator = 'per_sa_allsa.adq_pop_tot',
                    start = 2018, end = 2018) 

grafcorte <- ggplot(dados_redesdesegurança23,
                    mapping = aes(y = per_sa_allsa.adq_pop_tot,
                                  x = year)) + 
  geom_point()

print(grafcorte)

library(WDI)
library(ggplot2)
library(dplyr)

# Coleta de dados para 2018
dados_redesdesegurança23 <- WDI(country = 'all',
                                indicator = 'per_sa_allsa.adq_pop_tot',
                                start = 2018, end = 2018)

# Filtra os dados do Brasil
dados_brasil_2018 <- dados_redesdesegurança23 %>%
  filter(country == "Brazil")

# Gráfico atualizado
grafcorte <- ggplot(dados_redesdesegurança23,
                    aes(x = year, y = per_sa_allsa.adq_pop_tot)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "#2C3E50") +  # Pontos com leve dispersão horizontal
  geom_point(data = dados_brasil_2018,
             aes(x = year, y = per_sa_allsa.adq_pop_tot),
             color = "red", size = 3) +  # Ponto do Brasil destacado
  labs(title = "Cobertura de Segurança Social em 2018",
       x = "Ano",
       y = "Segurança 2018") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#34495E"),
    axis.title = element_text(size = 13, face = "bold", color = "#2C3E50"),
    axis.text = element_text(color = "#2C3E50")
  )

# Exibir o gráfico
print(grafcorte)

