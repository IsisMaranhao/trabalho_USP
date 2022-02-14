# TRABALHO PARA O CURSO - BASE DE DADOS PIXAR:

#install.packages("remotes")
#remotes::install_github("cienciadedatos/dados")

install.packages("ggplot2")
install.packages("dplyr")
install.packages("hms")
install.packages("kableExtra", dependencies = TRUE)
install.packages("tidyverse")
library(ggplot2)
library(dados)
library(dplyr)
library(kableExtra)
library(tidyverse)


# academy - Filmes da Pixar com indicações ao Oscar
# box_office - Bilheteria dos filmes da Pixar
# genres - Gêneros dos filmes da Pixar
# pixar_films - Filmes da Pixarl
# pixar_people - Equipe dos filmes
# box_office - Filmes da Pixar e avaliações

# Ou você usa library(dados) e pode oscar <- pixar_oscars para receber os dados
# ou então, usa oscar <- dados::pixar_oscars sem precisar chamar o library antes.

oscar <- dados::pixar_oscars
bilheteria <- dados::pixar_bilheteria
avaliacao <- dados::pixar_avalicao_publico
#generos <- dados::pixar_generos
#filmes <- dados::pixar_filmes
#equipe <- dados::pixar_equipe


# Contando dados de um banco de dados:

# Fazendo tabelas:

quant_oscar <- oscar |>
  dplyr::count(filme, sort = TRUE) |>
  janitor::adorn_totals()

quant_oscar |>
  dplyr::rename("Filmes PIXAR" = filme, "Números de Oscars" = n) |>
  knitr::kable() |>
  kableExtra::kable_styling(full_width = FALSE)
  

# Retirando a ultima linha da base de dados:

quant_oscar.novo <- quant_oscar[-24,]

# Gráficos:
ggplot(quant_oscar.novo, aes(y = filme, x = n, fill = filme)) + 
  geom_col(stat = "identity", show.legend = FALSE, width = .5)


# Unindo duas bases de dados. 

base_juntas <- inner_join(avaliacao, bilheteria,
                            by = "filme")

base_juntas.novo <- base_juntas[-24,]
View(base_juntas.novo)
str(base_juntas.novo)
names(base_juntas.novo)

selecao_1 <- base_juntas.novo[,c("filme","nota_rotten_tomatoes","nota_metacritic", "bilheteria_mundial", "orcamento")]

selecao_1 |>
  dplyr::rename("FILMES PIXAR" = filme, 
                "TOMATE DE OURO" = nota_rotten_tomatoes,
                "CRÍTICOS" = nota_metacritic, 
                "BILHETERIA MUNDIAL" = bilheteria_mundial,  
                "ORÇAMENTO" = orcamento) |>
  knitr::kable() |>
  kableExtra::kable_styling(full_width = FALSE)

#Visualizando as observações e as especificações referentes às variáveis do dataset

glimpse(selecao_1) 

#Estatísticas univariadas

summary(selecao_1)


# Modelo de Regressão Linear Simples:

modelo_filme <- lm(formula = bilheteria_mundial ~ orcamento,
                   data = selecao_1)

#Observando os parâmetros do modelo_tempodist
summary(modelo_filme)

selecao_1$yhat <- modelo_filme$fitted.values
selecao_1$erro <- modelo_filme$residuals


R2 <- (sum((selecao_1$yhat - mean(selecao_1$orcamento))^2))/
  ((sum((selecao_1$yhat - mean(selecao_1$orcamento))^2)) + (sum((selecao_1$erro)^2)))

round(R2, digits = 4)

# Por exemplo, um R² = 0,8234 significa que o modelo linear explica 82,34% da 
# variância da variável dependente a partir do regressores (variáveis independentes) 
# incluídas naquele modelo linear.

# Gráfico:

ggplot(selecao_1, aes(x = bilheteria_mundial, y = orcamento)) +
  geom_point(color = "#39568CFF") +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              level =  0.80,) +
  labs(x = "Bilheteria Mundial", y = "Orçamento") +
  scale_color_manual("Legenda:", values = "grey50") +
  theme_bw()

# O Teste de Shapiro-Wilk tem como objetivo avaliar se uma distribuição é 
#semelhante a uma distribuição normal. A distribuição normal também pode ser 
#chamada de gaussiana e sua forma assemelha-se a de um sino. Esse tipo de 
#distribuição é muito importante, por ser frequentemente usada para modelar 
#fenômenos naturais.
# Na prática, podemos querer saber, por exemplo, se a idade dos participantes 
#da nossa amostra segue ou não uma distribuição normal. Para isso, podemos usar 
#o teste de Shapiro-Wilk.
# Como resultado, o teste retornará a estatística W, que terá um valor de 
#significância associada, o valor-p. Para dizer que uma distribuição é normal, 
#o valor p precisa ser maior do que 0,05.

library(nortest)
sf.test(modelo_filme$residuals)

# E, de fato, o teste de Shapiro-Wilk mostra evidências que esta amostra não é 
# diferente de uma distribuição normal. Como resultados temos que W = 0,998 e 
#p = 0,518. Ou seja, p > 0,05.

# Referência

# Miot, H. A. (2017).  Avaliação da normalidade dos dados em estudos clínicos e 
# experimentais. Jornal Vascular Brasileiro [online]. 16(2), pp. 88-91. 
# doi.org/10.1590/1677-5449.041117.


