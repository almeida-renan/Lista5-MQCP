## Carregando ESEB e pacotes ----

ESEB2018 <- foreign::read.spss("ESEB2018.sav", to.data.frame = T)

library(tidyverse)
library(janitor)
library(infer)

## Manipulando variável sobre identificação partidária ----

t.pid <- ESEB2018 %>%
  mutate(partyid = as.factor(case_when(
    Q22A == "Sim" ~ "Sim",
    Q22A == "Não" ~ "Não",
    Q22A == "Não sabe (Esp.)" ~ "Não",
    Q22A == "Não respondeu (Esp.)" ~ "Não"
  ))
  )

## Teste Qui Quadrado ----

t.pid %>%
  infer::chisq_test(D2_SEXO ~ partyid)

# Gráficos
pidqui_quadrado <- t.pid %>% 
  specify(explanatory = D2_SEXO, response =  partyid, success = "Sim") %>%
  calculate(stat = "Chisq", order = c("Masculino", "Feminino"))

pidteorica_qui_quadrado <- t.pid %>% 
  specify(explanatory = D2_SEXO, response =  partyid, success = "Sim") %>%
  hypothesize(null = "independence") 

pidteorica_qui_quadrado %>%
  visualize(method = "theoretical") + 
  shade_p_value(pidqui_quadrado,
                direction = "greater")

## Tabela cruzada ----

tabelacruzada <- t.pid %>%
  tabyl(D2_SEXO, partyid) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  rename("Sexo/PartyID" = D2_SEXO)

tabelacont <- tabelacruzada[c(1,3,2)]

xtable::xtable(tabelacont)

## Tabela esperada ----

esperada <- tribble(
  ~sexo_partyid, ~Sim, ~Não,
  "Masculino",  188.49, 1001.38,
  "Feminino",   208.45, 1107.41
)

xtable::xtable(esperada)
