## Carregando ESEB ----

ESEB2018 <- foreign::read.spss("ESEB2018.sav", to.data.frame = T)

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
  tabyl(D1A_FAIXAID, partyid) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting()

t.pid %>%
  infer::chisq_test(D1A_FAIXAID ~ partyid)

pidqui_quadrado <- t.pid %>% 
  specify(explanatory = D1A_FAIXAID, response =  partyid, success = "Sim") %>%
  calculate(stat = "Chisq")

pidteorica_qui_quadrado <- t.pid %>% 
  specify(explanatory = D1A_FAIXAID, response =  partyid, success = "Sim") %>%
  hypothesize(null = "independence") 

pidteorica_qui_quadrado %>%
  visualize(method = "theoretical") + 
  shade_p_value(brinksqui_quadrado,
                direction = "greater")