library(dplyr)
library(readxl)
library(purrr)
library(ggplot2)
library(stargazer)
library(tidyr)
library(openxlsx)

inp <- "/Users/dhjs/Documents/projects/infocdmx_reconstruccion/solicitudes"
list.files(inp)
out <- "/Users/dhjs/Documents/projects/infocdmx_reconstruccion/clasificacion"

# 2017 --------------------------------------------------------------------

e17 <- list.files(paste(inp, "2017", sep = "/"))

d17 <- map(
  e17,
  ~ read_excel(paste(inp, "2017", .x, sep = "/"))
)

t17 <- bind_rows(d17)

write.xlsx(t17, file = paste(inp, "2017 final.xlsx", sep = "/"))

t17 %>% 
  group_by(Sujeto, clasificacion) %>% 
  count() %>% 
  arrange(Sujeto, desc(n)) %>% 
  ungroup() %>% 
  mutate(clasificacion = stringr::str_to_sentence(clasificacion)) %>% 
  pivot_wider(
    names_from = clasificacion,
    values_from = n
  ) %>% 
  mutate(
    Total = rowSums(.[2:8], na.rm = T)
  ) %>%
  stargazer(
    summary = F,
    type = "html",
    out = paste(inp, "nueva clas 17.html", sep = "/")
  )

# 2018 --------------------------------------------------------------------

e18 <- list.files(paste(inp, "2018", sep = "/"))

d18 <- map(
  e18,
  ~ read_excel(paste(inp, "2018", .x, sep = "/"))
)

t18 <- bind_rows(d18)

write.xlsx(t18, file = paste(inp, "2018 final.xlsx", sep = "/"))

t18 %>% 
  group_by(Sujeto, clasificacion) %>% 
  count() %>% 
  arrange(Sujeto, desc(n)) %>% 
  ungroup() %>% 
  mutate(clasificacion = stringr::str_to_sentence(clasificacion)) %>% 
  pivot_wider(
    names_from = clasificacion,
    values_from = n
  ) %>% 
  mutate(
    Total = rowSums(.[2:8], na.rm = T)
  ) %>%
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "nueva clas 18.html", sep = "/")
  )

# 2019 --------------------------------------------------------------------

e19 <- list.files(paste(inp, "2019", sep = "/"))

d19 <- map(e19,
           ~ read_excel(paste(inp, "2019", .x, sep = "/")))

t19a <- bind_rows(d19[c(1:19, 21:27)])
t19b <- bind_rows(d19[20])

names(t19b) <- names(t19a)

t19 <- bind_rows(t19a, t19b)

write.xlsx(t19, file = paste(inp, "2019 final.xlsx", sep = "/"))

t19 %>% 
  group_by(Sujeto, clasificacion) %>% 
  count() %>% 
  arrange(Sujeto, desc(n)) %>% 
  ungroup() %>% 
  mutate(clasificacion = stringr::str_to_sentence(clasificacion)) %>% 
  pivot_wider(
    names_from = clasificacion,
    values_from = n
  ) %>% 
  mutate(
    Total = rowSums(.[2:8], na.rm = T)
  ) %>%
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "nueva clas 19.html", sep = "/")
  )

final <- bind_rows(t17, t18, t19)

write.xlsx(final, file = paste(inp, "Reconstruccion consolidada.xlsx", sep = "/"))
