library(readxl)
library(purrr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stargazer)
library(stringr)
library(scales)
library(janitor)

inp <- "/Users/dhjs/Documents/projects/infocdmx_reconstruccion/solicitudes"
out <- "/Users/dhjs/Documents/projects/infocdmx_reconstruccion/clasificacion"
graf <- "/Users/dhjs/Documents/projects/infocdmx_reconstruccion/graficas"

e17 <- list.files(paste(inp, "2017", sep = "/"))

d17 <- map(e17,
            ~ read_excel(paste(inp, "2017", .x, sep = "/")))

t17 <- bind_rows(d17)


e18 <- list.files(paste(inp, "2018", sep = "/"))

d18 <- map(e18,
            ~ read_excel(paste(inp, "2018", .x, sep = "/")))

t18 <- bind_rows(d18)


e19 <- list.files(paste(inp, "2019", sep = "/"))

d19 <- map(e19,
           ~ read_excel(paste(inp, "2019", .x, sep = "/")))

t19a <- bind_rows(d19[c(1:19, 21:27)])
t19b <- bind_rows(d19[20])

# Nombres -----------------------------------------------------------------
nombres <- c(
  "No.", 
  "Sujeto_N", 
  "Sujeto", 
  "Organo_N", 
  "clave",
  
  "propio",
  "Folio", 
  "fecha_presentacion", 
  "Medio_presento", 
  "Informacion_objeto", 
  
  "Preguntas_solicitud", 
  "Tematica_solicitud", 
  "Area_interes", 
  "info_de_oficio", 
  "Estado_info", 
  
  "prevencion_solicitante", 
  "fecha_prevencion", 
  "solicitud_prevenida", 
  "preguntas_prevenidas", 
  "notificar_ampliacion", 
  
  "modalidad_respuesta", 
  "total_entes_turnados", 
  "ente_turnado", 
  "info_entregada", 
  "costo_reproduccion", 
  
  "monto_reproduccion", 
  "medio_disposicion_info", 
  "fecha_notificacion", 
  "medio_notificacion", 
  "dias_transcurridos_recepcion_notificacion",
  
  "Servidores_publicos_involucrados", 
  "sexo", 
  "edad", 
  "ocupacion", 
  "escolaridad", 
  
  "entidad_federativa", 
  "observaciones",
  "mes", 
  "year",
  "clasificacion"
)

names(t17) <- nombres

t17 <- t17 %>% 
  mutate(
    mes = month(fecha_presentacion),
    
    clasificacion = str_replace_all(clasificacion, 
                                    c("da?os" = "Da?os", "evaluacion" = "Evaluaci?n",
                                      "fondos" = "Fondos", "legalidad" = "Legalidad",
                                      "prevencion" = "Prevenci?n", "reconstruccion" = "Reconstrucci?n",
                                      "respuesta al sismo" = "Respuesta al sismo")),
    
    edad = as.numeric(edad),
    
    grupo_edad = ifelse(edad <= 19, "Hasta 19 anios",
                        ifelse(edad > 19 & edad < 30, "De 20 a 29 anios",
                               ifelse(edad >= 30 & edad < 40, "De 30 a 39 anios",
                                      ifelse(edad >= 40 & edad < 50, "De 40 a 49 anios",
                                             ifelse(edad >= 50 & edad < 60, "De 50 a 59 anios",
                                                    ifelse(edad >= 60 & edad < 70, "De 60 a 69 anios", 
                                                           ifelse(edad >= 70, "70 o mas anios", edad))))))),
    
    escolaridad = str_replace_all(escolaridad, c("Bachillerato o carrera t?cnica" = "Bachillerato",
                                                 "Bachillerato" = "Bachillerato o carrera t?cnica",
                                                 "Maestr?a o Doctorado" = "Maestr?a o doctorado")),
    
    ocupacion = str_replace_all(ocupacion, c("Otros - Amas de Casa" = "Hogar",
                                             "Otros - Organizaciones No Gubernamentales Internacionales" = "ONG",
                                             "Otros - Organizaciones No Gubernamentales Nacionales" = "ONG",
                                             "Otros - Asociaci?n pol?tica" = "Asociaci?n pol?tica",
                                             "Servidor P?blico" = "Servidor p?blico",
                                             "Otros - Comerciante" = "Comerciante",
                                             "Otros - Empleado u obrero" = "Empleado u obrero")),
    
    ocupacion = ifelse(str_detect(ocupacion, "Acad?mico") == T, "Acad?mico o estudiante",
                       ifelse(str_detect(ocupacion, "Empresarial")== T, "Empresario",
                              ifelse(str_detect(ocupacion, "Gubernamental") == T, "Servidor p?blico",
                                     ifelse(str_detect(ocupacion, "omunicaci?n") == T, "Medios de comunicaci?n", 
                                            ifelse(str_detect(ocupacion, "Otro") == T, "Otro", ocupacion))))),
    
    entidad_federativa = str_replace_all(entidad_federativa, c(
      "Michoac?n de Ocampo" = "Michoac?n",
      "Michoac?n" = "Michoac?n de Ocampo",
      
      "Quer?taro de Arteaga" = "Quer?taro",
      "Quer?taro" = "Quer?taro de Arteaga",
      
      "Estado de M?xico" = "M?xico",
      "M?xico" = "Estado de M?xico?"
    )),
    
    Organo_N = str_replace_all(Organo_N, c(
      "1" = "Administraci?n P?blico Central",
      "2" = "Desconcentrados y Paraestales",
      "3" = "Alcald?as",
      "4" = "Judicial",
      "5" = "Legislativo",
      "6" = "Aut?nomo", 
      "7" = "Partidos Pol?ticos",
      "8" = "Sindicatos"
    ))
  )

names(t18) <- nombres

t18 <- t18 %>% 
  mutate(
    mes = month(fecha_presentacion),
    
    clasificacion = str_replace_all(clasificacion, 
                                    c("da?os" = "Da?os", "evaluacion" = "Evaluaci?n",
                                      "fondos" = "Fondos", "legalidad" = "Legalidad",
                                      "prevencion" = "Prevenci?n", "reconstruccion" = "Reconstrucci?n",
                                      "respuesta al sismo" = "Respuesta al sismo")),
    
    edad = as.numeric(edad),
    
    grupo_edad = ifelse(edad <= 19, "Hasta 19 anios",
                        ifelse(edad > 19 & edad < 30, "De 20 a 29 anios",
                               ifelse(edad >= 30 & edad < 40, "De 30 a 39 anios",
                                      ifelse(edad >= 40 & edad < 50, "De 40 a 49 anios",
                                             ifelse(edad >= 50 & edad < 60, "De 50 a 59 anios",
                                                    ifelse(edad >= 60 & edad < 70, "De 60 a 69 anios", 
                                                           ifelse(edad >= 70, "70 o mas anios", edad))))))),
    
    escolaridad = str_replace_all(escolaridad, c("Bachillerato o carrera t?cnica" = "Bachillerato",
                                                 "Bachillerato" = "Bachillerato o carrera t?cnica",
                                                 "Maestr?a o Doctorado" = "Maestr?a o doctorado")),
    
    ocupacion = str_replace_all(ocupacion, c("Otros - Amas de Casa" = "Hogar",
                                             "Otros - Organizaciones No Gubernamentales Internacionales" = "ONG",
                                             "Otros - Organizaciones No Gubernamentales Nacionales" = "ONG",
                                             "Otros - Asociaci?n pol?tica" = "Asociaci?n pol?tica",
                                             "Servidor P?blico" = "Servidor p?blico",
                                             "Otros - Comerciante" = "Comerciante",
                                             "Otros - Empleado u obrero" = "Empleado u obrero")),
    
    ocupacion = ifelse(str_detect(ocupacion, "Acad?mico") == T, "Acad?mico o estudiante",
                       ifelse(str_detect(ocupacion, "Empresarial")== T, "Empresario",
                              ifelse(str_detect(ocupacion, "Gubernamental") == T, "Servidor p?blico",
                                     ifelse(str_detect(ocupacion, "omunicaci?n") == T, "Medios de comunicaci?n", 
                                            ifelse(str_detect(ocupacion, "Otro") == T, "Otro", ocupacion))))),
    
    entidad_federativa = str_replace_all(entidad_federativa, c(
      "Michoac?n de Ocampo" = "Michoac?n",
      "Michoac?n" = "Michoac?n de Ocampo",
      
      "Quer?taro de Arteaga" = "Quer?taro",
      "Quer?taro" = "Quer?taro de Arteaga",
      
      "Estado de M?xico" = "M?xico",
      "M?xico" = "Estado de M?xico?"
    )),
    
    Organo_N = str_replace_all(Organo_N, c(
      "1" = "Administraci?n P?blico Central",
      "2" = "Desconcentrados y Paraestales",
      "3" = "Alcald?as",
      "4" = "Judicial",
      "5" = "Legislativo",
      "6" = "Aut?nomo", 
      "7" = "Partidos Pol?ticos",
      "8" = "Sindicatos"
    ))
  )

names(t19a) <- nombres
names(t19b) <- nombres

t19 <- bind_rows(t19a, t19b)

t19 <- t19 %>% 
  mutate(
    mes = month(fecha_presentacion),
    
    clasificacion = str_replace_all(clasificacion, 
                                    c("da?os" = "Da?os", "evaluacion" = "Evaluaci?n",
                                      "fondos" = "Fondos", "legalidad" = "Legalidad",
                                      "prevencion" = "Prevenci?n", "reconstruccion" = "Reconstrucci?n",
                                      "respuesta al sismo" = "Respuesta al sismo")),
    
    edad = as.numeric(edad),
    
    grupo_edad = ifelse(edad <= 19, "Hasta 19 anios",
                        ifelse(edad > 19 & edad < 30, "De 20 a 29 anios",
                               ifelse(edad >= 30 & edad < 40, "De 30 a 39 anios",
                                      ifelse(edad >= 40 & edad < 50, "De 40 a 49 anios",
                                             ifelse(edad >= 50 & edad < 60, "De 50 a 59 anios",
                                                    ifelse(edad >= 60 & edad < 70, "De 60 a 69 anios", 
                                                           ifelse(edad >= 70, "70 o mas anios", edad))))))),
    
    escolaridad = str_replace_all(escolaridad, c("Bachillerato o carrera t?cnica" = "Bachillerato",
                                                 "Bachillerato" = "Bachillerato o carrera t?cnica",
                                                 "Maestr?a o Doctorado" = "Maestr?a o doctorado")),
    
    ocupacion = str_replace_all(ocupacion, c("Otros - Amas de Casa" = "Hogar",
                                             "Otros - Organizaciones No Gubernamentales Internacionales" = "ONG",
                                             "Otros - Organizaciones No Gubernamentales Nacionales" = "ONG",
                                             "Otros - Asociaci?n pol?tica" = "Asociaci?n pol?tica",
                                             "Servidor P?blico" = "Servidor p?blico",
                                             "Otros - Comerciante" = "Comerciante",
                                             "Otros - Empleado u obrero" = "Empleado u obrero")),
    
    ocupacion = ifelse(str_detect(ocupacion, "Acad?mico") == T, "Acad?mico o estudiante",
                       ifelse(str_detect(ocupacion, "Empresarial")== T, "Empresario",
                              ifelse(str_detect(ocupacion, "Gubernamental") == T, "Servidor p?blico",
                                     ifelse(str_detect(ocupacion, "omunicaci?n") == T, "Medios de comunicaci?n", 
                                            ifelse(str_detect(ocupacion, "Otro") == T, "Otro", ocupacion))))),
    
    entidad_federativa = str_replace_all(entidad_federativa, c(
      "Michoac?n de Ocampo" = "Michoac?n",
      "Michoac?n" = "Michoac?n de Ocampo",
      
      "Quer?taro de Arteaga" = "Quer?taro",
      "Quer?taro" = "Quer?taro de Arteaga",
      
      "Estado de M?xico" = "M?xico",
      "M?xico" = "Estado de M?xico?"
    )),
    
    Organo_N = str_replace_all(Organo_N, c(
      "1" = "Administraci?n P?blico Central",
      "2" = "Desconcentrados y Paraestales",
      "3" = "Alcald?as",
      "4" = "Judicial",
      "5" = "Legislativo",
      "6" = "Aut?nomo", 
      "7" = "Partidos Pol?ticos",
      "8" = "Sindicatos"
    ))
  )

# 2017 --------------------------------------------------------------------
summary(t17)

t17 %>% 
  group_by(mes, clasificacion) %>% 
  count() %>% 
  ggplot(aes(x = mes, y = n, group = clasificacion)) +
  geom_line(aes(color = clasificacion)) +
  geom_point(aes(color = clasificacion)) +
  labs(title = "N?mero de solicitudes por mes",
       subtitle = "2017", y = "", x = "",
       color = "Clasificación") +
  scale_x_continuous(labels = c("Septiembre", "Octubre", "Noviembre", "Diciembre")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        legend.position = "bottom")
ggsave(paste(graf, "Clasificacion 2017.png", sep = "/"), dpi = 300)

t17 %>% 
  group_by(mes) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    total = sum(n),
    porc = n*100/total
  ) %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por mes_2017.html", sep = "/")
  )

t17 %>% 
  group_by(Sujeto) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    total = sum(n),
    porc = round(n*100/total, 1)
  ) %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por sujeto_2017.html", sep = "/")
  )

t17 %>% 
  filter(!is.na(grupo_edad)) %>% 
  group_by(grupo_edad) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por edad_2017.html", sep = "/")
  )

t17 %>% 
  filter(!is.na(ocupacion)) %>% 
  group_by(ocupacion) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por ocupacion_2017.html", sep = "/")
  )

t17 %>% 
  group_by(Organo_N) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por organo_2017.html", sep = "/")
  )

t17 %>% 
  filter(!is.na(escolaridad)) %>% 
  group_by(escolaridad) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por escolaridad_2017.html", sep = "/")
  )

t17 %>% 
  filter(!is.na(sexo)) %>% 
  group_by(sexo) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por sexo_2017.html", sep = "/")
  )

##Pensar en manera de relacionar visualmente estas categor?as

# 2018 --------------------------------------------------------------------

t18 %>% 
  group_by(mes, clasificacion) %>% 
  count() %>% 
  ggplot(aes(x = as.factor(mes), y = n, group = clasificacion)) +
  geom_line(aes(color = clasificacion)) +
  geom_point(aes(color = clasificacion)) +
  labs(title = "N?mero de solicitudes por mes",
       subtitle = "2018", y = "", x = "",
       color = "Clasificación") +
  scale_x_discrete(labels = c("Enero", "Febrero", "Marzo",
                                "Abril", "Mayo", "Junio", "Julio",
                                "Agosto", "Septiembre", "Octubre",
                                "Noviembre", "Diciembre")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        legend.position = "bottom")
ggsave(paste(graf, "Clasificacion 2018.png", sep = "/"), dpi = 300)

t18 %>% 
  group_by(mes) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    total = sum(n),
    porc = n*100/total
  ) %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por mes_2018.html", sep = "/")
  )

t18 %>% 
  group_by(Sujeto) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    total = sum(n),
    porc = round(n*100/total, 1)
  ) %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por sujeto_2018.html", sep = "/")
  )

t18 %>% 
  filter(!is.na(grupo_edad)) %>% 
  group_by(grupo_edad) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por edad_2018.html", sep = "/")
  )

t18 %>% 
  filter(!is.na(ocupacion)) %>% 
  group_by(ocupacion) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por ocupacion_2018.html", sep = "/")
  )

t18 %>% 
  group_by(Organo_N) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por organo_2018.html", sep = "/")
  )

t18 %>% 
  filter(!is.na(escolaridad)) %>% 
  group_by(escolaridad) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por escolaridad_2018.html", sep = "/")
  )

t18 %>% 
  filter(!is.na(sexo)) %>% 
  group_by(sexo) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por sexo_2018.html", sep = "/")
  )

# 2019 --------------------------------------------------------------------

t19 %>% 
  group_by(mes, clasificacion) %>% 
  count() %>% 
  ggplot(aes(x = as.factor(mes), y = n, group = clasificacion)) +
  geom_line(aes(color = clasificacion)) +
  geom_point(aes(color = clasificacion)) +
  labs(title = "N?mero de solicitudes por mes",
       subtitle = "2019", y = "", x = "",
       color = "Clasificación") +
  scale_x_discrete(labels = c("Enero", "Febrero", "Marzo",
                              "Abril", "Mayo", "Junio", "Julio",
                              "Agosto", "Septiembre", "Octubre",
                              "Noviembre", "Diciembre")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        legend.position = "bottom")
ggsave(paste(graf, "Clasificacion 2019.png", sep = "/"), dpi = 300)

t19 %>% 
  group_by(mes) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    total = sum(n),
    porc = n*100/total
  ) %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por mes_2019.html", sep = "/")
  )

t19 %>% 
  group_by(Sujeto) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    total = sum(n),
    porc = round(n*100/total, 1)
  ) %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por sujeto_2019.html", sep = "/")
  )

t19 %>% 
  filter(!is.na(grupo_edad)) %>% 
  group_by(grupo_edad) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por edad_2019.html", sep = "/")
  )

t19 %>% 
  filter(!is.na(ocupacion)) %>% 
  group_by(ocupacion) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por ocupacion_2019.html", sep = "/")
  )

t19 %>% 
  group_by(Organo_N) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por organo_2019.html", sep = "/")
  )

t19 %>% 
  filter(!is.na(escolaridad)) %>% 
  group_by(escolaridad) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por escolaridad_2019.html", sep = "/")
  )

t19 %>% 
  filter(!is.na(sexo)) %>% 
  group_by(sexo) %>% 
  count() %>% 
  stargazer(
    summary = F,
    type = "html",
    out = paste(out, "clasificacion por sexo_2019.html", sep = "/")
  )
