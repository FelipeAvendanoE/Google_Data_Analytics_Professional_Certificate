# Cargamos librerias
library(tidyverse)
library(lubridate)
library(skimr)

# Cambiamos el directorio de  trabajo
setwd("C:/Users/-/Desktop/Case Study 1")

# Leemos los 12 archivos
divvy_202105 <- read.csv("202105-divvy-tripdata.csv")
divvy_202106 <- read.csv("202106-divvy-tripdata.csv")
divvy_202107 <- read.csv("202107-divvy-tripdata.csv")
divvy_202108 <- read.csv("202108-divvy-tripdata.csv")
divvy_202109 <- read.csv("202109-divvy-tripdata.csv")
divvy_202110 <- read.csv("202110-divvy-tripdata.csv")
divvy_202111 <- read.csv("202111-divvy-tripdata.csv")
divvy_202112 <- read.csv("202112-divvy-tripdata.csv")
divvy_202201 <- read.csv("202201-divvy-tripdata.csv")
divvy_202202 <- read.csv("202202-divvy-tripdata.csv")
divvy_202203 <- read.csv("202203-divvy-tripdata.csv")
divvy_202204 <- read.csv("202204-divvy-tripdata.csv")

# Verificamos que contengan las mismas columnas
colnames(divvy_202105)
colnames(divvy_202106)
colnames(divvy_202107)
colnames(divvy_202108)
colnames(divvy_202109)
colnames(divvy_202110)
colnames(divvy_202111)
colnames(divvy_202112)
colnames(divvy_202201)
colnames(divvy_202202)
colnames(divvy_202203)
colnames(divvy_202204)

# Unimos los 12 archivos en un unico dataframe
divvy <- bind_rows(divvy_202105, divvy_202106, divvy_202107, divvy_202108,
                   divvy_202109, divvy_202110, divvy_202111, divvy_202112,
                   divvy_202201, divvy_202202, divvy_202203, divvy_202204)

# Eliminamos los dataframe que no utilizaremos
rm(divvy_202105,
   divvy_202106,
   divvy_202107,
   divvy_202108,
   divvy_202109,
   divvy_202110,
   divvy_202111,
   divvy_202112,
   divvy_202201,
   divvy_202202,
   divvy_202203,
   divvy_202204
    )

# Obtenemos un resumen de los datos
summary(divvy)

# Eliminamos las observaciones sin coordenadas
divvy_clean <- drop_na(divvy, end_lat)
rm(divvy)

# Damos un nuevo vistazo a los datos
head(divvy_clean)
skim_without_charts(divvy_clean)

# Conocemos los tipos de bicicletas y de miembros
unique(divvy_clean$rideable_type)
unique(divvy_clean$member_casual)

# Observamos que existen observaciones con strings vacios
# Revisamos cada columna para ver valores incorrectos u omitidos según su largo
sort(unique(nchar(divvy_clean$ride_id)))
sort(unique(nchar(divvy_clean$rideable_type)))
sort(unique(nchar(divvy_clean$started_at)))
sort(unique(nchar(divvy_clean$ended_at)))
sort(unique(nchar(divvy_clean$start_station_name)))
sort(unique(nchar(divvy_clean$start_station_id)))
sort(unique(nchar(divvy_clean$end_station_name)))
sort(unique(nchar(divvy_clean$end_station_id)))
sort(unique(nchar(divvy_clean$member_casual)))

# Revisaremos aquellos string con largos cero.
# Primero todos los que no tengan ningun dato para nombre de estacion o ID
nrow(divvy_clean %>%
  filter(nchar(divvy_clean$start_station_name) == 0,
         nchar(divvy_clean$start_station_id) == 0,
         nchar(divvy_clean$end_station_name) == 0,
         nchar(divvy_clean$end_station_name) == 0
         ))

# Ahora aquellos que les falte al menos un dato
nrow(divvy_clean %>%
       filter(nchar(divvy_clean$start_station_name) == 0 |
              nchar(divvy_clean$start_station_id) == 0 |
              nchar(divvy_clean$end_station_name) == 0 |
              nchar(divvy_clean$end_station_name) == 0
       ))

# Aquellos sin datos para el comienzo del viaje
nrow(divvy_clean %>%
       filter(nchar(divvy_clean$start_station_name) == 0,
              nchar(divvy_clean$start_station_id) == 0
       ))

# Aquellos sin datos para el fin del viaje
nrow(divvy_clean %>%
       filter(nchar(divvy_clean$end_station_name) == 0,
              nchar(divvy_clean$end_station_name) == 0
       ))


# llevamos los string a formato datetime y calculamos columnas de apoyo.
# Agrupar por meses, dias y horas.
# Cambiar nombre a los tipos de miembro
divvy_clean <- divvy_clean %>%
  mutate(start_date = ymd_hms(started_at),
         end_date = ymd_hms(ended_at),
         duration_min = as.numeric((end_date - start_date)/60),
         year = year(start_date),
         month = month(start_date, label = TRUE),
         week_day = wday(start_date, label = TRUE, week_start = 1, abbr = FALSE),
         hour = hour(start_date),
         member_casual = recode(member_casual, "member" = "anual",
                                "casual" = "ocasional")
        )

# Obtenemos un nuevo resumen de los datos
summary(divvy_clean)
skim(divvy_clean)

# Existen duraciones negativas y mayores a un día. Serán eliminadas
divvy_clean <- divvy_clean[divvy_clean$duration > 0,]

divvy_clean <- divvy_clean[divvy_clean$duration < 1440,]

# Volvemos a calcular el resumen para observar los cambios realizados
summary(divvy_clean)

# Calculamos el promedio, la mediana, el maximo y el minimo de duracion 
# del viaje y la cantidad de viajes.


# Primero por dia de la semana
divvy_clean %>%
  group_by(week_day) %>%
  summarise(mean = mean(duration_min), median = median(duration_min), 
            max = max(duration_min), min = min(duration_min), n = n())

# Por tipo de usuario
divvy_clean %>%
  group_by(member_casual) %>%
  summarise(mean = mean(duration_min), median = median(duration_min), 
            max = max(duration_min), min = min(duration_min), n = n())

# Por día de la semana y tipo de usuario
divvy_clean %>%
  group_by(week_day, member_casual) %>%
  summarise(mean = mean(duration_min), median = median(duration_min), 
            max = max(duration_min), min = min(duration_min), n = n())


# Observamos las estaciones mas comunes

estaciones_inicio <- divvy_clean %>%
  filter(start_station_name != "") %>%
  group_by(member_casual, start_station_name) %>%
  summarise(n_viajes = n()) %>%
  arrange(desc(n_viajes))

head(estaciones_inicio, 10)

estaciones_fin <- divvy_clean %>%
  filter(end_station_name != "") %>%
  group_by(member_casual, end_station_name) %>%
  summarise(n_viajes = n()) %>%
  arrange(desc(n_viajes))

head(estaciones_fin, 10)


# Graficamos
# Agrupado según mes
ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = month, fill = member_casual),
           position = "dodge" ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Viajes por mes y usuario",
       subtitle = "Número de viajes por mes y por tipo de usuario",
       x = "Mes",
       y = "Número de viajes",
       fill="Tipo de miembro"
       )

ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = month, y = duration_min, fill = member_casual),
           stat = "summary", fun = "mean", position = "dodge" ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Viajes por mes y usuario",
       subtitle = "Número de viajes por mes y por tipo de usuario",
       x = "Mes",
       y = "Número de viajes",
       fill="Tipo de miembro"
  )

ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = month, y = duration_min, fill = member_casual),
           stat = "summary", fun = "sum",position = "dodge" ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Viajes por mes y usuario",
       subtitle = "Número de viajes por mes y por tipo de usuario",
       x = "Mes",
       y = "Número de viajes",
       fill="Tipo de miembro"
  )

# Agrupado por día de la semana
ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = week_day, fill = member_casual),
           position = "dodge" ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Viajes por día y usuario",
      subtitle = "Número de viajes por día y por tipo de usuario",
      x = "Día de la semana",
      y = "Número de viajes",
      fill="Tipo de miembro"
      )

ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = week_day, y = duration_min, fill = member_casual),
           stat = "summary", fun = "mean", position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Viajes por día y usuario",
       subtitle = "Número de viajes por día y por tipo de usuario",
       x = "Día de la semana",
       y = "Número de viajes",
       fill="Tipo de miembro"
  )


ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = week_day, y = duration_min, fill = member_casual),
           stat = "summary", fun = "sum", position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Viajes por día y usuario",
       subtitle = "Número de viajes por día y por tipo de usuario",
       x = "Día de la semana",
       y = "Número de viajes",
       fill="Tipo de miembro"
  )

# Agrupado por hora de inicio del viaje
ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = hour, fill = member_casual),
           position = "dodge" ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Viajes por hora y usuario",
       subtitle = "Número de viajes por hora de inicio y por tipo de usuario",
       x = "Hora del día",
       y = "Número de viajes",
       fill="Tipo de miembro"
  )


ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = hour, y = duration_min, fill = member_casual),
           stat = "summary", fun = "mean", position = "dodge" ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Viajes por hora y usuario",
       subtitle = "Número de viajes por hora y por tipo de usuario",
       x = "Hora del día",
       y = "Número de viajes",
       fill="Tipo de miembro"
       )

ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = hour, y = duration_min, fill = member_casual),
           stat = "summary", fun = "sum", position = "dodge" ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Viajes por hora y usuario",
       subtitle = "Número de viajes por hora y por tipo de usuario",
       x = "Hora del día",
       y = "Número de viajes",
       fill="Tipo de miembro"
  )

# Agrupado según la duración del viaje
ggplot(data = divvy_clean) + 
  geom_bar(mapping = aes(x = duration_min, fill = member_casual),
           position = "dodge") +
  coord_cartesian(xlim = c(0, 50)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Duración del viaje",
       subtitle = "Duración del viaje por tipo de usuario",
       x = "Duración en min",
       y = "Número de viajes",
       fill="Tipo de miembro"
       )


# Estaciones más comunes
ggplot(data = estaciones_inicio[1:10, ]) + 
  geom_col(mapping = aes(x = reorder(start_station_name, n_viajes), 
                         y = n_viajes, fill = member_casual),
           position = "dodge") +
  coord_flip() +
  labs(title = "Top 10 estaciones de inicio",
       subtitle = "Estaciones de inicio más populares según tipo de usuario",
       x = "Estación",
       y = "Número de viajes",
       fill="Tipo de miembro"
       )


ggplot(data = estaciones_fin[1:10, ]) + 
  geom_col(mapping = aes(x = reorder(end_station_name, n_viajes),
                         y = n_viajes, fill = member_casual),
           position = "dodge") +
  coord_flip() +
  labs(title = "Top 10 estaciones de fin",
       subtitle = "Estaciones de fin más populares según tipo de usuario",
       x = "Estación",
       y = "Número de viajes",
       fill="Tipo de miembro"
       )



