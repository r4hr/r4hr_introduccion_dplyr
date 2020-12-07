# Eliminar los # para instalar los paquetes

## # Instalando el paquete individual
# install.packages("dplyr")
 
## # Instalando el conjunto de paquetes de tidyverse
# install.packages("tidyverse")
 



## Librerías --------------------------------------
library(dplyr)   # Manipulación y limpieza de datos
library(readr)   # Lectura y carga de archivos
library(tidyr)   # Transformación de datos
library(ggplot2) # Hacer gráficos según la gramática de los gráficos


## Datasets ---------------------------------------

# Cargamos encuesta de sueldos
encuesta <- read_delim("https://raw.githubusercontent.com/r4hr/r4hr_introduccion_dplyr/main/Datos/encuesta_sueldos.csv", delim = ",")

# Cargamos otro archivo con kpis de ausentismo y de horas extras
kpis <- read_delim("https://raw.githubusercontent.com/r4hr/r4hr_introduccion_dplyr/main/Datos/kpi_aus_he.csv", delim = ";")


# Ejemplo de dplyr -------------------------------
encuesta %>% 
  select(puesto, sueldo_bruto) %>% # Selecciona las columnas
  group_by(puesto) %>%     # Agrupa por la variable puesto
  summarise(sueldo_promedio = mean(sueldo_bruto)) %>% # Crea una variable con el sueldo promedio
  arrange(-sueldo_promedio) # Ordena descendentemente los resultados por la variable que pasemos.


# Concatenando funciones en dplyr ----------------

# Código sin el pipe
arrange(filter(select(encuesta, puesto, sueldo_bruto, personas_a_cargo), personas_a_cargo > 0), puesto)

# Código con el pipe
encuesta %>%
  select(puesto, sueldo_bruto, personas_a_cargo) %>%
  filter(personas_a_cargo>0) %>%
  arrange(puesto) 

# Funciones principales de dplyr

# Select---------------------------------------------
encuesta %>% 
  select(satisfaccion, sueldo_bruto) 

encuesta %>%
  select_if(is.numeric) %>% 
  head()


# Filter----------------------------------------------
encuesta %>%
  select(personas_a_cargo, sueldo_bruto) %>% 
  filter(personas_a_cargo  > 0) 


# Filtramos los empleados que son de Universidad Privada y de empresas con más de 700 empleados.
# (Operador lógico "y")
encuesta %>%
  filter(universidad == "Universidad Privada" &  empleados > 700)


# Filtramos por dos condiciones de una misma variable (operador lógico "o")
encuesta %>%
  filter(rubro == "Servicios de salud"| rubro == "Tecnologías de Información, Sistemas, y afines")

# Otra forma de filtrar por varias condiciones de una misma variable
encuesta %>%
   filter(rubro %in% c("Servicios de salud", "Tecnologías de Información, Sistemas, y afines"))

# Filtrar un rango de valores de una variable numérica
encuesta %>% 
  filter(between(sueldo_bruto,    # Variable a filtrar 
                 50000,           # Valor mínimo
                 150000))         # Valor máximo



# Arrange ------------------------------------------

# Orden por variable numérica
encuesta %>%
  select(puesto, edad) %>%
  arrange(edad)

# Orden por variable categórica
encuesta %>%
  select(rubro, empleados) %>%
  arrange(rubro)

# Orden descendente - función auxiliar desc()
encuesta %>%
  select(puesto, edad) %>%
  arrange(desc(edad)) 

# Orden descendente - signo menos ("-")
encuesta %>%
  select(puesto, edad) %>%
  arrange(-edad)  


# Group by -------------------------------------------------
encuesta %>%
  select(puesto, sueldo_bruto) %>%
  group_by(puesto) %>% 
  head()


# Summarise -------------------------------------------------
encuesta %>%
  select(puesto, sueldo_bruto) %>%
  group_by(puesto) %>%
  summarise(sueldo_promedio = mean(sueldo_bruto)) # Calcula sueldo promedio


# Mutate ----------------------------------------------------
encuesta %>%
  select(puesto, sueldo_bruto) %>%
  mutate(sueldo_dolar = sueldo_bruto / 86.75) # Crea una columna con el sueldo en dólares


# Mutate puede modificar y sobreescribir variables existentes
class(encuesta$puesto)

# Cambia la variable factor de tipo character a factor
# Ordena los puestos por jerarquía
encuesta %>% select(puesto, sueldo_bruto) %>% 
  mutate(puesto = factor(puesto, levels = c("Director", "Gerente", "Jefe", "Responsable", #<<
                                            "HRBP", "Analista", "Administrativo"))) %>%   #<<
  group_by(puesto) %>% 
  summarise(sueldo_promedio = mean(sueldo_bruto))
  

# Función auxiliar case_when() - Categorizar variables numéricas

encuesta %>% 
  select(empleados, sueldo_bruto) %>% 
  mutate(cantidad_empleados = case_when( 
    empleados <= 100 ~ "Hasta 100",      
    empleados <= 500 ~ "Hasta 500",      
    empleados <= 1000 ~ "Hasta 1000",    
    empleados <= 5000 ~ "Hasta 5000",    
    empleados = TRUE ~ "Más de 5000"     
  )) %>% 
  group_by(cantidad_empleados) %>% 
  summarise(sueldo_promedio = mean(sueldo_bruto))


# Rename ----------------------------------------------------

names(encuesta)

encuesta <- encuesta %>%
  rename(sistema_gestion = erp) 

names(encuesta)


# Pivotear datasets ----------------------------------------
View(kpis)


# Transformar un dataset ancho en uno largo
kpis_long <- kpis %>% 
  pivot_longer(cols = c("ene":"dic"), # Selecciona columnas a pivotar
               names_to = "mes",   # Nombre de la variable que contendrá lo que antes era nombre de columna
               values_to = "tasa") # Nombre de la variable que contendrá los valores

kpis_long

# Ordena las variables mes
kpis_long <- kpis_long %>% 
  mutate(mes = factor(mes, levels = c("ene", "feb", "mar", "abr", "may", "jun",
                                      "jul", "ago", "sep", "oct", "nov", "dic")))

# Secuencia básica de análisis de datos ---------------------

## 1 - Agrupar los datos
## 2 - Hacer resúmenes estadísticos
## 3 - Graficar


# Ver los resultados de los indicadores por Sector
kpis_long %>% 
  group_by(Sector, KPI) %>% 
  summarise(tasa_anual = mean(tasa)) %>% 
  ggplot(aes(x = KPI, y = tasa_anual)) +  # Realiza el gráfico de barras
  geom_col() +
  facet_grid(~Sector)


# Analizar la evolución mensual de los KPI's de cada sector
ggplot(kpis_long, aes(x = mes, y = tasa, group = Sector, colour = Sector)) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  facet_wrap(~KPI,nrow = 2)

