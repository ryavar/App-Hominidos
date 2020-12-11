reg <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/shps/reg.rds")
reg_data <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/personas_region.rds") %>% 
  group_by(REGION) %>% 
  summarise(Population=sum(Personas,na.rm = T),Hombres=sum(Hombre,na.rm = T),Mujeres=sum(Mujer,na.rm = T)) %>% 
  cbind(centroid(reg)) %>% 
  mutate(
    popup = str_c(str_c("Región:", REGION,
                        sep = " "),
                  str_c("Población:", Population,
                        sep = " "),
                  str_c(Hombres,"Hombres",
                        sep = " "),
                  str_c(Mujeres, "Mujeres",
                        sep = " "),
                  sep = "\n")
  )
rm(reg)


provi <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/shps/provi.rds")
provi_data <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/personas_provincia.rds") %>%
  group_by(PROVINCIA) %>% 
  summarise(Population=sum(Personas,na.rm = T),Hombres=sum(Hombre,na.rm = T),Mujeres=sum(Mujer,na.rm = T)) %>% 
  cbind(centroid(provi)) %>% 
  mutate(
    popup = str_c(str_c("Provincia:", PROVINCIA,
                        sep = " "),
                  str_c("Población:", Population,
                        sep = " "),
                  str_c(Hombres,"Hombres",
                        sep = " "),
                  str_c(Mujeres, "Mujeres",
                        sep = " "),
                  sep = "\n")
  )
rm(provi)

comuna <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/shps/comuna.rds")
comuna_data <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/personas_comuna.rds") %>% 
  filter(COMUNA!=12202) %>% 
  group_by(COMUNA) %>% 
  summarise(Population=sum(Personas,na.rm = T),Hombres=sum(Hombre,na.rm = T),Mujeres=sum(Mujer,na.rm = T)) %>% 
  cbind(centroid(comuna)) %>% 
  mutate(
    popup = str_c(str_c("Comuna:", COMUNA,
                        sep = " "),
                  str_c("Población:", Population,
                        sep = " "),
                  str_c(Hombres,"Hombres",
                        sep = " "),
                  str_c(Mujeres, "Mujeres",
                        sep = " "),
                  sep = "\n")
  )
rm(comuna)


distrito <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/shps/distrito.rds")
distrito@data <- distrito@data %>% mutate(DISTRITO=COD_DISTRI, COMUNA=as.character(COMUNA))
distrito_data <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/personas_dc.rds") %>% 
  filter(DISTRITO!=99 & COMUNA!=12202) %>% 
  group_by(COMUNA,DISTRITO) %>% 
  full_join(distrito@data %>% 
              as.data.frame() %>% 
              mutate(COMUNA=as.numeric(COMUNA)),
            by=c("COMUNA","DISTRITO")) %>%
  summarise(Population=sum(Personas,na.rm = T),Hombres=sum(Hombre,na.rm = T),Mujeres=sum(Mujer,na.rm = T)) %>% 
  data.frame(centroid(distrito)) %>% 
  mutate(
    popup = str_c(str_c("Distrito:", DISTRITO,
                        sep = " "),
                  str_c("Población:", Population,
                        sep = " "),
                  str_c(Hombres,"Hombres",
                        sep = " "),
                  str_c(Mujeres, "Mujeres",
                        sep = " "),
                  sep = "\n")
  )
rm(distrito)


distrito@data %>% 
  mutate(DISTRITO=COD_DISTRI, COMUNA=as.character(COMUNA)) %>% 
  anti_join(distrito_data %>% 
              as.data.frame() %>% 
              mutate(COMUNA=as.character(COMUNA)),
            by=c("COMUNA","DISTRITO")) %>% 
  View()

distrito_data %>% 
  as.data.frame() %>%
  mutate(COD_DISTRI=DISTRITO, COMUNA=as.factor(COMUNA)) %>%
  anti_join(distrito@data,
            by=c("COMUNA","COD_DISTRI")) %>% 
  View()
testo <- distrito@data %>% 
  mutate(DISTRITO=COD_DISTRI, COMUNA=as.character(COMUNA)) %>% 
  full_join(distrito_data %>% 
              as.data.frame() %>% 
              mutate(COMUNA=as.character(COMUNA)),
            by=c("COMUNA","DISTRITO"))

personas %>% 
  filter(COMUNA==3101, PNEA!="No aplica / Sin información") %>% 
  group_by(PNEA) %>% 
  summarise(H=sum(Hombre,na.rm = T),M=sum(Mujer,na.rm = T),Total=sum(Personas,na.rm = T)) %>% 
  janitor::adorn_totals()

personas %>% 
  filter(COMUNA==3101) %>% 
  group_by(Rama) %>% 
  summarise(H=sum(Hombre,na.rm = T),M=sum(Mujer,na.rm = T),Total=sum(Personas,na.rm = T)) %>% 
  janitor::adorn_totals()
#2620:9b::1965:d5ce
shiny::runApp('C:/Users/M/Desktop/GAC/APP MH/APP/mapa',host="25.103.240.173" , port=2620)
 