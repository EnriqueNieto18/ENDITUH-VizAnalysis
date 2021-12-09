library(tidyverse)
library(ggplot2)
library(readr)
library(tidyr)
library(stargazer)
library(mfx) 
library(hrbrthemes)
library(viridis)
library(hexbin)
library(ggpubr)
library(gridExtra)

####Empezando el ódigo
#Subiendo la base sobre Redes sociales

tr_endutih_usuario_anual_2020 <- read_csv( "/tr_endutih_usuario_anual_2020.csv") #Cargar datos
tr_endutih_usuario_anual_2020[
  ,c(5,9,11,13,114:125)]->Endutih ##Filtro de columnas que interesan
Endutih[1:11]->Endutih
colnames(Endutih)[10:11]<-c("Whatsapp", "Youtube")
Endutih %>% gather(Red, Respuesta, Facebook:Youtube)->End
na.omit(End)->End
End$Respuesta[End$Respuesta==1]<-"Si"
End$Respuesta[End$Respuesta==2]<-"No"
End %>% group_by(
  EDAD, Red, Respuesta) %>% count()->Enda #Base de datos para Graficos

### Logit y probit... usando el sexo y estrato para explicar el uso
#Redes sociales, dando 0 y 1 a variables....
Endutih$SEXO[Endutih$SEXO==2]<-0
Endutih$Facebook[Endutih$Facebook==2]<-0
Endutih$Instagram[Endutih$Instagram==2]<-0
#Modelo Logit
fb<-glm(Facebook~SEXO + ESTRATO, family = binomial(link = "probit"),
        data = Endutih)
ig<-glm(Instagram~SEXO + ESTRATO, family = binomial(link = "probit"), 
        data = Endutih)
#Modelo Probit
fb1<-glm(Facebook~SEXO + ESTRATO, family = binomial(link = "logit"),
         data = Endutih)
ig1<-glm(Instagram~SEXO + ESTRATO, family = binomial(link = "logit"),
         data = Endutih)
#Creación de tablas Latex
stargazer(ig,fb)
stargazer(ig1,fb1)

#Prueba de código
En$Respuesta[En$Respuesta==2]<-0
prob<-glm(Respuesta~ EDAD + Red, family = binomial(link = "probit"), data = En)
summary(prob)
stargazer(prob)

### Subir una base excel 
write.csv(Enda,file = 
            "C:/Users/Enrique Nieto/Documents/Social Media/Edituh/RedesFrame.csv")

#Acomodando los datos por edad
RedesFrame1 <- read_csv("Social Media/Edituh/RedesFrame1.csv")

#Filtrando mayores de 70y LinkedIn
RedesFrame1<-  RedesFrame1 %>% filter(EDAD<70 &
                                        Respuesta=="Si" & Red!="LinkedIn")

# Gráfica con multiples lines Redes sociales
ggplot(RedesFrame1, aes(EDAD,Porcentaje, col=Red)) +
  geom_smooth() + geom_point() + facet_wrap(reorder(~Red), scales="free") + 
  theme_minimal() + 
  labs(title=
         "Porcentaje de usuarios con una cuenta por red social en México 2020",
       y="% de cuentas por edad",
       caption="Elaboración propia con datos de ENDUTIH"
       , x="Edad", ylim=c(0,100)) +
  scale_x_continuous(breaks = seq(5,70,15)) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_color_manual(values=c("#3a569c","#e01b77","#efe200",
                              "#27a8e1", "#35dc52", "#d32322"))

#### Tipo de cuenta de entrada

Dispositivo<-c("Smartphone", "Laptop", "SmartTv", "Computadora","Videojuego")
Porcentaje<-c(96,41,22.2,16.5,6.1)
Dispostivos<-data.frame(Dispositivo,Porcentaje)

ggplot(Dispostivos, aes(reorder(Dispositivo, -Porcentaje), Porcentaje)) + 
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal()  + ylim(0,120) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size=18),
        axis.title = element_text(size=20)) +
  labs(y="Porcentaje %", x="Dispositivo", 
       title="Dispostivos con los que se entro una vez a internet (Porcentaje)" ,
       caption = "Fuente: Elaboración propia con datos de la ENDUTIH 2020") +  
  geom_text(size=7, aes(label = paste(format(Porcentaje), "%")), vjust = -0.5) 


#### MCO edad y tiempo en internet

###
Mco<-data.frame(tr_endutih_usuario_anual_2020$P7_4,
                tr_endutih_usuario_anual_2020$EDAD,
                tr_endutih_usuario_anual_2020$ESTRATO,
                tr_endutih_usuario_anual_2020$SEXO,
                tr_endutih_usuario_anual_2020$NIVEL,
                tr_endutih_usuario_anual_2020$GRADO,
                tr_endutih_usuario_anual_2020$P7_10_3,
                tr_endutih_usuario_anual_2020$P7_11_2,
                tr_endutih_usuario_anual_2020$P7_11_4,
                tr_endutih_usuario_anual_2020$P7_22_18,
                tr_endutih_usuario_anual_2020$P7_31_4,
)
#Nombres columnas
colnames(Mco)<-c("Horas","Edad","Estrato","Sexo","Nivel","Grado", 
                 "Mensajería", 
                 "Video Streaming",
                 "Musica streaming",
                 "Taxi App",
                 "Pagos App")

Mco<-na.omit(Mco)
Mco$Estrato<-as.character(Mco$Estrato)
#Grupos generacionales
Mco$Grupo[Mco$Edad>=9 & Mco$Edad<=24]<-"Gen Z"
Mco$Grupo[Mco$Edad>=25 & Mco$Edad<=40]<-"Millenial"
Mco$Grupo[Mco$Edad>=41 & Mco$Edad<=56]<-"Gen X"
Mco$Grupo[Mco$Edad>=57 & Mco$Edad<=72]<-"Boomers"


###ENDITUH 2 Gasto por celular
Endituh2<-data.frame(tr_endutih_usuario_anual_2020$EDAD,
                tr_endutih_usuario_anual_2020$ESTRATO,
                tr_endutih_usuario_anual_2020$SEXO,
                tr_endutih_usuario_anual_2020$NIVEL,
                tr_endutih_usuario_anual_2020$GRADO,
                tr_endutih_usuario2_anual_2020$P8_8
)

colnames(Endituh2)<-c("Edad","Estrato","Sexo","Nivel","Grado","Gasto")

## Filtro gasto por telefonía... 
Endituh3<-Endituh2 %>% filter(Gasto<2500)

Endituh3<-na.omit(Endituh3)
Endituh3$Estrato<-as.character(Endituh3$Estrato)
Endituh3$Nivel<-as.character(Endituh3$Nivel)
#Remplazar valores en Estrato

Endituh3$Estrato[Endituh3$Estrato==1]<-"Bajo"
Endituh3$Estrato[Endituh3$Estrato==2]<-"Medio"
Endituh3$Estrato[Endituh3$Estrato==3]<-"Medio-Alto"
Endituh3$Estrato[Endituh3$Estrato==4]<-"Alto"

#Gráfico

a<-ggplot(Endituh3,
       aes(Edad, Gasto, col=Estrato))  +
  geom_smooth() + theme_minimal()  +  labs(title = "Gasto Mensual en telefonía celular por estrato socio-económico",
       y="Gasto ($)",
       caption = "Elaboración propia con datos de la ENDITUH") 

summary(lm(Endituh3$Gasto~Endituh3$Estrato, method = "gam"))
lm(Endituh3$Gasto~Endituh3$Estrato, method = "gam")

## Comparando Apps entre Variables (Estrato, Sexo, NIvel etc)

#Barplot
Mco1<-Mco %>% gather(Tipo, Respuesta, 7:11)
Mco1$Respuesta[Mco1$Respuesta==1]<-"Sí"
Mco1$Respuesta[Mco1$Respuesta==2]<-"No"

Mco

Mco2<-Mco1 %>% group_by(Tipo, Estrato, Edad) %>% filter(Respuesta=="Sí" & 
                                            Grupo==c("Millenial",
                                                     "Gen Z")) %>% 
summarise(Respuesta=n())

Mco3<-Mco1 %>% group_by(Tipo, Estrato) %>% filter(Respuesta=="No" & 
                                            Grupo==c("Millenial",
                                                     "Gen Z")) %>% 
summarise(Respuesta=n())
summary(Mco1)
stargazer(Mco1)
stargazer(Endutih, type="text")

#Limpieza/Nueva columna
MCO<-data.frame(Mco2,Mco3$Respuesta)
colnames(MCO)[3:4]<-c("Sí","No")
MCO$Respuesta<-MCO$Sí/(MCO$Sí+MCO$No)
Mco2<-na.omit(Mco2)

Estrat<-data.frame(Mco2,Mco3$Respuesta)
colnames(Estrat)[3:4]<-c("Sí","No")
Estrat$Respuesta<-Estrat$Sí/(Estrat$Sí+Estrat$No)
Estrat$Estrato[Estrat$Estrato==1]<-"Bajo"
Estrat$Estrato[Estrat$Estrato==2]<-"Medio"
Estrat$Estrato[Estrat$Estrato==3]<-"Medio-Alto"
Estrat$Estrato[Estrat$Estrato==4]<-"Alto"
## Gráfica estrat

ab<-ggplot(Estrat, aes(reorder(Tipo, -Respuesta),Respuesta,
                   fill=(reorder(Estrato, -Respuesta)))) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_percent() + 
  theme_minimal() + scale_fill_viridis_d() +
     labs(title="Penetración de tipo de App por estrato (Gen Z y Millenial)", 
       caption="Elaboración propia con datos de la ENDITUH",
       fill="Estrato Socio-Económico", x="Tipo App")#Gev vs Gen

#Gev vs Gen

#Gráficando barras

ggplot(MCO, aes(Grupo,Respuesta,fill=reorder(Tipo, -Respuesta))) + 
         geom_bar(stat="identity", position = "dodge") +
  scale_fill_viridis_d() + scale_y_percent() + 
  theme_minimal() #Gev vs Gen

ac<-ggplot(MCO, aes(reorder(Tipo, -Respuesta),Respuesta,fill=(Grupo))) + 
  geom_bar(stat="identity", position="dodge") +
 scale_y_percent() + 
  theme_minimal() +
  labs(title="Penetración de tipo de App entre Gen Z y Millenial", 
       caption="Elaboración propia con datos de la ENDITUH",
       fill="Generación", x="Tipo App")#Gev vs Gen

ggplot(MCO, aes(Grupo,Respuesta,fill=reorder(Tipo, -Respuesta))) + 
  geom_bar(stat="identity", position = "dodge") +
facet_wrap(~Tipo)  +
  scale_fill_viridis_d() + scale_y_percent() + 
  theme_minimal() #Gev vs Gen

#Juntandolas
grid.arrange(ac,ab)

#Pairs correlograma
pairs(strat)

#Regresion Mco
edad<-lm(Horas~Edad, data=Mco)
summary(edad)
#Boxplot
ggplot(na.omit(Mco), aes(Edad, Horas, fill=Grupo)) + 
geom_boxplot() + scale_fill_brewer(palette = 1) +
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0,12,2)) +
  scale_x_continuous() + 
  labs(title="Distribución de horas en internet por grupo generaciónal",
       caption="Elaboración propia con datos de la ENDUTIH")

#MCO
Mco$Horas<-as.character(Mco$Horas)
ggplot(Mco, aes(Edad,Horas)) + geom_point() + theme_minimal() 

Corr<-lm(data=Mco, Horas~Edad)
summary(Corr
        )
stargazer(Corr)

##Tabla LaTex
Tabla1<-data.frame("Tipo App"=c("Mensajería Instantanea", "Música streaming",
                              "Video Streaming", "Taxi App", "Pagos App", "-"),
"Red social"=c("Facebook","Instagram","Snapchat","Twitter","Whastapp",
                "Youtube"))
stargazer(Tabla1)

#Densidad

### Distribución comparada Gen Z y Millenial entre las horas pasadas en internet
# Y compras en linea, boxplot comparando
#BarPlot


### Heat Map -- Relación uso de tipos de Apps por Edad

#Filtrando preguntas sobre apps
