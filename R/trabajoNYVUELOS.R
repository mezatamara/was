#5.2.4 ej 1 :
library(nycflights13)
library(tidyverse)
library(readr)
library(dplyr)
nycflights13::flights
fl<-nycflights13::flights
  flny<- fl%>%
  #1
  flny1<- fl%>%
  filter(arr_delay>119)         # vuelos con retraso de 2+ horas
  #2
  flny2<- fl%>%
  filter(dest=="IAH"|dest=="HOU" ) # vuelos a Houston
  #3
  flny3<- fl%>%
  filter(carrier=="AA"|carrier=="DL"|carrier=="UA") 
  #4
  flny4<- fl%>%
  filter(month%in%c(7,8,9))
  #5
  flny5<- fl%>%
  filter(arr_delay>119&dep_delay==0)
  #6
  flny6<- fl%>%
  filter(arr_delay<=60 & air_time<=30)
  #7 <--- ej 2
  flny7<- fl%>%
  filter(between(hour, 0,6)) 
  
#--------------------------------------------------------------------------
#5.3.1
  #1
  flny<- fl%>%
    arrange(desc(is.na(dep_delay)))       #ordenar por NA
                                                 
  #2
  fl_Mrsts<-arrange(fl,desc(dep_delay))  #mas retrasado
  fl_Madlts<-arrange(fl,dep_delay)       #mas adelnatado

  #3
  fl_airtime<-arrange(fl,air_time) #vuelo mas rapido velocidad 

  #4
  fl_distanceC<-arrange(fl,distance)    #vuelo mas corto 
  fl_distanceL<-arrange(fl,desc(distance)) #vuelo mas largo

#--------------------------------------------------------------------------
#5.4.1
  #2
  fl_select<-select(flights, year, day, year, year, day, dest, dep_delay) 
  #What happens if you include the name of a variable multiple times in a select() call?
      #RTA: Cualquier variable duplicada solo se incluye una vez.
  
  #3
  vars<-c("year", "month", "day", "dep_delay", "arr_delay")
  vars_tab<-select(fl, one_of(vars))
  #Why might it be helpful in conjunction with this vector?
      #RTA:: Porque con esta funcion es mas facil crear vectore que tengan caracteres y que tengan nombres de variables.
  
  #4
  fl_4.4<-select(fl, contains("time"))
  #Does the result of running the following code surprise you?
      #RTA: si me sorprende xd, no sabia que mediente una linea de codigo se podia realizar una "busqueda" de esta forma.
  
  fl_4.4_ch<-select(flights, contains("time",ignore.case = FALSE)) # no se qure hace :v
---------------------------------------------------------------------
#5.5.2
    #1
    fl_t <- mutate(fl,
            dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
            sched_dep_time_mins = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440
    )
    #2 
#What do you expect to see?
      #RTA:: Espero que air_time sea la diferencia entre llegada y salida. 
fl_air_time <- mutate(fl, dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                      arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
                      air_time2 = air_time - arr_time + dep_time)
#What do you see?
#What do you need to do to fix it?
      #RTA:: la relación entre air_time, arr_time, y dep_time es: air_time <= arr_time - dep_time
            #suponiendo que la zona horaria de arr_time y dep_time son iguales.
------------------------------------------------------------------------
#5.6.7
    #1
  # En muchos escenarios, el retraso de llegada es más importante. En la mayoría de los casos,
  # llegando tarde es más costoso para el pasajero, ya que podría interrumpir las próximas 
  # etapas de su viaje, como la conexión de vuelos o reuniones programadas.
  # Si una salida se retrasa sin afectar la hora de llegada,este retraso no tendrá esos planes
  # y tampoco afecta el tiempo total que pasa viajando.
-------------------------------------------------------------------
#5.7.1
    #2
  fl<-nycflights13::flights
flny<- fl%>%
  filter(!is.na(tailnum)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(min_rank(on_time) == 1)
      


  
  