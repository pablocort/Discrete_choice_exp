### Pablo Cortés-Sánchez


# Limpiar la consola, el entorno y fijar directorio de trabajo
cat('\f')
rm(list=ls())
options('scipen'=100, 'digits'=4) # Forzar a R a no usar e+
setwd("~/CoreWoman/GRADE - Behavioral science/experimental design")
getwd()

#### 0.2. Instalar las librerias que vamos a usar en la clase de hoy
paquetes = c("tidyverse",'dplyr', 'idefix' )
for ( paquete in paquetes){
  if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete) ; print(paste0('La libreria ', '"', paquete ,'"', ' ha sido instalada.'))}
  else { print(paste0('La libreria ', '"', paquete ,'"', ' ya está instalada.'))}
  rm(paquete)
}
#### Llamar las librerias
sapply(paquetes,require,character.only=T) 
rm(paquetes)



#### Discrete Choice Experiment 
###########################################
###########################################



#numero de niveles por atributro
at.lvls <- c(2, 2, 4)

#tipo de atributo
c.type <- c("D", "D","D")
#niveles del atributo continuo
#con.lvls <- list(c(1.5, 1.65, 1.8, 2.0))


# opciones contantes (ninguna de las anteriores)
alt.cte <- c(0,0,0,1)

#creación de todos las canastas posibles 
cs <- Profiles(lvls = at.lvls, coding = c.type)

# vector de priors
m <- c(-0, 1, 0, 1, 1, 1)

# varianzas para la distribución
v <- diag(length(m))

#semillas para aleatorización
set.seed(123)

# distribución
ps <- MASS::mvrnorm(n = 500, mu = m, Sigma = v)
ps <- list(ps[,1:1], ps[,2:6])

# Iteración sobre priors
D.nc <- Modfed(cand.set = cs, n.sets = 6, n.alts = 4, 
               alt.cte = alt.cte, par.draws = ps, no.choice = TRUE,
               best = FALSE)

D.nc

for (i in 1:length(D.nc)) print(D.nc[[i]]$error)


# niveles de cada atributo
lvls <- list(
  c("Con beneficios", "sin beneficios"), 
  c("virtual", "presencial"),
  c('$1.5', '$1.6', '$1.7','$1.8')) 


test <- Decode(des = D.nc[[1]]$design, n.alts = 4,
               lvl.names = lvls, alt.cte = alt.cte, 
               coding = c.type, no.choice = 4)
test

cbind(test$design, probs = as.vector(t(D.nc[[1]]$probs)))




