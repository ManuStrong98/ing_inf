# NOMBRES:     Copa Quiñonez Jose Manuel, Valdivia Lopez Emily
# Codigo SIS:  201706502, 202107794
# Carrera:     Ing Informatica
# Fecha:       6 de diciembre 2021

####### OBJETIVOS:   MRU, MRUV, CINMETICA

################### IMPORTANDO
   
   c = JoseManuel
   
############      2. GENERANDO GRAFICAS

############         2.1 posición en función del tiempo
   # MRU
      plot(c$t, c$MRU)
   
   #MRUV
      plot(c$t, c$MRUV)

############      3. DETERMINAR LAS CURVAS DE AJUSTE, MC 
   # MC MRU
      LMU = lm(c$MRU~c$t)
      summary(LMU)
      plot(LMU)
      abline(LMU, col="green")
   
   # MC MRUV
      
      f = lm(c$MRUV~poly(x = c$t, degree = 2))
      f = lm(c$MRUV~c$t)
   f = lm(c$MRU~c$t)
   f = lm(c$MRU~c$t)
   f = lm(c$MRU~c$t)
   g = lm(c$MRUV~c$t)
   plot(f)
   lines(c$t, predict(f))
   summary(f)
   
   abline(f, col = "red")
   abline(g)

   
   ################ MC
   z = (c$t)^2
   y = c$MRUV
   a_m = (sum(y) * sum(z^2) - sum(z*y) * sum(z)) / (
      length(y) * sum(z^2) - (sum(z))^2)
   
   b_m = (length(y) * sum(z*y) - sum(z) * sum(y)) / (
      (length(y) * sum(z^2) - (sum(z))^2)   )
   yrecta = a_m + b_m * z    
   #plot(c$t, yrecta)
   lines(c$t, yrecta, col= "BLUE")
plot(c$t, yrecta)   
