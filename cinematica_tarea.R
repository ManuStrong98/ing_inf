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
      
      
      z = (c$t)^2
      y = c$MRUV
      
      a_m = (sum(y) * sum(z^2) - sum(z*y) * sum(z)) / (
         length(y) * sum(z^2) - (sum(z))^2)
      
      b_m = (length(y) * sum(z*y) - sum(z) * sum(y)) / (
         (length(y) * sum(z^2) - (sum(z))^2)   )
      yrecta = a_m + b_m * z    
      
      
      lines(c$t, yrecta, col= "BLUE")
      
