# Audrey Carolina López Carmona
# FUNCIONES ANUALIDADES

print(paste0("ANUALIDADES VENCIDAS"))
print(paste0("Para VF Conocido:"))

VFVenc=function( A , r , n ){
  VFVenc=A*(((1+r)^n)-1)/r
  return(VFVenc)
}
pagoVFVenc=function(VF,r,n){
  pagoVFVenc=VF/((((1+r)^n)-1)/r)
  return(pagoVFVenc)
}
tasaVFVenc=function(VF,A,n,umbral=10*(10^-18)){
  r=0.01
  iteraciones=0
  
  izquierda=VF/A
  derecha=(((1+r)^n)-1)/r
  diferencia=izquierda-derecha
  
  if (diferencia > 0){
    #Cuando la r inicial es menor a lo deseado:
    #Algoritmo de alza de tasa:
    
    iteraciones=0
    r0=r
    while (diferencia > 0){
      r1=r
      iteraciones=iteraciones+1
      r=r*1.01
      
      izquierda=VF/A
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      cat("\f")
      print(paste0("Iteracion de alza de tasa:",iteraciones))
      r2=r
    }
    #diferencia1=diferencia
    diferenciaTasa=r-r0
    diferencia=umbral
    #Algoritmo de la bisección:
    while(abs(diferenciaTasa)>umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2
      
      izquierda=VF/A
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      
      if (diferencia>0){
        r1=r1
        r2=r
      }
      else{
        r2=r2
        r1=r
      }
      diferenciaTasa=r-r0
      r0=r
      cat("\f")
      print(paste0("Iteración de bisección: ", iteraciones))
    }
  } else{
    #Cuando la r inicial es mayor a lo deseado:
    #Algoritmo de bajada de tasa:
    iteraciones=0
    r0=r
    while(diferencia < 0){
      r2=r
      iteraciones=iteraciones+1
      r=r/1.01
      
      izquierda=VF/A
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      #cat("\f")
      #print(paste0("Iteración de baja de tasa:",iteraciones))
      r1=r
    }
    #diferencia1=diferencia
    diferenciaTasa=r-r0
    r0=r
    diferencia=umbral
    #Algoritmo de la bisección:
    while(abs(diferenciaTasa)> umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2
      
      izquierda=VF/A
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      
      if (diferencia > 0){
        r1=r
        r2=r2
      }
      else{
        r2=r
        r1=r1
      }
      diferenciaTasa=r-r0
      r0=r
      #cat("\f")
      #print(paste0("Iteración de bisección:"), iteraciones))
    }
  }
  #Genera objeto de salida:
  objetoTasa=list(Iteraciones=iteraciones, tasa=r)
  #cat("\f")
  #print(paste0("Total de iteraciones realizadas:"), iteraciones))
  return(objetoTasa)
}
periodosVFVenc=function( VF , A , r ){
  periodosVFVenc= log ( ( ( VF * r ) / A ) + 1 )/ log ( 1 + r )
  return(periodosVFVenc)
}

#DATOS EJERCICIO
A=500
r=0.003125   
n=960       


#Cálculos

VFVencCal=VFVenc( A , r , n )
cat(" VFVenc: ", VFVencCal, "\n")

pagoVFVenc=pagoVFVenc(VFVencCal , r , n)
cat("pagoVFVenc:", pagoVFVenc,"\n")

tasaVFVenc=tasaVFVenc(VFVencCal, A , n)
cat("tasaVFVenc", tasaVFVenc$tasa, "\n")
#cat("Iteraciones realizadas: ", tasaVFVenc$Iteraciones, "\n")

periodosVFVenc=periodosVFVenc(VFVencCal, A , r )
cat("periodosVFVenc: " , periodosVFVenc, "\n")

print(paste0("Para VA Conocido:"))
VAVenc = function( A , r , n){
  VAVenc=A*((1-(1+r)^(-n))/r)
  return(VAVenc)
}
pagoVAVenc = function(VA , r , n){
  pagoVAVenc= VA/((1-(1+r)^-n)/r)
  return(pagoVAVenc)
} 
tasaVAVenc= function(VA, A , n , umbral=10*(10^-18)){
  r=0.001
  iteraciones=0
  
  izquierda=VA/A
  derecha=(1-((1+r)^-n))/r
  diferencia=izquierda-derecha
  
  if (diferencia < 0){
    #Cuando la r inicial es menor a lo deseado:
    #Algoritmo de alza de tasa:
    
    iteraciones=0
    r0=r
    while (diferencia < 0){
      r1=r
      iteraciones=iteraciones+1
      r=r*1.01
      
      izquierda=VA/A
      derecha=(1-((1+r)^-n))/r
      diferencia=izquierda-derecha
      #cat("\f")
      #print(paste0("Iteracion de alza de tasa:",iteraciones))
      r2=r
    }
    #diferencia1=diferencia
    diferenciaTasa=r-r0
    diferencia=umbral
    #Algoritmo de la bisección:
    while(abs(diferenciaTasa)>umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2
      
      izquierda=VA/A
      derecha=(1-((1+r)^-n))/r
      diferencia=izquierda-derecha
      
      if (diferencia>0){
        r1=r1
        r2=r
      }
      else{
        r2=r2
        r1=r
      }
      diferenciaTasa=r-r0
      r0=r
      #cat("\f")
      # print(paste0("Iteración de bisección: ", iteraciones))
    }
  } else{
    #Cuando la r inicial es mayor a lo deseado:
    #Algoritmo de bajada de tasa:
    iteraciones=0
    r0=r
    while(diferencia > 0){
      r2=r
      iteraciones=iteraciones+1
      r=r/1.01
      
      izquierda=VA/A
      derecha=((1-(1+r)^-n))/r
      diferencia=izquierda-derecha
      #cat("\f")
      #print(paste0("Iteración de baja de tasa:",iteraciones))
      r1=r
    }
    #diferencia1=diferencia
    diferenciaTasa=r-r0
    r0=r
    diferencia=umbral
    #Algoritmo de la bisección:
    while(abs(diferenciaTasa)> umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2
      
      izquierda=VA/A
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      
      if (diferencia > 0){
        r1=r
        r2=r2
      }
      else{
        r2=r
        r1=r1
      }
      diferenciaTasa=r-r0
      r0=r
      #cat("\f")
      #print(paste0("Iteración de bisección:"), iteraciones))
    }
  }
  #Genera objeto de salida:
  objetoTasa=list(Iteraciones=iteraciones, tasa=r )
  #cat("\f")
  #print(paste0("Total de iteraciones realizadas:"), iteraciones))
  return(objetoTasa)
}
periodosVAVenc= function (VA, A, r){
  periodosVAVenc=-log(1-((VA*r))/A)/log(1+r)
  return(periodosVAVenc)
}

#DATOS EJERCICIO
A=450
n=24
r=0.11/12
#Cálculos

VAVencCal=VAVenc( A , r , n )
cat("VAVenc: ", VAVencCal, "\n")
tasaVAVenc=tasaVAVenc(VAVencCal, A , n)
cat("tasaVAVenc: ", tasaVAVenc$tasa, "\n")
#cat("Iteraciones realizadas: ", tasaVAVenc$Iteraciones, "\n")

pagoVAVenc=pagoVAVenc(VAVencCal , r , n)
cat("pagoVAVenc: ", pagoVAVenc,"\n")


periodosVAVenc=periodosVAVenc(VAVencCal, A , r )
cat("periodosVAVenc: " , periodosVAVenc, "\n")

print(paste0("ANUALIDADES ANTICIPADAS:"))
print(paste0("Para VF conocido:"))

VFAnt= function (A, r, n){
  VFAnt= A*(((1+r)^n-1)/r)*(1+r)
  return(VFAnt)
}
pagoVFAnt=function (VF,r,n){
  pagoVFAnt=(VF*r)/(((1+r)^n)-1)
  return(pagoVFAnt)
}
periodosVFAnt=function(VF , A, r){
  periodosVFAnt=log(((VF * r )/ (A* (1+r)))+1)/log(1+r)
  return(periodosVFAnt)
}
tasaVFAnt=function(VF,A,n,umbral=10*(10^-18)){
  r=0.01
  iteraciones=0
  
  izquierda=VF/(A*(1+r))
  derecha=(((1+r)^n)-1)/r
  diferencia=izquierda-derecha
  
  if (diferencia > 0){
    #Cuando la r inicial es menor a lo deseado:
    #Algoritmo de alza de tasa:
    
    iteraciones=0
    r0=r
    while (diferencia > 0){
      r1=r
      iteraciones=iteraciones+1
      r=r*1.01
      
      izquierda=VF/(A*(1+r))
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      #cat("\f")
      #print(paste0("Iteracion de alza de tasa:",iteraciones))
      r2=r
    }
    #diferencia1=diferencia
    diferenciaTasa=r-r0
    diferencia=umbral
    #Algoritmo de la bisección:
    while(abs(diferenciaTasa)>umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2
      
      izquierda=VF/(A*(1+r))
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      
      if (diferencia>0){
        r1=r1
        r2=r
      }
      else{
        r2=r2
        r1=r
      }
      diferenciaTasa=r-r0
      r0=r
      #cat("\f")
      #print(paste0("Iteración de bisección: ", iteraciones))
    }
  } else{
    #Cuando la r inicial es mayor a lo deseado:
    #Algoritmo de bajada de tasa:
    iteraciones=0
    r0=r
    while(diferencia < 0){
      r2=r
      iteraciones=iteraciones+1
      r=r/1.01
      
      izquierda=VF/(A*(1+r))
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      #cat("\f")
      #print(paste0("Iteración de baja de tasa:",iteraciones))
      r1=r
    }
    #diferencia1=diferencia
    diferenciaTasa=r-r0
    r0=r
    diferencia=umbral
    #Algoritmo de la bisección:
    while(abs(diferenciaTasa)> umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2
      
      izquierda=VF/(A*(1+r))
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      
      if (diferencia > 0){
        r1=r
        r2=r2
      }
      else{
        r2=r
        r1=r1
      }
      diferenciaTasa=r-r0
      r0=r
      #cat("\f")
      #print(paste0("Iteración de bisección:"), iteraciones))
    }
  }
  #Genera objeto de salida:
  objetoTasa=list(Iteraciones=iteraciones, tasa=r)
  #cat("\f")
  #print(paste0("Total de iteraciones realizadas:"), iteraciones))
  return(objetoTasa)
}

#DATOS EJERCICIO
A=500
r=0.075/24
n=960

#Cálculos

VFAntCal=VFAnt( A , r , n )
cat("VFAnt: ", VFAntCal, "\n")

pagoVFAnt=pagoVFAnt(VFAntCal , r , n)
cat("pagoVFAnt:", pagoVFAnt,"\n")

tasaVFAnt=tasaVFAnt(VFAntCal, A , n)
cat("tasaVFAnt", tasaVFAnt$tasa, "\n")
#cat("Iteraciones realizadas: ", tasaVFAnt$Iteraciones, "\n")

periodosVFAnt=periodosVFAnt(VFAntCal, A , r )
cat("periodosVFAnt: " , periodosVFAnt, "\n")

print(paste0("Para VA conocido:"))

VAAnt= function(A,r,n){
  VAAnt=A*((((1-(1+r)^-n)))/(r))*(1+r)
  return(VAAnt)
}
pagoVAAnt=function(VA, r, n){
  pagoVAAnt=VA*r*(1+r)^n/((1+r)^(n+1)-(1+r))
  return(pagoVAAnt)
}
tasaVAVenc= function(VA, A , n , umbral=10*(10^-18)){
  r=0.001
  iteraciones=0
  
  izquierda=VA/(A*(1+r))
  derecha=(1-((1+r)^-n))/r
  diferencia=izquierda-derecha
  
  if (diferencia < 0){
    #Cuando la r inicial es menor a lo deseado:
    #Algoritmo de alza de tasa:
    
    iteraciones=0
    r0=r
    while (diferencia < 0){
      r1=r
      iteraciones=iteraciones+1
      r=r*1.01
      
      izquierda=VA/(A*(1+r))
      derecha=(1-((1+r)^-n))/r
      diferencia=izquierda-derecha
      #cat("\f")
      #print(paste0("Iteracion de alza de tasa:",iteraciones))
      r2=r
    }
    #diferencia1=diferencia
    diferenciaTasa=r-r0
    diferencia=umbral
    #Algoritmo de la bisección:
    while(abs(diferenciaTasa)>umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2
      
      izquierda=VA/(A*(1+r))
      derecha=(1-((1+r)^-n))/r
      diferencia=izquierda-derecha
      
      if (diferencia>0){
        r1=r1
        r2=r
      }
      else{
        r2=r2
        r1=r
      }
      diferenciaTasa=r-r0
      r0=r
      #cat("\f")
      # print(paste0("Iteración de bisección: ", iteraciones))
    }
  } else{
    #Cuando la r inicial es mayor a lo deseado:
    #Algoritmo de bajada de tasa:
    iteraciones=0
    r0=r
    while(diferencia > 0){
      r2=r
      iteraciones=iteraciones+1
      r=r/1.01
      
      izquierda=VA/(A*(1+r))
      derecha=((1-(1+r)^-n))/r
      diferencia=izquierda-derecha
      #cat("\f")
      #print(paste0("Iteración de baja de tasa:",iteraciones))
      r1=r
    }
    #diferencia1=diferencia
    diferenciaTasa=r-r0
    r0=r
    diferencia=umbral
    #Algoritmo de la bisección:
    while(abs(diferenciaTasa)> umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2
      
      izquierda=VA/(A*(1+r))
      derecha=(((1+r)^n)-1)/r
      diferencia=izquierda-derecha
      
      if (diferencia > 0){
        r1=r
        r2=r2
      }
      else{
        r2=r
        r1=r1
      }
      diferenciaTasa=r-r0
      r0=r
      #cat("\f")
      #print(paste0("Iteración de bisección:"), iteraciones))
    }
  }
  #Genera objeto de salida:
  objetoTasa=list(Iteraciones=iteraciones, tasa=r )
  #cat("\f")
  #print(paste0("Total de iteraciones realizadas:"), iteraciones))
  return(objetoTasa)
}
periodosVAAnt= function(VA, A, r){
  periodosVAAnt=log((A *( 1 + r )^( n + 1 )-A *(1 + r))/(VA * r))/log(1+r)
  return(periodosVAAnt)
} 
#DATOS EJERCICIO
A=450
n=24
r=0.11/12

#Cálculos

VAAntCal=VAAnt( A , r , n )
cat("VAAnt: ", VAAntCal, "\n")
tasaVAAnt=tasaVAVenc(VAAntCal, A , n)
cat("tasaVAAnt: ", tasaVAAnt$tasa, "\n")
#cat("Iteraciones realizadas: ", tasaVAAnt$Iteraciones, "\n")

pagoVAAnt=pagoVAAnt(VAAntCal , r , n)
cat("pagoVAAnt: ", pagoVAAnt,"\n")


periodosVAAnt=periodosVAAnt(VAAntCal, A , r )
cat("periodosVAAnt: " , periodosVAAnt, "\n")

print(paste0("ANUALIDADES DIFERIDAS"))
VADif=function(A,r,n,k){
  VADif=A*((1-(1+r)^-(n-k+1))/r)
  return(VADif)
}
pagoVADif= function(VA, r,n,k){
  pagoVADif=VA/((1-(1+r)^-(n-k+1))/r)
  return(pagoVADif)
}
#DATOS EJERCICIO
A=450
n=24
r=0.11/12
k=3

#Cálculos 
VADifCalc=VADif(A, r, n, k)
cat("VADif: ", VADifCalc, "\n")
pagoVADifCalc= pagoVADif(VADifCalc, r, n, k)
cat("pagoVADif: " , pagoVADifCalc, "\n")