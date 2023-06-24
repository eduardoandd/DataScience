calculo_nota <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) {

  #MÉDIA
  soma_notas<- a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t
  media_notas<- soma_notas/20
  cat("A media das notas é: " ,media_notas, " | ")
  
  #MEDIANA
  lista_notas <- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
  tamanho_lista <- length(lista_notas)
  rol_lista <- sort(lista_notas)
  
  if(tamanho_lista%%2==0){
    mediana<- mean(rol_lista[(tamanho_lista/2)+0:1])
    cat("A mediana para números pares é: " ,mediana, " | ")
  }
  else{
    mediana<- mean(rol_lista[tamanho_lista/2])
    cat("A mediana para números impares é: " ,mediana, " | ")
  }
  
  #MÉDIA DAS 50% MENORES NOTAS
  menores_notas<- mean(rol_lista[0:(tamanho_lista/2)])
  cat("A média das 50% menores notas é: " ,menores_notas, " | ")
  
  #MÉDIA DAS 50% MAIORES NOTAS
  maiores_notas<- mean(rol_lista[((tamanho_lista/2)+1):tamanho_lista])
  cat("A média das 50% maiores notas é: " ,maiores_notas, " | ")
  
}

calculo_nota(5,4,3,10,8,7,7.5,6,6.5,10,9,2,0,7,9,5,4,5,8,8.5)

#IDADES
calculo_idade<- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t){
  #MÉDIA
  soma_idade<- a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t
  media_idade<- soma_idade/20
  cat("A media das idades é: " ,media_idade, " | ")
  
  #MEDIANA
  lista_idade<- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
  tamanho_lista<- length(lista_idade)
  rol_lista<- sort(lista_idade)
  
  if(tamanho_lista%%2==0){
    mediana<- mean(rol_lista[(tamanho_lista/2)+0:1])
    cat("A mediana de idade entre números pares é: " ,mediana, " | ")
  }
  else{
    mediana<- mean(rol_lista[(tamanho_lista/2)])
    cat("A mediana de idade entre números impares é: " ,mediana, " | ")
  }
  
  #MÉDIA DAS 50% MENORES IDADES
  menores_idades<- mean(rol_lista[0:(tamanho_lista/2)])
  cat("A média das 50% menores idades é: " ,menores_idades, " | ")
  
  
  #MÉDIA DAS 50% MAIORES IDADES
  maiores_idades<- mean(rol_lista[((tamanho_lista/2)+1):tamanho_lista])
  cat("A média das 50% maiores idades é: " ,maiores_idades, " | ")
}
calculo_idade(11,13,12,11,14,14,15,16,11,14,14,13,13,15,11,14,12,12,12,15)

idades<- c(11,13,12,11,14,14,15,16,11,14,14,13,13,15,11,14,12,12,12,15)
notas<- c(5,4,3,10,8,7,7.5,6,6.5,10,9,2,0,7,9,5,4,5,8,8.5)

df<- data.frame(idades,notas)

library(dplyr)

df_rol_idades <- df[order(df$idades), ]
df_rol_notas <- df[order(df$notas), ]

#OBTENDO AS 50% MENORES IDADES E TIRANDO A MEDIA DELAS
df_menores_idades<- head(df_rol_idades,nrow(df_rol_idades)/2)
media_menores_idades<- mean(df_menores_idades$notas)
cat("A média entre as 50% menores idades é: ", media_menores_idades, " | ")

#OBTENDO AS 50% MAIORES IDADES E TIRANDO A MEDIA DELAS
df_maior_idade<- tail(df_rol_idades, nrow(df_rol_idades)/2)
media_maior_idade <- mean(df_maior_idade$notas)
cat("A média entre as 50% maiores idades é: ", media_maior_idade, " | ")


#OBTENDO AS 50% MENORES NOTAS E TIRANDO A MEDIA DELAS
df_menor_nota<-head(df_rol_notas, nrow(df_rol_notas)/2)
media_menor_nota<- mean(df_menor_nota$notas)
#cat("A média entre as 50% MENORES notas é: ", media_menor_nota, " | ")


#OBTENDO AS 50% MAIORES NOTAS E TIRANDO A MEDIA DELAS
df_maior_nota<-tail(df_rol_notas, nrow(df_rol_notas)/2)
media_maior_nota<- mean(df_maior_nota$notas)
#cat("A média entre as 50% MAIORES notas é: ", media_maior_nota, " | ")


#HÁ RELAÇÕES ENTRE IDADES E NOTAS?

#DIFERENÇA PORCENTAL
porcentual_notas<- ((8.4/4.05)-1)*100
cat("A diferença porcentual entre as maiores notas e menores notas é: " ,porcentual_notas)

porcentual_idades<-((14.4/11.8)-1)*100
cat("A diferença porcentual entre as maiores idades e menores idades é: " ,porcentual_idades)


#COEFICIENTE DE VARIAÇÃO
cv_idades<- (sd(idades)/mean(idades)) * 100
cat("O coeficiente de variação de idade é: " ,cv_idades, " | ")
cv_notas<- (sd(notas)/mean(notas)) * 100
cat("O coeficiente de variação de nota é: " ,cv_notas, " | ")

#MEDIDA AVANÇADA
corelacao<-cor(idades,notas, method = "pearson")
cat("A correlação é: ",corelacao)







