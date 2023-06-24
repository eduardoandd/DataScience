calculo_notas <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t){
  
  #MÉDIA
  soma_notas <- a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t
  media_notas <- soma_notas/20
  
  cat("A Média das notas é:" ,media_notas, " |")
  
  #MEDIANA
  lista_notas <- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
  tamanho_lista <- length(lista_notas)
  lista_rol <- sort(lista_notas)
  
  if(tamanho_lista%%2==0){
    calculo_mediana <- mean(lista_rol[(tamanho_lista)/2]+0:1)
    
    cat("O calculo da mediana para números pares é: ", calculo_mediana, " |")
  }
  else{
    calculo_mediana <- lista_rol[(tamanho_lista)/2]
    cat("O calculo da mediana por numeros impares é:", calculo_mediana, "|")
  }
  
  #PEGAR A MÉDIA DAS 50% MENORES NOTAS
  menores_notas <- mean(lista_rol[0:(tamanho_lista/2)])
  cat("A média das 50% menores notas são: ", menores_notas, " |")
  
  #PEGAR A MÉDIA DAS 50% MAIORES NOTAS
  maiores_notas <- mean(lista_rol[((tamanho_lista/2)+1):tamanho_lista])
  cat("A média das 50% MAIORES notas são: ", maiores_notas, " |")
  
}

calculo_notas(1,5,6,9,10,8,9,0,4,8,10,9,7,10,10,3,5,6,7,8)


calculo_idade <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) {
  soma_idades <-a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s
  media_idade <- soma_idades/20
  
  cat("A Média das idades é:" ,media_idade, " |")
  
  #MEDIANA
  lista_idades <- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
  tamanho_lista <- length(lista_idades)
  rol_idades <- sort(lista_idades)
  
  if(tamanho_lista%%2==0){
    calculo_mediana<- mean(rol_idades[(tamanho_lista/2)+0:1])
    cat("O calculo da mediana para números pares é: ", calculo_mediana, " |")
    
  }
  else{
    calculo_mediana<- mean(rol_idades[(tamanho_lista/2)])
    cat("O calculo da mediana para números impares é: ", calculo_mediana, " |")
    
  }
}

calculo_idade(15,15,20,30,10,13,12,21,43,33,17,22,17,18,15,16,20,30,28)


#TRAZENDO OS VETORES PARA FORA DAS FUNÇÕES
notas<- c(1,5,6,9,10,8,9,0,4,8,10,9,7,10,10,3,5,6,7,8)
idades<- c(15,15,20,30,10,13,12,21,43,33,17,22,17,18,15,16,20,30,28,29)

df <- data.frame(notas,idades)

#ORDENAR EM FUNÇÃO DAS IDADES
library(dplyr)
df_rol_idades <- df[order(df$idades), ]

#OBTENDO A MEDIA DAS 50% MENORES IDADES
df_menor <- head(df_rol_idades, nrow(df_rol_idades)/2)
media_notas_menor <- mean(df_menor$notas)
cat("A média de notas das 50% menores idades é: ", media_notas_menor, "| ")

#OBTENDO A MEDIA DAS 50% MAIORES IDADES
df_maior <-tail(df_rol_idades, nrow(df_rol_idades)/2)
media_notas_maior <- mean(df_maior$notas)
cat("A média de notas das 50% menores idades é: ", media_notas_maior, "| ")

#HA RELAÇÃO ENTRE IDADE E NOTA?
diferenca_porcentual_notas <-((9.1/4.4)-1)*100
cat("A diferença porcentual entre as maiores e as menores notas é: ",diferenca_porcentual_notas,'%' )

diferenca_porcentual_idade<-((7.3/6.2)-1)*100
cat("A diferença porcentual entre as maiores e as menores idades é: ",diferenca_porcentual_idade,'%' )
#R:AS NOTAS VARIAM EM UMA FAIXA MUITO MAIOR.

#COEFICIENTE DE VARIAÇÃO
cv_notas<- (sd(notas)/mean(notas)) * 100
cat("Coeficiente de variação das notas é: ", cv_notas, "|")
cv_idades<- (sd(idades)/mean(idades))* 100
cat("Coeficiente de variação das idades é: ", cv_idades, "|")
#NOVAMENTE CHEGAMOS A CONCLUSÃO Q A NOTA VARIA MAIS.

#MEDIDA AVANÇADA
corelacao<-cor(idades,notas, method = "pearson")
cat("A correlação é: ",corelacao)
#QUER DIZER QUE QUANTO MAIOR A NOTA, MAIOR A IDADE








