# WDI - DEVELOPMENT INDICATORS 
# OS DADOS DO BANCO MUNDIAL SÃO ACESSADOS 

#install.packages("WDI") 
library(WDI) # SEMPRE CHAMAR O PACOTE 
# PROCUREM AS VINGNETTES
# PÁGINAS COM INSTRUÇÕES DOS PACOTES 

#PIB (US$ corrente)(NY.GDP.MKTP.CD)
#PIB, PREÇOS CORRENTES EM DÓLAR NORTE-AMERICANO
# CÓDIGO: NY.GDP.MKTP.CD

# VAMOS PESQUISAR O TERMO 'GDP' NO WDI
varpib <- WDIsearch('gdp') # CRIAR OBJETO COM O RES.

# BUSCAR OS DADOS COM O CÓDIGO DO SITE 

options(scipen = 999) # AJUSTA A NOTAÇÃO CIENTÍFICA
dadospib <- WDI(country = 'all',
                indicator = 'NY.GDP.MKTP.CD') 

dadospib2023 <- WDI(country = 'all',
                indicator = 'NY.GDP.MKTP.CD',
                start = 2023, end = 2023)
