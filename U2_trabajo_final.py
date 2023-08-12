# -*- coding: utf-8 -*-
"""
Created on Fri Aug 11

@author: Leonardo Cervetto
"""

# =============================================================================
# Librerias
# =============================================================================
#%%%
import pandas as pd
import numpy as np
import yfinance as yf
import statsmodels.api as sm
import matplotlib.pyplot as plt 
from pypfopt import plotting
from pypfopt import risk_models
from sklearn.linear_model import LinearRegression
from pandas_datareader.nasdaq_trader import get_nasdaq_symbols


from pypfopt import EfficientFrontier
from pypfopt import risk_models
from pypfopt import expected_returns


#%%
# =============================================================================
# Parte obligatoria NASDAQ
# =============================================================================

# Problemas al correr el código
symbols = get_nasdaq_symbols()

# Alternativa (tampoco funcionó)
nasdaq_tickers = yf.Tickers('^IXIC')  





########################################
###### Propuesta de solución ###########
########################################

'''
1. Use Power Query de Excel para extraer los tickers del NASDAQ directamente de la url de Investing.com (https://es.investing.com/indices/nq-100-components)
2. Guardé el archivo como CSV y lo subí a GitHub 

'''


# Extracción
url = "https://raw.githubusercontent.com/leo-cerv/Uch/main/NASDAQ.csv"
NASDAQ = pd.read_csv(url)


# Filtrar por la inicial del nombre
letra_filtro = 'L'

# Filtrar el DataFrame
strings_filtrados = NASDAQ[NASDAQ['NASDAQ'].str.startswith(letra_filtro)]

# valor random
resultado = np.random.choice(strings_filtrados['NASDAQ'])

# resultado
print("La acción que te corresponde es: "+ resultado)




#########################################
# Copia de la función_my_nasdaq_symbols #
#########################################

# función para hacer reemplazar get_nasdaq_symbols()

def get_my_nasdaq_symbols(search = None):
    
    url = "https://raw.githubusercontent.com/leo-cerv/Uch/main/NASDAQ.csv"
    NASDAQ = pd.read_csv(url)
    
    # Filto
    letra_filtro = search
    strings_filtrados = NASDAQ[NASDAQ['NASDAQ'].str.startswith(letra_filtro)]

    return(strings_filtrados)
    
# Test 
get_my_nasdaq_symbols(search = "E")
get_my_nasdaq_symbols(search = "Z")



# hacer función con resultado random 
def get_my_nasdaq_random(search = None):

   url = "https://raw.githubusercontent.com/leo-cerv/Uch/main/NASDAQ.csv"
   NASDAQ = pd.read_csv(url)

   # Filtro
   letra_filtro = search
   strings_filtrados = NASDAQ[NASDAQ['NASDAQ'].str.startswith(letra_filtro)]

   # valor random
   resultado = np.random.choice(strings_filtrados['NASDAQ'])
   
   return print("La acción que te corresponde es: "+ resultado)

# Test
get_my_nasdaq_random(search = "E")
get_my_nasdaq_random(search = "A")











#%%
# =============================================================================
# Pregunta 2
# =============================================================================

################# A)

'''
Acciones a utilizar que comienzan con la inicial de mi nombre:
    
LYB = "LyondellBasell Industries NV"
LVS = "Las Vegas Sands Corp"
LH = "Laboratory Corporation of America Holdings"

'''

# Extracción
data = yf.download(tickers = "LH LVS LYB",  
            period = "2y",        
            interval = "1d",       
            prepost = False,       
            repair = True) 

# Lpimpieza 
data = data.iloc[:, [0,1,2]]
data.columns = data.columns.droplevel(0)





################# B)

# Covarianza para verificación de correlación
LH = data["LH"]
LVS = data["LVS"]
LYB = data["LYB"]

# Matriz covarianza
arr_cov = np.array([LH , LVS, LYB])
covar = np.cov(arr_cov , bias = True)

# Matriz correlación
risk_models.cov_to_corr(covar)

### RESP:
# Son de sectores diferentes. correlaciónes <0.6





################# C)

# Retornos
data_r = data.pct_change()
data_r = data_r.drop(data_r.index[0])


# Vectores de retornos
LH_R = data_r["LH"]
LVS_R = data_r["LVS"]
LYB_R = data_r["LYB"]

# Matriz covarianza de los retornos
arr_cov_r = np.array([LH_R , LVS_R, LYB_R])
covar_r = np.cov(arr_cov , bias = True)

# Matriz correlación de los retornos
risk_models.cov_to_corr(covar_r)





################# D)

# Se comprueba que ambas matrices de correlación son iguales
risk_models.cov_to_corr(covar)
risk_models.cov_to_corr(covar_r)





################# E) & F)




# Parámetros
e = data_r.mean(axis = 0)
V = pd.DataFrame.cov(data_r)
corr = pd.DataFrame.corr(data_r)
corr = risk_models.cov_to_corr(V) 



# Frontera eficiente
ef = EfficientFrontier(e, V,weight_bounds=(-0.5, 1.5)) 
w = ef.max_sharpe(risk_free_rate=0.001)
w1 = ef.clean_weights() 


# Portafolio eficiente
performance = ef.portfolio_performance(verbose=True,risk_free_rate = 0.001)
ef = EfficientFrontier(e, V, weight_bounds=(-0.5, 1.5))


# Plot
fig, ax = plt.subplots() 
plotting.plot_efficient_frontier(ef, ax=ax, show_assets=True,show_tickers = True)
ax.set_title("Frontera eficiente con 3 activos")
ax.legend()
plt.xlabel('Riesgo')
plt.ylabel('Retorno')
plt.show()







#%%
# =============================================================================
# Pregunta 3
# =============================================================================



################# A) & B)

datos = yf.download(tickers = "LH ^TNX ^GSPC",  
            period = "2y",        
            interval = "1d",       
            prepost = False,       
            repair = True) 








################# C)

# Orden de los datos
datos = datos.iloc[:, [0,1,2]]
datos.columns = datos.columns.droplevel(0)

# Ajuste tasa libre de riesgo 
tasa    =    datos['^TNX']/100/360 
datos.insert(2, "Tasa", tasa, True)


# Obtener los retornos
datos_r = datos.pct_change()
datos_r = datos_r.drop(datos_r.index[0])


datos_r['r_rf'] = datos['LH'] -  datos['Tasa']
datos_r['rm_rf'] = datos['^GSPC'] - datos['Tasa']







################# D)

# Regresión lineal con statsmodels

x = datos_r['LH']
x = sm.add_constant(x)

y = datos_r['^GSPC']

# modelos
results = sm.OLS(y,x).fit()

# resultados
print(results.summary())







# Regresión lineal con sklearn

modelo = LinearRegression()

# Ajustar el modelo a los datos
x = np.array(datos_r['LH'])
y = np.array(datos_r['^GSPC'])

# Ajustar el modelo a los datos
modelo.fit(x.reshape(-1, 1), y)

# Predecir valores usando el modelo ajustado
y_pred = modelo.predict(x.reshape(-1, 1))

# Imprimir los coeficientes de la ecuación de regresión
print("Coeficiente:", modelo.coef_[0])
print("Intercepto:", modelo.intercept_)



import matplotlib.pyplot as plt
plt.scatter(x, y, label="Datos reales")
plt.plot(x, y_pred, color='red', label="Regresión lineal")
plt.xlabel("Variable independiente")
plt.ylabel("Variable dependiente")
plt.legend()
plt.show()






# ==============================================================================






"""
->  Diplomado de aplicaciones de análisis de datos 
    para las decisiones financieras

  
->  Trabajo final curso de:
    "Analisis con Python"
            PARTE II 
            
            
            
Created on Wed Aug  9 
@author: Leonardo Cervetto

"""

'''
########################################################
## Conversor de Ethereum a pesos chilenos ##
########################################################

Este proyecto es una aplicación la cual ayuda a los 
desarrolladores DeFi (Finanzas descentralizadas) / Web3 calcular 
el costo de las funciones asociadas a los Smart Contracts
desplegados en la red de Ethereum en pesos chilenos (cotización actual)

Además, incluye funciones que permiten hacer conversiones de diferentes
unidades de Ethereum (como wei, gwei, finney) a pesos chilenos

Como valor agregado, está el desarrollo de una función que permite obtener 
estadígrafos para la cotización semanal, mensual, semestral y anual de Etheruem.
Además, de dos funciones que grafican series temporales y distribución de los retornos
para los mismos periodos.



'''

# =============================================================================
# Importación de librerias
# =============================================================================
import pandas as pd
import numpy as np
import yfinance as yf 
from datetime import datetime
import matplotlib.pyplot as plt




#%%
# =============================================================================
# Descarga, seleción y limpieza de la cotización de Ethereum-USD
# =============================================================================

# Descarga de la data de Yahoo finance:
    # Extracción de cotización diaria de un año de Ethereum - Dólar Americano
    
ETH = yf.download(tickers = "ETH-USD",  
            period = "1y",         
            interval = "1d",       
            prepost = False) 

# Se seleciona solamente el valor de cierre filtrando la columna "Close"
ETH = ETH["Close"]

# Definimos los retornos con la funcion "pct_change()"
ETH_R = ETH.pct_change() 

# Se guarda el valor de la ultima observación, correspondiente a la cotización actual al momento de correr la función
last_ETH = ETH.tail(1)[0].round(2)




#%%
# =============================================================================
# Descarga, seleción y limpieza de la conversión USD-CLP
# =============================================================================

# Decarga de la data de Yahoo finance:
 # Extracción de cotización diaria de un año de Dólar Americano - Peso Chileno
CLP_USD = yf.download(tickers = "USDCLP=X",  
            period = "1y",         
            interval = "1d",       
            prepost = False) 

# Se seleciona solamente el valor de cierre filtrando la columna "Close"
CLP_USD = CLP_USD["Close"]

# Se guarda el valor de la ultima observación, correspondiente a la cotización actual al momento de correr la función
last_CLP = CLP_USD.tail(1)[0].round(0)



 
#%%

# =============================================================================
# Funciones para convertir unidades de Ethereum
# =============================================================================

'''
Cada función que almacena información una Blockchain a través de un Contrato inteligente,
tiene un costo asociado que puede ser calculado en diferentes unidades de medida asociadas a Ethereum

Se desarrollaron tres funciones para las más usadas


'''
# =============================================================================
# Wei
# =============================================================================

# Función para obtener el valor de 1 Wei en ETH:
    # 1 Wei corresponde a 0.000000000000000001 ETH

def to_wei(x = last_ETH):
    wei = x / 1000000000000000000
    return(wei)

#%%
# =============================================================================
# Gwei
# =============================================================================

# Función para obtener el valor de 1 Gwei en ETH:
    # 1 Gwei corresponde a 0.0000000001 ETH

def to_gwei(x = last_ETH):
    gwei = x / 1000000000
    return(gwei)

#%%
# =============================================================================
# Finney
# =============================================================================

# Función para obtener el valor de 1 Finney en ETH:
    # 1 Finney corresponde a 0.001 ETH
    
def to_finney(x = last_ETH):
    finney = x / 1000
    return(finney)




 #%%
# =============================================================================
# Unidades de ETH convertidad a pesos chileno
# =============================================================================
 
'''
Como valor agregado, se crea un mensaje el cual entrega un resumen general del precio
de unidades de Ethereum a peso chileno. Este código puede ser muy útil para montarlo en una GUI

'''

# Calculo de los parametros:

# Precio 1 ETH a Clp$
precio_ETH = last_CLP * last_ETH

# fecha actual
fecha_actual = datetime.now()
fecha_corta = fecha_actual.strftime("%d/%m/%Y")

# wei a Clp$
wei = to_wei(precio_ETH)

# gwei a Clp$
gwei = to_gwei(precio_ETH)

# Finney a Clp$
finney = to_finney(precio_ETH)

# Mensaje final parametrizado
print("Con fecha: "+ fecha_corta + "\n" +
      "El precio de 1 Wei es de " + str(wei) + " pesos chilenos" + "\n"+
      "El precio de 1 Gwei es de " + str(gwei) + " pesos chilenos" +"\n"+
      "El precio de 1 Finney es de "+ str(finney)+ " peso chilenos"+ "\n"+
      "\n"+
      "Considerar que:" + "\n"+
      "El precio de 1 ETH corresponde a "+ str(last_ETH) +" Dolares" + "\n" +
      "Eso equivale a " + str(precio_ETH) + " pesos chilenos (valor CLP/UDS "+ str(last_CLP) +")")




#%%
# =============================================================================
# Funciones para desarolladores WEB3/Ethereum chilenos
# =============================================================================

'''
Las siguientes funciones pueden resultar útiles para sol desarrolladores de Ethereum
Se pueden usar en paralelo mientras se desarrollan contratos inteligentes e ir calculando 
en simultaneo, con estas funciones, el costo en pesos chilenos (a precio actual) asociado a 
las funciones que se van desarrollando.

Ir controlando el costo "En vivo" puede generar ahorros de dinero al permitir comparar
el costo de las funciones que se van optimizando a medida que se desarrollan.

'''



# Conversor Wei a Clp$:
    # Dentro de la función está el objeto correspondiente a la conversión unidad de ETH-CLP,
    # El usuario solamente debe ingresar la cantidad de Wei a convertir

def wei_to_clp(x = 1):
    wei_clp = wei * x
    return(wei_clp)
  
# Test de la función. 150000000000000 Wei corresponden a CLP$ 283 
wei_to_clp(150000000000000)   



# Convertor Gwei a Clp$
  # Dentro de la función está el objeto correspondiente a la conversión unidad de ETH-CLP,
  # El usuario solamente debe ingresar la cantidad de Gwei a convertir
  
def gwei_to_clp(x = 1):
    gwei_clp = gwei * x
    return(gwei_clp)

# Test de la función. 1500000000 Gwei corresponden a CLP$ 2 381 341   
gwei_to_clp(1500000000) 


# Convertor Finney a Clp$
  # Dentro de la función está el objeto correspondiente a la conversión unidad de ETH-CLP,
  # El usuario solamente debe ingresar la cantidad de Finney a convertir
  
def finney_to_clp(x = 1):
    finney_clp = finney * x
    return(finney_clp)

# Test de la función. 19000 Finney corresponden a CLP$ 30 163 656 
finney_to_clp(19000) 

#%%
# =============================================================================
# Estadistica descriptiva generales de Ethereum
# =============================================================================

'''
Parámetros generales:
    La siguiente función permite obtener estadística descriptiva general para el
    cierre de la cotización de ETH.
    
    
    
    La función incluye un loop doble:
        
        1. sirve para recorrer una lista numérica para obtener la última semana de cotización (7),
            mes (30), trimestre (90), semestre (180) y año (360). Hay que considerar que ETH cotiza 24/7
            
        2. La segunda lista es para poder imprimir en la consola un string correspondiente al periodo
            a analizar
'''
        
    
def parametros_generales(x = ETH):
    
    periodos_n = [7, 30, 90, 180, 360]
    
    periodo_t = ["semanal", "mensual", "trimestral", "semestral", "anual"]
    
    for n, t in zip(periodos_n, periodo_t):
        dummy = x.tail(n).describe()
        print("El rendimiento " + t + " es:" + "\n"+ str(dummy))


# Parámetros para el precio           
parametros_generales()

# Parámetros para el retorno
parametros_generales(x = ETH_R)


#%%
# =============================================================================
# Serie temporal ETH por periodo
# =============================================================================


'''
La siguiente función permite generar un plot de serie temporal exclusivo para la cotización de diferentes 
periodos del precio de ETH

La función se compone de condicionantes.
EJ: si la condición corresponde al parámetro ingresado en la función, entonces se hará el plot correspondiente

'''




def line_plot_ETH(x = "anual"):
    
  if x == "anual":
      año = 360
  elif x == "mensual":
      mes = 30
  elif x == "semanal":
      semana = 7
  else:
      print("ERROR: Ingresa en la funicón un priodo: anual, mensual o semanal")
      
  if x == "anual":
      ETH.tail(año).plot(kind = 'line', 
                         marker = '.', 
                         linestyle = '-',
                         color = 'b', 
                         title = 'Serie anual ETH')
  elif x == "mensual":
      ETH.tail(mes).plot(kind = 'line', 
                         marker = 'o', 
                         linestyle = '-',
                         color = 'b', 
                         title = 'Serie mensual ETH')
  elif x == "semanal":
      ETH.tail(semana).plot(kind = 'line', 
                         marker = 'o', 
                         linestyle = '-',
                         color = 'b', 
                         title = 'Serie semanal ETH')
  else:
      print("ERROR: Ingresa en la funicón un priodo: anual, mensual o semanal")
         
      
# Ajustando los parametros de la función se obtiene un plot fácilmente para el periodo ingresado

# plot ETH anual
line_plot_ETH(x = "anual")

# plot ETH mensual
line_plot_ETH(x = "mensual")

# plot ETH semanal
line_plot_ETH(x = "semanal")  
    
#%% 
# =============================================================================
# Density plot ETH por periodo
# =============================================================================


'''
Similar a la función anterior, esta permite generar un plot de densidad exclusivo para la cotización de diferentes 
periodos del precio de ETH

La función se compone de condicionantes.
EJ: si la condición corresponde al parámetro ingresado en la función, entonces se hará el plot correspondiente

'''

 
ETH_R.plot(kind = "density")

def density_plot_ETH(x = "anual"):
    
  if x == "anual":
      año = 360
  elif x == "mensual":
      mes = 30
  elif x == "semanal":
      semana = 7
  else:
      print("ERROR: Ingresa en la funicón un priodo: anual, mensual o semanal")
      
  if x == "anual":
      ETH_R.tail(año).plot(kind = "density", 
                         linestyle = '-',
                         color = 'b', 
                         title = 'Serie anual ETH')
  elif x == "mensual":
      ETH_R.tail(mes).plot(kind = "density",  
                         linestyle = '-',
                         color = 'b', 
                         title = 'Serie mensual ETH')
  elif x == "semanal":
      ETH_R.tail(semana).plot(kind = "density", 
                         linestyle = '-',
                         color = 'b', 
                         title = 'Serie semanal ETH')
  else:
      print("ERROR: Ingresa en la funicón un priodo: anual, mensual o semanal")
      
    
# Ajustando los parametros de la función se obtiene un plot fácilmente para el periodo ingresado
   
# plot ETH anual
density_plot_ETH(x = "anual")

# plot ETH mensual
density_plot_ETH(x = "mensual")

# plot ETH semanal
density_plot_ETH(x = "semanal")   













