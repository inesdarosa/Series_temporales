import pandas as pd
import os
from datetime import datetime
from datetime import date

os.chdir('C:/Users/Public/ancap_datos')

var_ind = pd.read_csv('Desafio1_VariablesControl.csv')
var_ind.head()
var_ind['TimeStamp'].dtype

var_ind['TimeStamp'] = pd.to_datetime(var_ind['TimeStamp'])
var_ind.head()
var_ind['TimeStamp'].dtype

var_ind['TimeStamp'] = var_ind['TimeStamp'].dt.date
var_ind.tail(10)

var_ind_mean = var_ind.groupby('TimeStamp').mean()
var_ind_mean.index
var_ind_mean['Fecha'] = var_ind_mean.index
var_ind_mean['Fecha'].dtype


var_ind_mean.rename(columns={'TimeStamp':'Fecha'}, inplace=True)
var_ind_mean.head()
var_ind_mean.to_csv('var_ind_mean.csv')

var_dep = pd.read_csv('Desafio_1_Contenido_C7_Total.csv')
var_dep.head()
var_dep['Fecha'].dtype

var_dep['Fecha'] = pd.to_datetime(var_dep['Fecha'])
var_dep.head()
var_dep['Fecha'].dtype
var_dep = var_dep.drop(['Hora', 'archivo'], axis=1)
var_dep['Fecha'].dtype

var_dep['Fecha'] = var_dep['Fecha'].dt.strftime('%Y-%m-%d')
var_dep['Fecha'].dtype
var_dep = var_dep.sort_values(by='Fecha')
var_dep.to_csv('var_dep.csv')

mean_ind = pd.read_csv('var_ind_mean.csv')
dependiente = pd.read_csv('var_dep.csv')

mean_ind['Fecha'].dtype
dependiente['Fecha'].dtype

mean_ind = mean_ind.set_index('Fecha')
dependiente = dependiente.set_index('Fecha')

df_inner = mean_ind.merge(dependiente, how='inner', left_index=True, right_index=True)
df_inner.to_csv('indep_mean_dep.csv')
df_inner.to_csv('indep_dep.csv')




