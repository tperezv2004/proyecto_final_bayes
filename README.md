# Proyecto Final Bayes

Proyecto grupal del curso de Bayes, donde se trabaja con datos de competencias de powerlifting obtenidos desde **OpenPowerlifting**.

El objetivo principal es limpiar, filtrar y analizar datos de levantadores, utilizando una base de datos reducida a competencias realizadas en Estados Unidos.

## Estructura del repositorio

El repositorio se organiza en las siguientes carpetas:

```text
Proyecto_Final_Bayes/
├── Codigo/
├── Data/
├── PDFs/
└── README.md
```

## Carpeta `Codigo`

En esta carpeta se encuentran los scripts utilizados en el proyecto.

Archivos principales:

* `Limpieza.R`: limpia la base de datos original y genera una base filtrada para Estados Unidos.
* `avance_luna.R`: avance realizado por Luna.
* `avance_tomi.R`: avance realizado por Tomi.

## Carpeta `Data`

En esta carpeta deben guardarse las bases de datos del proyecto.

Esta carpeta está ignorada por GitHub, ya que los archivos de datos pueden ser muy pesados. Por esta razón, los datos no se suben directamente al repositorio.

Archivos principales:

* `completos.csv`: base de datos original descargada desde OpenPowerlifting.
* `reporte_USA.csv`: base de datos filtrada que se utiliza para el análisis.

## Carpeta `PDFs`

En esta carpeta se encuentran los documentos relacionados con el proyecto.

Archivos principales:

* `informe.Rmd`: archivo editable del informe.
* `informe.pdf`: versión en PDF del informe.
* `Proyecto_Instrucciones_Generales.pdf`: instrucciones generales del proyecto.

## Datos utilizados

Los datos fueron obtenidos desde OpenPowerlifting:

* Página principal: https://www.openpowerlifting.org/
* Descarga de datos: https://openpowerlifting.gitlab.io/opl-csv/bulk-csv.html

Para descargar los datos, se debe bajar el archivo:

```text
openpowerlifting-latest.zip
```

Dentro del archivo `.zip` aparecen varios archivos. El archivo `.csv` principal se debe renombrar como:

```text
completos.csv
```

Luego, este archivo debe guardarse en la siguiente ubicación:

```text
Data/completos.csv
```

## Preparación de los datos

Una vez descargado y guardado el archivo `completos.csv`, se debe ejecutar el siguiente script:

```text
Codigo/Limpieza.R
```

Este script limpia la base original y genera el archivo:

```text
Data/reporte_USA.csv
```

Este archivo corresponde a la base de datos filtrada para Estados Unidos, que es la que se utiliza en el análisis del proyecto.

Agregue un script para dejar el script final mas ordenado, con el procesamiento de los datos aparte. Si se ejecuta 

```text
Codigo/data_modelos_finales.R
```
se crea un nuevo archivo llamado `Data/modelos_finales.csv`, no se esta usando en nada por ahora, pero mi idea es que quede como un archivo con los datos ya procesados para los modelos, y asi el script del modelo quede mas ordenado y limpio.

## Falta 
1. Agregar BF, ya que solo tenemos LOO y la profe dijo que debiamos agregarlo (intrepretar y "comparar" ambos)

2. Revisar ortografía y code 

3. Analisas graficos y resultados

4. Hacer predicciones con el modelo (ejemplo: edad -> + probabilidad de ser ELITE)

5. Terminar Informe