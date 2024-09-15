# Problem-Set-1_Machine-Learning_2024

**Integrantes**
* Julieth Molano
* Diego Cuesta
* Henry Carvajal
* Jorge Ramirez




Este repositorio contiene todos los recursos necesarios para resolver el primer Problem Set del curso de Big Data y Machine Learning del segundo semestre de 2024 en la Universidad de los Andes. La información está organizada en varias carpetas, cada una con un propósito claro, para facilitar tanto la comprensión del análisis como la replicación de los resultados obtenidos.


En primer lugar, fuera de las carpetas principales, se encuentran los archivos .Rhistory y .RData, que corresponden a los resultados de la ejecución del código. El archivo .Rhistory contiene un registro de todos los comandos ejecutados en la consola durante la edición del código, permitiendo revisar el historial y verificar los resultados. Por su parte, el archivo .RData almacena todos los objetos creados en el entorno de trabajo, como variables y modelos utilizados, facilitando la restauración completa del espacio de trabajo para cada integrante del equipo.

Con respecto a las carpetas, estas contienen la siguiente información:
Documento: Esta carpeta almacena el documento final en .pdf. Para su realización se empleo la herramienta Overleaf de Latex, por lo que este repositorio no permite la reproducibilidad de dicho documento final.
scripts: En esta carpeta están los scripts utilizados para generar los resultados.

00_main_script: Contiene el código principal, se determina el directorio de trabajo, se cargan los paquetes necesarios y llama a los otros scripts. Para su reproducibilidad es importante determinar la ruta de trabajo. En la línea 37 puede cambiar "otro_directorio" por la ruta en donde replico el repositorio.
01_web_scraping: Este código realiza web scraping de la página web del profesor Ignacio Sarmiento, para la adquisición de los datos. Además, guarda la base de datos en formato .RData en la carpeta de stores.
02_data: Este código realiza la limpieza de la base de datos, mantiene las variables de interés, realiza imputación de datos y estadísticas descriptivas. Además, guarda los resultados en formato .RData en la carpeta de stores.
03_age_wage_profile: Este código realiza las estimaciones de la sección 3 del problem set (teoría edad-salario). Además, guarda los resultados en formato .RData en la carpeta de stores.
04_gender_earnings_gap: Este código realiza las estimaciones de la sección 4 del problem set (brecha salarial por género). Además, guarda los resultados en formato .RData en la carpeta de stores.
05_predicting_earnings: Este código realiza las estimaciones de la sección 5 del problem set (predicciones de ingreso).



En el repositorio, la carpeta Base_Datos contiene las tablas consolidadas con la información obtenida tras el proceso de extracción de datos, facilitando su carga y uso a lo largo del proyecto. Además, permite la evaluación de los datos mediante herramientas externas.

La carpeta Graficas almacena las visualizaciones que ayudaron en el análisis de la información y permitieron la evaluación visual de los resultados obtenidos durante el taller.
La carpeta Latex almacena los documentos .tex con los resultados de las diferentes regresiones para poder ser posteriormente compiladas en la herramienta de edición de texto Latex. 
Por último, la carpeta Scripts contiene el código desarrollado para resolver cada punto del proyecto, incluyendo un script inicial que optimiza la conexión entre los miembros del equipo y soluciona problemas de codificación que pudieran afectar el desarrollo del taller.
