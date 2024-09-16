# Universidad de los Andes
# Problem-Set-1_Machine-Learning_2024

**Integrantes**
* Julieth Molano
* Diego Cuesta
* Henry Carvajal
* Jorge Ramirez

Este repositorio contiene todos los recursos necesarios para resolver el primer Problem Set del curso de Big Data y Machine Learning del segundo semestre de 2024 en la Universidad de los Andes. La información está organizada en varias carpetas, cada una con un propósito claro, para facilitar tanto la comprensión del análisis como la replicación de los resultados obtenidos.

En primer lugar, fuera de las carpetas principales, se encuentran los archivos .Rhistory y .RData, que corresponden a los resultados de la ejecución del código. El archivo .Rhistory contiene un registro de todos los comandos ejecutados en la consola durante la edición del código, permitiendo revisar el historial y verificar los resultados. Por su parte, el archivo .RData almacena todos los objetos creados en el entorno de trabajo, como variables y modelos utilizados, facilitando la restauración completa del espacio de trabajo para cada integrante del equipo.

Con respecto a las carpetas, estas contienen la siguiente información:

**Documento:**  Esta carpeta almacena el documento final en .pdf. Para su realización se empleo la herramienta Overleaf de Latex, por lo que este repositorio no permite la reproducibilidad de dicho documento final.

**Scripts:** En esta carpeta están los scripts utilizados para generar todos los resultados.

 - **0. Script_Base:** Contiene el código principal, se determina el directorio de trabajo, se cargan los paquetes necesarios y llama a los otros scripts. Para su reproducibilidad es importante determinar la ruta de trabajo. En la línea 50 puede cambiar "otro_directorio" por la ruta en donde replico el repositorio.
 
 - **1. Scraping:** El código realiza el proceso de web scraping para conseguir los datos.
   
 - **2. Filtro Base y seleccion de variables:** El código realiza  limpieza de la base de datos, mantiene las variables de interés y hace imputación de datos en missing values. Además, guarda la base de datos de interés (base_final.rds) que sera usada en los siguientes códigos.

 - **3. Estadísticas descriptivas:** El código  realiza el análisis descriptivo de los datos.
   
 - **4. Modelo ingreso-edad:** El código realiza las estimaciones y análisis de datos de la sección 3 del Problem Set (Teoría edad-salario).
   
 - **5. Evaluacion:** El código realiza las estimaciones y análisis de datos de la sección 5 del Problem Set (Predicciones del ingreso).
   
 - **6. Modelo Gender_Earnings_Gap:** El código realiza las estimaciones y análisis de datos de la sección 4 del Problem Set (Brecha salarial diferenciando por género).

**Base_Datos:** En esta carperta se almacena la base de datos ajustada que es empleada para realizar el taller: "base_final.rds". Adicional contiene las tablas consolidadas con la información obtenida tras el proceso de extracción de datos, facilitando su carga y uso a lo largo del proyecto.

**Graficas:** Esta carpeta almacena las visualizaciones que ayudaron en el análisis de la información y permitieron la evaluación visual de los resultados obtenidos durante el taller. 

**Latex:** Esta carpeta almacena los documentos .tex con los resultados de las diferentes regresiones para poder ser posteriormente compiladas en la herramienta de edición de texto overleraf. 
