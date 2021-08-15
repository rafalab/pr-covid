### Preguntas frecuentes

Haga clic en la pregunta para ver la contestación.

<details><summary>¿Cuál es el objetivo de este dashboard? </summary>

Proveer un resumen detallado, basado en datos, de la situación del COVID-19 en Puerto Rico. El propósito principal es dar una idea de cuantos contagios **actuales** hay, cómo esto afecta la salubridad de PR, y las tendencias. Intentamos corregir por sesgos y otras complicaciones que hacen resumir los datos una tarea retante.

El proyecto comenzó durante el verano del 2020 cuando en Puerto Rico se reportaban casos detectados pero no el número de pruebas administradas. Como resultado, era imposible saber si el perfil optimista que veíamos era debido a que había pocas personas infectadas o a falta de pruebas. Una vez conseguimos acceso a los datos de pruebas, comenzamos a reportar la tasa de positividad. De ahí el nombre de nuestra dirección en la Web: [tpr.covidpr.info](tpr.covidpr.info), tpr = _test positivity rate_. Pueden ver definición de tasa de positividad y explicación de por qué la usamos [aquí](https://rafalab.github.io/pr-covid/tasa-de-positividad-faq.html). 

Cuando comenzamos a reportar la tasa de positividad en junio era bien baja, pero a finales de ese mes comenzó a subir rapidamente y pudimos alertar antes de llegar a niveles críticos. Desde entonces hemos añadido muchas otros resúmenes que la comunidad nos ha pedido. Esta página provee detalles de indicadores, métricas y otros resúmenes que ofrecemos, y como mejor sacarle provecho a este recurso.

</details>

<details><summary>¿Qué es la tasa de positividad? </summary>

La tasa de positividad se define para un periodo dado (como una semana) de dos formas:

* tasa de positividad (pruebas) = Número de pruebas positivas / Número de pruebas totales

* tasa de positividad (casos) = Casos nuevos único detectados con prueba / Número de personas que se hicieron la prueba 

Al ser un por ciento en vez de un total, en general, la tasa de positividad nos permiten comparar la tasa en periodos con pocas pruebas a la tasa en periodos con muchas pruebas. La tasa de positividad es la métrica que, hasta ahora, mejor nos permite estimar cuantas hospitalizaciones y muertes habrá en dos semanas. Para más detalles vean [este documento](https://rafalab.github.io/pr-covid/tasa-de-positividad-faq.html).

</details>

<details><summary>¿Cómo uso el dashboard para mejor entender la situación del COVID-19 en PR? </summary>

El dashboard estás dividido en un panel de control a la izquierda (arriba en pantallas pequeñas) y un panel principal a la derecha (abajo en pantallas pequeñas). 

El panel principal tiene 16 partes con una pestaña para cada una. 

La página de inició, bajo la pestaña _RESUMEN_, provee un resumen general de la situación. Nos enfocamos en 6 indicadores: la tasa de positividad, los casos detectados por día, pruebas diarias, las hospitalizaciones, las muertes, y el por ciento de la población vacunada. Esta página se divide en tres partes: 1) niveles actuales, 2) niveles de hace una semana y niveles meta y 3) resúmenes gráfico. Debido a que hay retrasos en reportar resultados de pruebas y muertes, los niveles actuales no reportan casos, pruebas ni muertes. Más detalles sobre estas métricas se incluyen en la sección _¿Qué indicadores se incluyen y por qué?_ 

El panel de control te permite cambiar opciones de como ver los datos. Las opciones que pueden cambiar son las siguientes:

* Las fechas examinadas: Por defecto mostramos los últimos 90 días, pero pueden cambiarlo a cualquier intervalo. Incluimos tres botones que automáticamente permiten cambiar a la ultima semana, los últimos 90 días, o todas las fechas desde marzo 12. Está opción no afecta las tablas en la pestaña _RESUMEN_ pues en está fijamos la fechas a las más recientes, aunque si cambia los gráficas.

* El tipo prueba usada para crear resúmenes: moleculares o de antígeno. También incluimos una opción que combina las dos pruebas diagnósticas: moleculares y de antígeno. Note que los datos de hospitalizaciones, muertes, y vacunas no son afectados por esta opción. Está opción no afecta la pestaña _RESUMEN_ pues en está fijamos el tipo de prueba que mostramos.

* Datos diarios o acumulativos: Por defecto mostramos datos diarios ya que se destacan las tendencias mucho mejor. Pero ofrecemos la opción de cambiar a datos acumulativos. 

* El rango del eje de y de las gráficas: Por defecto escogemos rangos que ayudan a hacer comparaciones de distintos periodos. Pero esto se puede cambiar para que el rango sea sea determinado por los datos.

* Descargar datos: Aquí el usuario puede escoger entre varias bases de datos para descargar.

</details>

<details><summary>¿Cómo se calculan las tendencias? </summary>

La mejor forma de determinar la tendencia es mirando las gráficas. Además de esto hacemos un cálculo estadístico para deteminar si un indicador esta subiendo, bajando o quedandose más o menos igual.

Las flechas de colores muestran la tendencia de cada indicador. Específicamente, comparamos cada semana con la semana anterior y llevamos a cabo una prueba de significancia estadística.

* &#8595; = Disminución estadísticamente significativa.
* &#8596; = No hay cambio estadísticamente significativo.
* &#8593; = Aumento estadísticamente significativo.

Los colores indican la tendencia que deseamos ver (verde) y la que no (rojo). El total de pruebes es la única métrica que queremos ver subir.

</details>

<details><summary>¿De dónde sacan los datos? </summary>

Los **datos de pruebas** viene directamente del BioPortal, una base de datos que maneja el Departamento de Salud. Estos datos son, en su mayoría, sometidos electronicamente por los hospitales y laboratorios clínicos que hacen las pruebas. El Departamento de Salud hace estos datos públicos a través de [APIs](https://en.wikipedia.org/wiki/API). Incluimos una lista de estos APIs [aquí](https://github.com/rafalab/pr-covid/blob/master/dashboard/apis.md).

Desafortunadamente, el Departamento de Salud no incluye los datos de mortalidad y hospitalizaciones en el BioPortal. Estos los tenemos que organizar a mano leyendo los informes oficiales de Salud todas las mañanas. Este trabajo lo hace María-Eglée Pérez del Departamento de Matemáticas, UPR-RP quien comparte los datos [aquí](https://raw.githubusercontent.com/rafalab/pr-covid/master/dashboard/data/DatosMortalidad.csv). Estos datos no están compartamentalizados por tipo de prueba. 

Los  datos actualizados de vacunación son provistos por la CDC via [Our World in Data](https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv).

Datos de rezagos de muerte son provistos por 
[@midnucas](https://twitter.com/midnucas) quien organiza una [tabla](https://raw.githubusercontent.com/sacundim/covid-19-puerto-rico/master/assets/data/cases/PuertoRico-bitemporal.csv) con los datos de todos los informes diarios de salud.

</details>

<details><summary>¿Por qué los datos son diferentes a los del informe oficial de Departamento de Salud? </summary>

Los datos del BioPortal son en su mayoría recopilados de forma electrónica por lo que es dinámico y continuamente sujeto a cambios. Además, dado a que en el Departamento de Salud se utilizan métodos manualesde depuración de datos y aquî usamos métodos automatizados, los datos difieren ligeramente de los números oficiales. El BioPortal provee información para hacer esta depuración mucho más rápido, pero es posible que no se capture un pequeño porcentaje de duplicados.
</details>

<details><summary>¿Qué indicadores se incluyen y por qué? </summary>

Cada pestaña se enfoca en un indicador o métrica diferente.

* Datos Diarios - Una tabla mostrando los datos diarios para varios indicadores. Al final de la página hay un botón que permite descargar la tabla.

* Positividad - Graficamos dos versiones de la tasas de positividad. Mostramos tasas diarias con puntos y una curva mostrando la media móvil de 7 días. Mostramos la curva entrecortada los últimos 7 días como recordatorio hay un retraso en reportar que posiblemente introduce sesgos. Incluímos un intervalo de confianza de 95% para le media móvil.
También se puede cambiar el tipo de prueba es usada para el cálculo. Una explicación detallada se encuentra [aquí](https://rafalab.github.io/pr-covid/tasa-de-positividad-faq.html). 

* Hospitalizaciones - Mostramos con barras el número de personas hosptializadas por COVID-19 cada día. También mostramos el número de estos que en el ICU. Las curvas muestra una media móvil de 7 días. Si escogen la opción de que los datos escojan el rango del eje de y, solo se muestran las hosptalizaciones.

* ICU - Los curva muestra el por ciento de las camas de ICU que están ocupadas por pacientes de COVID-19.

* Muertes - Las barras grises muestra las muertes ocurridas cada día. La curva negra es una media móvil de 7 días. No mostramos la curva para los últimos 7 días por que sabemos que estarán sesgado por el retraso en reportar.

* Pruebas - Las barras violetas muestran las pruebas para cada día. La curva muestra una media móvil de 7 días. 
Promedio diario de personas que se hicieron la prueba esa semana. No mostramos la curva para los últimos 7 días por que sabemos que estarán sesgado por el retraso en reportar. Más abajo mostramos las tasa de positividad (pruebas) para los 16 laboratorios/hospitales que más pruebas hacen. Mas abajo de eso mostramos que porcentaje de la pruebas fue hecha por cada laboratorio/hospital por día.

* Casos - Las barras rojas muestran los casos para cada día. Recuerden que pueden cambiar el tipo de prueba en el panel de controles.  Cambiando Moleculares + Antígeno puede ver todos los casos detectados con prueba diagnóstica.  No mostramos la curva para los últimos 7 días por que sabemos que estarán sesgado por el retraso en reportar.

* Regiones - Indicadores claves estratificados por región de Puerto Rico. Este panel nos ayuda a detectar si una parte de Puerto Rico tiene más casos que otra.

* Municipio - Tabla de tasa de positividad (prueba) por municipio. También incluye el número de positivos que son menores de 20 años. Aquí es informativo cambiar las fechas a la última semana o las últimas dos semanas para ver un cuadra más actual que los últimos 90 días que se muestran por defecto.

* Mapa - Una mapa de Puerto Rico mostrando la tasa de positividad para cada municipio con intensidades del color rojo.

* Por edad - Un histograma o serie de tiempo mostrando la distribución de casos por grupos de edad. 

* Rezago - Para las pruebas positivas, negativas y muertes reportadas en el periodo escogido en el panel de control, mostramos el porcentaje que tomo 1 día, 2 días, 3 días, y así sucesivamente, en ser reportada. 

* Labs - Para el periodo de tiempo escogido en el panel de control, mostramos el número de pruebas hechas por casa laboratorio/hospital. Los que hacen pocas pruebas son agrupados en _Otros_.

* Vacunas - En la gráfica mostramos el número dosis distribuidas, total de vacunas administradas, personas con por lo menos una dosis, y personas con ambas dosis, para cada día.

</details>

<details><summary>¿Por qué reportan promedios (media móvil) de 7 días? </summary>

Muchos de los indicadores diarios que mostramos son el resultado de tomar el promedio de los últimos 7 días. Las dos razones principales para calcular estos promedios de 7 días son:

* El día de la semana tiene un efecto grande en los números observados. Por ejemplo, los lunes se hacen muchas más pruebas que los domingos y por lo tanto se ven muchos más casos los lunes. Al tomar un promedio de 7 días, incluimos datos de todos los días de la semana, lo cual hace el indicador más comparable de día a día.

* Algunos de las observaciones varían naturalmente por el azar. Tomar promedios reduce esta variabilidad.

Cuando tomamos promedios de 7 días todos los días, a la curva resultante se le llama una _media móvil_.
 </details>


<details><summary>¿Qué representan las curvas entrecortadas? </summary>

Cómo los datos de las pruebas tardan hasta una semana en completar, usamos curvas entrecortadas para representar la última semana y recordarnos de este hecho.

</details>

<details><summary>¿Por qué muestran intervalos de confianza? </summary>

Los datos observados incluyen variabilidad aleatoria. Cuando se recopilan menos datos, esto afectará más los resultados. Por ejemplo los casos reportados en Culebra son menos fiables como estimado de cuantos casos hay que los de San Juan. El intervalo de confianza nos da una idea de cuanto es esta variabilidad.

</details>

<details><summary>¿Puedo ver resúmenes basados en otros tipos de prueba? </summary>

Por defecto mostramos resúmenes basados en pruebas moleculares ya que hemos calibrado los metas basado en estos datos. 
Pero en el panel de control pueden cambiar el tipo de pruebas usadas en los cálculos a moleculares, moleculares y de antígenos, o de antígenos. Información sobre las diferencias entre estás pruebas se encuentran [aquí](https://espanol.cdc.gov/coronavirus/2019-ncov/symptoms-testing/testing.html).

</details>

<details><summary>¿Por qué quitaron los datos de pruebas serológicas? </summary>

No las estábamos usando para ningún monitoreo. Como ocupan espacio en el servidor y hacen los cómputos más lentos las removimos.

</details>

<details><summary>¿Puedo descargar los datos? </summary>

El dashboard ofrece varias tablas para descargar. En el panel de control pueden ver una botón que dice **Datos depurados** y debajo un botón que dice _DOWNLOAD_.  El botón de **Datos depurados** provee un menú para escoger las diferentes opciones que son:

* Todas las pruebas - tabla con sobre un millón de líneas, una para cada prueba. Incluye todos los tipos de prueba.
* Casos por día - tabla con los casos por día para tipo de prueba.
* Muertes y hospitalizaciones - Datos de hospitalizaciones y muertes sacado del informe oficial de salud.
* Casos, positivos y pruebas por día - tabla con información de pruebas y casos detectados. Incluye todos los tipos de prueba.
* Positivos y pruebas por municipio por día - Pruebas y positivos por municipio por día. No incluye identificador por lo cual no se pueden identificar casos únicos. Incluye todos los tipos de prueba.
* Positivos y pruebas por edad por día" - Pruebas y positivos por edad por día. No incluye identificador por lo cual no se pueden identificar casos únicos. Incluye todos los tipos de prueba.
* Positivos y pruebas por municipio/edad por día - Pruebas y positivos por edad y municipio por día. No incluye identificador por lo cual no se pueden identificar casos únicos. Incluye todos los tipos de prueba.
* Positivos por laboratorio -  Número de positivos y pruebas por laboratorio por día. Incluye todos los tipos de prueba.
* Pruebas por laboratorio - Número de pruebas por laboratorio por día. 
* Rezago - Datos de cuanto toman las pruebas en entrar por día.


</details>


<details><summary>¿Puedo cambiar las fechas que se muestra? </summary>

En el panel de control hay dos cuadros blancos en los cuales pueden escoger cualquier días para definir un periodo. Además hay 3 botones con selecciones pre-escogidas: última semana, últimos 90 días, y todos los días, incluyendo hoy.
</details>

<details><summary>¿Puedo ver el código? </summary>

Todo el código está en GitHub: [https://github.com/rafalab/pr-covid/tree/master/dashboard](https://github.com/rafalab/pr-covid/tree/master/dashboard)
</details>

<details><summary>¿Puedo usar la gráficas creadas por el dashboard? </summary>

Pueden usar las gráficas generadas para cualquier cosa. No hace falta, pero si quieren dar crédito incluyan a tpr.covidpr.info como la fuente.

</details>

<details><summary>¿Qué hago si tengo más preguntas?</summary>

Pueden hacer preguntas por Twitter: [@rafalab](https://twitter.com/rafalab)
</details>

<details><summary>Agradecimientos</summary>

Muchos han contribuido a mejorar este dashboard con datos, críticas, sugerencias, y peticiones. Gracias a:

* María-Eglée Pérez
* Rolando Acosta
* Annie Ng
* Marcos López Casillas
* José Rodríguez Orengo
* Daniel Colón Ramos
* Danilo Pérez Rivera
* Midnucas
* Giovanna Guerrero
* Joshua Villafañe
* Elvis Nieves Miranda
* Mónica Robles Fontán
* Fabiola Cruz López
* Arturo Portnoy
* Robby Cortés
