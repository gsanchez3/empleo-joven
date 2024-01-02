# empleo-joven
Codigos para el procesamiento de las bases y el tablero de empleo joven

Este repositorio tiene los codigos para actualizar todas las bases de empleo joven:
a) Correr-desde-cero: Esta carpeta es para generar todas las bases la primera vez.
  1. empleo_joven_v4. Este codigo genera las bases originales tomando todas las mectras. Ajusta los datos de las mectras fallidas y guarda la version beta con toda la informacion de los jovenes.
  2. calc_porc_total_todo. Este codigo calcula los puestos totales por cada clae2 y provincia. Dato que luego sera utilizado en el tablero. Ajusta tambien las mectras fallidas y guarda ese dato para proximos procesamientos. Al mergear con la version beta del paso 1, se genera la version final para el tablero: "clae2_prov_universos.csv".
  3. ajusta_bases. Este codigo meramente corrige algunos NA por cero, ya que al ajustar las mectras fallidas algunos datos faltantes generaban algunos NA. Agarra la base de "clae2_prov_universos.csv", ajusta estos casos y la vuelve a guardar.

b) tablero-power-bi: Esta carpeta tiene los codigos que hay que correr para actualizar los tableros todos los meses.
  1. genera_bases. Este codigo hace lo mismo que el paso 1 del anterior, solo que toma como insumo todo la informacion ya trabajada, le quita los meses que hay que actualizar, los calcula nuevamente con las nuevas mectras y luego los agrega a la nueva version beta.
  2. calculo_porc_total. Este codigo hace tambien el paso 2 de la carpeta anterior, pero nuevamente lo hace solo para los meses de las mectras nuevas. Luego actualiza la version "clae2_prov_universos.csv".
  3. ajustes_desest. Este codigo desestacionaliza las variables de remuneracion en distintos loops segun distintos niveles de agregacion, el general, por letra, por provincia y por provincia y letra. Luego guarda esta informacion en el archivo "provincia_letra_totales.csv" que sera utilizado en el script siguiente
  4. ajuste_final_conyeso. Este script termina de ajustar las bases a la version final que debe ser entregada a sistemas. Quita alguna informacion sobrante, genera la categoria Otros para las actividades marginales y mergea la informacion con y sin desestacionalizar. El producto de este codigo es la version final entregable: "fact_empleojoven.csv"

c) datos-abiertos: Esta carpeta tiene el codigo para cortar las bases para datos abiertos.
  1. arma_bases_da. Luego de actualizar toda la informacion con los scripts de la carpeta anterior, este script toma la base cruda como sale del paso 1 de la carpeta anterior "serie_completa_v5.xlsx". Ajusta la categoria Otros, limpia un poco las bases y corta la informacion en 56 series diferentes a ser publicadas en Datos Abiertos.
