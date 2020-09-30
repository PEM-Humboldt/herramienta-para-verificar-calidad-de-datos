# Herramienta para verificar calidad de datos
Herramienta para la verificación geográfica y taxonómica de registros biológicos.

V0.1.0

## Por dónde empezar

A continuación se presentan instrucciones para ejecutar la aplicación en su máquina local.

### Prerrequisitos

Es necesario tener instalado:

* [R](https://www.r-project.org/)
* Un navegador web moderno (*e.g.* [Google Chrome](https://www.google.com/chrome/), [Microsoft Edge](https://www.microsoft.com/en-us/edge) o [Mozilla Firefox](https://www.mozilla.org/en-US/firefox/new/))
* [RStudio](https://rstudio.com/products/rstudio/download/) (opcional)

### Instalación y ejecución

Si está familiarizado con `git`, ejecute el siguiente comando para clonar este repositorio en su máquina local:

```
git clone https://github.com/PEM-Humboldt/herramienta-para-verificar-calidad-de-datos.git
```

De lo contrario, descargue la carpeta comprimida desde el botón *Code* y extraiga su contenido en algún lugar de su máquina.

Esta aplicación necesita diferentes paquetes de R para su ejecución. El script `install.R` se encarga de instalar todos los paquetes de manera automática. Con el fin de evitar conflictos entre paquetes de R ya existentes en su máquina, la instalación se hace en una librería privada e independiente que es utilizada únicamente por esta aplicación. Por su parte, el script `runApp.R` es el encargado de abrir la aplicación en el navegador por defecto del sistema. Si ya está familiarizado con la ejecución de scripts de R, ejecute solo una vez el script `install.R` y después de haber reiniciado la sesión de R, ejecute el script `runApp.R`. De aquí en adelante solo es necesario ejecutar el script `runApp.R` cada vez que quiera abrir la aplicación. Si no está familiarizado con la ejecución de scripts de R, a continuación se presentan tres maneras distintas de hacerlo:

<br>

#### **R desde una consola**
Abra una terminal o consola (*i.e.* CMD en Windows) y navegue hasta el la carpeta con los contenidos de la aplicación utilizando el comando `cd`. Por ejemplo:

```
cd C:\Users\Foo\Documents\herramienta-para-verificar-calidad-de-datos
```

Una vez esté ubicado en la carpeta, ejecute el siguiente comando una sola vez para instalar los paquetes necesarios:

```
R --no-save < install.R
```

Una vez haya terminado la instalación podrá abrir la aplicación cada vez que quiera utilizando el comando (estando ubicado desde la carpeta de la aplicación):

```
R --no-save < runApp.R
```

La aplicación abrirá en el navegador por defecto del sistema. En caso de que su sistema no reconozca `R` como un comando, deberá agregar el ejecutable de `R` a la variable de entorno `PATH` o ejecutar el comando anterior utilizando la ruta completa del ejecutable. Por ejemplo:

```
C:\Program Files\R\R-4.0.2\bin\R.exe --no-save < runApp.R
```

<br>

#### **Rscript desde una consola**
Abra una terminal o consola (*i.e.* CMD en Windows) y navegue hasta el la carpeta con los contenidos de la aplicación utilizando el comando `cd`. Por ejemplo:

```
cd C:\Users\Foo\Documents\herramienta-para-verificar-calidad-de-datos
```

Una vez esté ubicado en la carpeta, ejecute el siguiente comando una sola vez para instalar los paquetes necesarios:

```
Rscript install.R
```

Una vez haya terminado la instalación podrá abrir la aplicación cada vez que quiera utilizando el comando (estando ubicado desde la carpeta de la aplicación):

```
Rscript runApp.R
```

La aplicación abrirá en el navegador por defecto del sistema. En caso de que su sistema no reconozca `Rscript` como un comando, deberá agregar el ejecutable de `Rscript` a la variable de entorno `PATH` o ejecutar el comando anterior utilizando la ruta completa del ejecutable. Por ejemplo:

```
C:\Program Files\R\R-4.0.2\bin\Rscript.exe runApp.R
```

<br>

#### **RStudio**
Para instalar los paquetes desde RStudio siga estos pasos una sola vez:

1. Haga click en File -> New Project...-> Existing Directory -> Browse. Navegue hasta la carpeta donde esta el contenido de la aplicación (e.g. `C:\Users\Foo\Documents\herramienta-para-verificar-calidad-de-datos`), seleccionela y haga click en Create Project.
2. Una vez se haya creado el proyecto y RStudio haya terminado de cargar, abra el archivo install.R. Este se puede abrir desde el panel de archivos (ubicado por defecto en la parte inferior derecha de RStudio). Haga click en Source para comenzar la instalación.
3. Una vez finalizada la instalación, cierre RStudio o reinicie la sesión de R (Crtl + Shift + F10) para reiniciar la sesión de R.

Para ejecutar la aplicación desde RStudio siga estos pasos:

1. Abra RStudio y haga click en File -> Open Project... Navegue hasta la carpeta donde esta el contenido de la aplicación y haga click en Open.
2. Una vez se haya creado el proyecto y RStudio haya terminado de cargar, abra el archivo runApp.R. Este se puede abrir desde el panel de archivos (ubicado por defecto en la parte inferior derecha de RStudio). Haga click en Run App y el navegador por defecto de su sistema abrirá una pestaña nueva con la aplicación.

<br>


## Estructura de archivos

| Archivo / Carpeta      | Descripción                                                                                                                                     |
|------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|
| db_to_dwc_simp.r       | Conversión de GDB a Darwin Core.                                                                                                                |
| divipola_codes.rds     | Contiene los códigos de las diviones políticas del país.                                                                                        |
| herram_estruct_datos.r | Correciones y transformaciones sobre los datos.                                                                                                 |
| install.R              | Instala los paquetes necesarios para correr la aplicación.                                                                                      |
| renv                   | Contiene el script de activación para utilizar la librería privada de la aplicación.                                                            |
| renv.lock              | Contiene los paquetes y sus versiones para la instalación.                                                                                      |
| runApp.r               | Punto de arranque para la aplicación.                                                                                                           |
| server.R               | Controla la conexión entre las funciones de los otros scripts y la interfaz gráfica.                                                            |
| ui.R                   | Contiene los elementos que se visualizan en la interfaz gráfica de la aplicación.                                                               |
| validacionGeografica.R | Función para validar que las coordenadas asociadas a cada registro se encuentren realmente dentro de los municipios y departamentos reportados. |
| taxon_Valid.r          | Función para validar listas de nombres científicos a partir de la función `gnr_resolve` del paquete `taxize`.                                   |

<br>

## Autores

* **Carolina Bello**
* **Carolina Castro**
* **Iván González**
* **Cristian Cruz** - [crcruzr](https://github.com/crcruzr)
* **Maria Cecilia Londoño**
* **Daniel López** - [danflop](https://github.com/danflop)
* **Elkin A. Noguera-Urbano** - [elkalexno](https://github.com/elkalexno)
* **María Helena Olaya-Rodríguez** - [heleolaya](https://github.com/heleolaya)
* **Juan Carlos Rey** - [biorey](https://github.com/biorey)
* **Erika Suárez** - [erikasv](https://github.com/erikasv)
* **Jorge Velásquez-Tibatá**
* **Marcelo Villa-Piñeros** - [marcelo-villa-p](https://github.com/marcelo-villa-p)


## Licencia

Este proyecto tiene licencia MIT; consulte el archivo [LICENSE](LICENSE) para obtener más detalles.
