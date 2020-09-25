# BioRegistros-app
Herramienta para la verificación geográfica y taxonómica de registros biológicos.

V0.1.0

## Por dónde empezar

A continuación se presentan instrucciones para ejecutar el proyecto en su máquina local, para propósitos de desarrollo y pruebas. En la sección de despliegue están las instrucciones par ponerlo en producción.

### Prerrequisitos

Es necesario tener instalado:

* [R v3.6.3+](https://www.r-project.org/)
* [GDAL](https://gdal.org/)

### Instalación y ejecución

La aplicación verifica e instala otros paquetes de R en caso de que sean necesarios. Sin embargo, puede suceder que algún paquete requiera una librería del sistema, esto se verá reflejado en el log de errores al ejecutar la aplicación. Específicamente, el paquete `sf` requiere algunas librerías. Una guía para instalarlas en diferentes sistemas operativos se encuentra [acá](https://r-spatial.github.io/sf/#installing).

La aplicación lee dos variables de ambiente para determinar el directorio de trabajo (*WORK_DIR*) y la ruta de instalación de dependencias (*LIB_PATH*), si estas no se han configurado al momento de ejecución, se tomarán los valores por defecto en el sistema.

#### Ubuntu
Para ejecutar en ubuntu se puede usar RScripts:

```
WORK_DIR=<rutal al codigo fuente> LIB_PATH=<ruta de instalación de paquetes> Rscript runApp.r
```

También se puede usar R directamente:
```
WORK_DIR=/home/esuarez/bioregistros/bioregistros-app LIB_PATH=/home/esuarez/R R --vanilla < runApp.r
```

#### Windows
Para ejecutar la aplicación en Windows, ejecute el script `runApp.r` desde la carpeta donde se encuentra el archivo. Esto abrirá una pestaña en su navegador por defecto con la aplicación.


## Autores

* **Cristian Cruz** - [crcruzr](https://github.com/crcruzr)
* **Helena Olaya** - [heleolaya](https://github.com/heleolaya)

Consulte también la lista de [colaboradores](https://github.com/PEM-Humboldt/bioregistros-app/contributors) que participaron en este proyecto.

## Licencia

Este proyecto tiene licencia MIT; consulte el archivo [LICENSE](LICENSE) para obtener más detalles.
