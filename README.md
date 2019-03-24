# Lppf: Logic Programs with Partial Functions

La aplicación es un sistema para la implementación de sistemas basados en reglas, en concreto, en programas de *Answer Set Programming* con funciones, y la obtención de sus resultados junto a una explicación de los mismos. Las explicaciones expresadas por la herramienta, consisten en redes causales en forma de árbol que explican los resultados basándose en las reglas del programa original. La herramienta permite manipular su apariencia y expresividad usando un sistema de etiquetado de reglas que admite la expresión de explicaciones en lenguaje natural. También es posible generar informes que contienen las explicaciones representadas en forma de grafos dirigidos dentro de imágenes. La herramienta desarrollada, se ha aplicado en un escenario real para resolver un problema del dominio médico, más concretamente, la tarea de emparejamiento entre donante-receptor de un trasplante de hı́gado. El sistema valora la utilidad del trasplante (la probabilidad del éxito del caso) empleando un conjunto de reglas que asocian una puntuación a cada caso en función, tanto del estado de salud del receptor, como de los datos del donante, y justifica los resultados apoyándose en las explicaciones.

## Ficheros

* **scanner.l**: Analizador léxico (flex/lex).
* **parser.y**: Analizador sintáctico (bison/yacc)
* **getterms.c + getterms.h**: Funciones útiles para usar en *scanner.l* y *parser.y*.
* **lppf.pl**: Código fuente del programa lppf *(usa util.pl, solutions.pl, display.pl, translate.pl, loadfile.pl, explanations.pl, htmlReportStyle.pl)*.

## Repositorio

* **master**: Estado del repositorio al acabar cada sprint.
* **develop**: Rama base para el desarrollo.

## Uso

Para generar el ejecutable:
```bash
make getterms
make lppf
```

Para ejecutarlo:
```bash
./lppf -n 0 -o salida ex/family.txt
```

**NOTA**: Para poder compilar y ejecutar el programa son necesarios:
* gcc
* flex
* bison
* swipl
* el ejecutable 'getterms' en el directorio de ejecución
* clingo
