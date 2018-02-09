## Ficheros

* **scanner.l**: Analizador léxico (flex/lex).
* **parser.y**: Analizador sintáctico (bison/yacc)
* **getterms.c + getterms.h**: Funciones útiles para usar en *scanner.l* y *parser.y*.
* **lppf.pl**: Código fuente del programa lppf *(usa util.pl, solutions.pl, display.pl, translate.pl, loadfile.pl)*.

## Repositorio

* **master**: Estado del repositorio al acabar cada sprint.
* **develop**: Rama base para el desarrollo.
* **getterms/xxx**: Los diferentes cambios en los analizadores léxico y sintáctico. Mergean contra develop.

## Uso

Para generar el ejecutable:
```bash
make
```

Para ejecutarlo:
```bash
./lppf -n 0 -o salida reglas.txt
```

**NOTA**: Para poder compilar y ejecutar el programa son necesarios:
* gcc
* flex
* bison
* el ejecutable 'getterms' en el directorio de ejecución
* clingo