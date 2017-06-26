package edu.umass.cs.iesl.watr
package simstring

import heuristics.Constants._

object DepartmentKeywords {

    final val keywords = FuzzyMap(List(
        ("laboratory", DEPARTMENT_KEYWORD),
        ("laboratorio", INSTITUTION_KEYWORD),
        ("laboratoire", INSTITUTION_KEYWORD),
        ("dept", DEPARTMENT_KEYWORD),
        ("dept.", DEPARTMENT_KEYWORD),
        ("unidade", DEPARTMENT_KEYWORD),
        ("einheit", DEPARTMENT_KEYWORD),
        ("unite", DEPARTMENT_KEYWORD),
        ("unidad", DEPARTMENT_KEYWORD),
        ("uita", DEPARTMENT_KEYWORD),
        ("branch", DEPARTMENT_KEYWORD),
        ("ramo", DEPARTMENT_KEYWORD),
        ("rama", DEPARTMENT_KEYWORD),
        ("zweig", DEPARTMENT_KEYWORD),
        ("branche", DEPARTMENT_KEYWORD),
        ("section", DEPARTMENT_KEYWORD),
        ("seccion", DEPARTMENT_KEYWORD),
        ("secao", DEPARTMENT_KEYWORD),
        ("abschnitt", DEPARTMENT_KEYWORD),
        ("division", DEPARTMENT_KEYWORD),
        ("divisao", DEPARTMENT_KEYWORD),
        ("divisione", DEPARTMENT_KEYWORD),
        ("program", DEPARTMENT_KEYWORD),
        ("programma", DEPARTMENT_KEYWORD),
        ("programm", DEPARTMENT_KEYWORD),
        ("Programa", DEPARTMENT_KEYWORD),
        ("programme", DEPARTMENT_KEYWORD),
        ("teilung", DEPARTMENT_KEYWORD),
        ("department", DEPARTMENT_KEYWORD),
        ("département", DEPARTMENT_KEYWORD),
        ("engineering", DEPARTMENT_KEYWORD),
        ("dipartimento", DEPARTMENT_KEYWORD),
        ("studi", DEPARTMENT_KEYWORD),
        ("degli", DEPARTMENT_KEYWORD),
        ("studies", DEPARTMENT_KEYWORD),
        ("servicio", DEPARTMENT_KEYWORD),
        ("departamento", DEPARTMENT_KEYWORD)))


}
