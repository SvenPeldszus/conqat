*----------------------------------------------------------------------*
*       CLASS YCL_DEPENDENCY_A DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class YCL_DEPENDENCY_A definition
  public
  inheriting from YCL_DEPENDENCY_B
  create public .

public section.

  methods MY_METHOD
    importing
      !P1 type STRING
    returning
      value(R) type STRING .
protected section.
  PRIVATE SECTION.
endclass. "YCL_DEPENDENCY_A definition



*----------------------------------------------------------------------*
*       class YCL_DEPENDENCY_A implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class YCL_DEPENDENCY_A implementation.


  METHOD my_method.

    write / 'This is method my_method of ycl_dependency_a. I was called with parameter '.
    write p1.

    r = 'success'.

    endmethod.
endclass. "YCL_DEPENDENCY_A implementation