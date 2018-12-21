class ZCX_CQSE_SRCOBJ_NOT_EXISTS definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
endclass. "ZCX_CQSE_SRCOBJ_NOT_EXISTS definition



*----------------------------------------------------------------------*
*       class ZCX_CQSE_SRCOBJ_NOT_EXISTS implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCX_CQSE_SRCOBJ_NOT_EXISTS implementation.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
endmethod.
endclass. "ZCX_CQSE_SRCOBJ_NOT_EXISTS implementation