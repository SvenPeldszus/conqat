class ZSAPLINK_TABLES definition
  public
  inheriting from ZSAPLINK
  final
  create public .

public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
endclass. "ZSAPLINK_TABLES definition



*----------------------------------------------------------------------*
*       class ZSAPLINK_TABLES implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZSAPLINK_TABLES implementation.


*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/
METHOD checkexists.

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: l_name TYPE ddobjname,
        dd02v_wa TYPE dd02v.
  l_name = objname.

  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = l_name
    IMPORTING
      dd02v_wa      = dd02v_wa
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc = 0 AND dd02v_wa-tabname IS NOT INITIAL.
    exists = 'X'.
  ENDIF.


ENDMETHOD.


*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/
METHOD createixmldocfromobject.

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: gotstate  TYPE ddgotstate,
        dd02v_wa  TYPE dd02v,
        dd09l_wa  TYPE dd09v,
        dd03p_tab TYPE STANDARD TABLE OF dd03p,
        dd03p_wa  LIKE LINE OF dd03p_tab,
        dd05m_tab TYPE STANDARD TABLE OF dd05m,
        dd05m_wa  LIKE LINE OF dd05m_tab,
        dd08v_tab TYPE STANDARD TABLE OF dd08v,
        dd08v_wa  LIKE LINE OF dd08v_tab,
        dd12v_tab TYPE STANDARD TABLE OF dd12v,
        dd12v_wa  LIKE LINE OF dd12v_tab,
        dd17v_tab TYPE STANDARD TABLE OF dd17v,
        dd17v_wa  LIKE LINE OF dd17v_tab,
        dd35v_tab TYPE STANDARD TABLE OF dd35v,
        dd35v_wa  LIKE LINE OF dd35v_tab,
        dd36m_tab TYPE STANDARD TABLE OF dd36m,
        dd36m_wa  LIKE LINE OF dd36m_tab.

*xml nodes
  DATA rootnode   TYPE REF TO if_ixml_element.
  DATA dd09l_node TYPE REF TO if_ixml_element.
  DATA dd03p_node TYPE REF TO if_ixml_element.
  DATA dd05m_node TYPE REF TO if_ixml_element.
  DATA dd08v_node TYPE REF TO if_ixml_element.
  DATA dd12v_node TYPE REF TO if_ixml_element.
  DATA dd17v_node TYPE REF TO if_ixml_element.
  DATA dd35v_node TYPE REF TO if_ixml_element.
  DATA dd36m_node TYPE REF TO if_ixml_element.
  DATA rc         TYPE sysubrc.
  DATA _tablname  TYPE ddobjname.
  _tablname = objname.

  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = _tablname
      langu         = sy-langu
    IMPORTING
      gotstate      = gotstate
      dd02v_wa      = dd02v_wa
      dd09l_wa      = dd09l_wa
    TABLES
      dd03p_tab     = dd03p_tab
      dd05m_tab     = dd05m_tab
      dd08v_tab     = dd08v_tab
      dd12v_tab     = dd12v_tab
      dd17v_tab     = dd17v_tab
      dd35v_tab     = dd35v_tab
      dd36m_tab     = dd36m_tab
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0 OR dd02v_wa-tabname IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>not_found.
  ENDIF.

* Create parent node
  DATA _objtype TYPE string.
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = dd02v_wa ).

  dd09l_node = xmldoc->create_element( 'dd09l' ).
  setattributesfromstructure( node = dd09l_node structure = dd09l_wa ).
  rc = rootnode->append_child( dd09l_node ).

  LOOP AT dd03p_tab INTO dd03p_wa.
    dd03p_node = xmldoc->create_element( 'dd03p' ).
    setattributesfromstructure( node = dd03p_node structure = dd03p_wa ).
    rc = rootnode->append_child( dd03p_node ).
  ENDLOOP.

  LOOP AT dd05m_tab INTO dd05m_wa.
    dd05m_node = xmldoc->create_element( 'dd05m' ).
    setattributesfromstructure( node = dd05m_node structure = dd05m_wa ).
    rc = rootnode->append_child( dd05m_node ).
  ENDLOOP.

  LOOP AT dd08v_tab INTO dd08v_wa.
    dd08v_node = xmldoc->create_element( 'dd08v' ).
    setattributesfromstructure( node = dd08v_node structure = dd08v_wa ).
    rc = rootnode->append_child( dd08v_node ).
  ENDLOOP.

  LOOP AT dd12v_tab INTO dd12v_wa.
    dd12v_node = xmldoc->create_element( 'dd12v' ).
    setattributesfromstructure( node = dd12v_node structure = dd12v_wa ).
    rc = rootnode->append_child( dd12v_node ).
  ENDLOOP.

  LOOP AT dd17v_tab INTO dd17v_wa.
    dd17v_node = xmldoc->create_element( 'dd17v' ).
    setattributesfromstructure( node = dd17v_node structure = dd17v_wa ).
    rc = rootnode->append_child( dd17v_node ).
  ENDLOOP.

  LOOP AT dd35v_tab INTO dd35v_wa.
    dd35v_node = xmldoc->create_element( 'dd35v' ).
    setattributesfromstructure( node = dd35v_node structure = dd35v_wa ).
    rc = rootnode->append_child( dd35v_node ).
  ENDLOOP.

  LOOP AT dd36m_tab INTO dd36m_wa.
    dd36m_node = xmldoc->create_element( 'dd36m' ).
    setattributesfromstructure( node = dd36m_node structure = dd36m_wa ).
    rc = rootnode->append_child( dd36m_node ).
  ENDLOOP.

*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.
ENDMETHOD.


*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/
METHOD createobjectfromixmldoc.

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: gotstate  TYPE ddgotstate,
        dd02v_wa  TYPE dd02v,
        dd09l_wa  TYPE dd09v,
        dd03p_tab TYPE STANDARD TABLE OF dd03p,
        dd03p_wa  LIKE LINE OF dd03p_tab,
        dd05m_tab TYPE STANDARD TABLE OF dd05m,
        dd05m_wa  LIKE LINE OF dd05m_tab,
        dd08v_tab TYPE STANDARD TABLE OF dd08v,
        dd08v_wa  LIKE LINE OF dd08v_tab,
        dd12v_tab TYPE STANDARD TABLE OF dd12v,
        dd12v_wa  LIKE LINE OF dd12v_tab,
        dd17v_tab TYPE STANDARD TABLE OF dd17v,
        dd17v_wa  LIKE LINE OF dd17v_tab,
        dd35v_tab TYPE STANDARD TABLE OF dd35v,
        dd35v_wa  LIKE LINE OF dd35v_tab,
        dd36m_tab TYPE STANDARD TABLE OF dd36m,
        dd36m_wa  LIKE LINE OF dd36m_tab.

*xml nodes
  DATA rootnode    TYPE REF TO if_ixml_element.
  DATA dd09l_node  TYPE REF TO if_ixml_element.
  DATA dd03p_node  TYPE REF TO if_ixml_element.
  DATA dd05m_node  TYPE REF TO if_ixml_element.
  DATA dd08v_node  TYPE REF TO if_ixml_element.
  DATA dd12v_node  TYPE REF TO if_ixml_element.
  DATA dd17v_node  TYPE REF TO if_ixml_element.
  DATA dd35v_node  TYPE REF TO if_ixml_element.
  DATA dd36m_node  TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.
  DATA rc          TYPE sysubrc.
  DATA _tablname   TYPE ddobjname.
  DATA _devclass   TYPE devclass.
  DATA checkexists TYPE flag.
  DATA _objtype    TYPE string.

  _devclass = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = dd02v_wa.

  objname = dd02v_wa-tabname.

  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

  dd09l_node = xmldoc->find_from_name( 'dd09l' ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = dd09l_node
    CHANGING
      structure = dd09l_wa.

* retrieve Tabl details
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd03p' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd03p_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd03p_wa.
    APPEND dd03p_wa TO dd03p_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd05m' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd05m_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd05m_wa.
    APPEND dd05m_wa TO dd05m_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd08v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd08v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd08v_wa.
    APPEND dd08v_wa TO dd08v_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd12v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd12v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd12v_wa.
    APPEND dd12v_wa TO dd12v_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd17v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd17v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd17v_wa.
    APPEND dd17v_wa TO dd17v_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd35v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd35v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd35v_wa.
    APPEND dd35v_wa TO dd35v_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd36m' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd36m_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd36m_wa.
    APPEND dd36m_wa TO dd36m_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  DATA : l_pgmid         TYPE tadir-pgmid,
           l_object      TYPE tadir-object,
           l_obj_name    TYPE tadir-obj_name,
           l_dd_objname  TYPE ddobjname,
           l_srcsystem   TYPE tadir-srcsystem,
           l_author      TYPE tadir-author,
           l_devclass    TYPE tadir-devclass,
           l_masterlang  TYPE tadir-masterlang.


  l_pgmid      = 'R3TR'.
  l_object     = _objtype.
  l_obj_name   = objname.
  l_dd_objname = objname.
  l_srcsystem  = sy-sysid.
  l_author     = sy-uname.
  l_devclass   = _devclass.
  l_masterlang = sy-langu.

  DATA: itadir TYPE tadir.
  itadir-pgmid      = l_pgmid.
  itadir-object     = l_object.
  itadir-obj_name   = l_obj_name.
  itadir-srcsystem  = l_srcsystem.
  itadir-author     = l_author.
  itadir-devclass   = l_devclass.
  itadir-masterlang = l_masterlang.
  MODIFY tadir FROM itadir.

  CALL FUNCTION 'TR_TADIR_INTERFACE'
    EXPORTING
      wi_test_modus                  = ' '
      wi_delete_tadir_entry          = 'X'
      wi_tadir_pgmid                 = l_pgmid
      wi_tadir_object                = l_object
      wi_tadir_obj_name              = l_obj_name
      wi_tadir_srcsystem             = l_srcsystem
      wi_tadir_author                = l_author
      wi_tadir_devclass              = l_devclass
      wi_tadir_masterlang            = l_masterlang
      iv_set_edtflag                 = ''
    EXCEPTIONS
      tadir_entry_not_existing       = 1
      tadir_entry_ill_type           = 2
      no_systemname                  = 3
      no_systemtype                  = 4
      original_system_conflict       = 5
      object_reserved_for_devclass   = 6
      object_exists_global           = 7
      object_exists_local            = 8
      object_is_distributed          = 9
      obj_specification_not_unique   = 10
      no_authorization_to_delete     = 11
      devclass_not_existing          = 12
      simultanious_set_remove_repair = 13
      order_missing                  = 14
      no_modification_of_head_syst   = 15
      pgmid_object_not_allowed       = 16
      masterlanguage_not_specified   = 17
      devclass_not_specified         = 18
      specify_owner_unique           = 19
      loc_priv_objs_no_repair        = 20
      gtadir_not_reached             = 21
      object_locked_for_order        = 22
      change_of_class_not_allowed    = 23
      no_change_from_sap_to_tmp      = 24
      OTHERS                         = 25.
  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
      WHEN 11 OR 23 OR 24.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_authorized.
      WHEN 22.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>locked.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>system_error.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'DDIF_TABL_PUT'
    EXPORTING
      name              = l_dd_objname
      dd02v_wa          = dd02v_wa
      dd09l_wa          = dd09l_wa
    TABLES
      dd03p_tab         = dd03p_tab
      dd05m_tab         = dd05m_tab
      dd08v_tab         = dd08v_tab
      dd35v_tab         = dd35v_tab
      dd36m_tab         = dd36m_tab
    EXCEPTIONS
      tabl_not_found    = 1
      name_inconsistent = 2
      tabl_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>system_error.
  ENDIF.

  DATA: trobjtype  TYPE trobjtype,
        trobj_name TYPE trobj_name.
  trobjtype  = l_object.
  trobj_name = l_obj_name.
  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object            = trobjtype
      obj_name          = trobj_name
    EXCEPTIONS
      wrong_object_name = 1.

  name = objName.
ENDMETHOD.


*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/
method DELETEOBJECT.

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

endmethod.


*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/
method GETOBJECTTYPE.

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  objecttype = 'TABL'.  "Tables and Structures
endmethod.
endclass. "ZSAPLINK_TABLES implementation