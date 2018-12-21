class ZSAPLINK_FUNCTIONGROUP definition
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

  methods ACTUALIZE_OBJECT_TREE .
  methods CREATE_TEXTPOOL
    importing
      !TEXTPOOLNODE type ref to IF_IXML_ELEMENT .
  methods CREATE_FUNCTION_MODULES
    importing
      !FM_NODE type ref to IF_IXML_ELEMENT
      !FCT_GROUP type TLIBG-AREA .
  methods CREATE_DOCUMENTATION
    importing
      !DOCNODE type ref to IF_IXML_ELEMENT .
  methods CREATE_FM_DOCUMENTATION
    importing
      !DOCNODE type ref to IF_IXML_ELEMENT .
  methods DEQUEUE_ABAP
    raising
      ZCX_SAPLINK .
  methods CREATE_INCLUDES
    importing
      !INCL_NODE type ref to IF_IXML_ELEMENT
      !DEVCLASS type DEVCLASS default '$TMP' .
  methods GET_TEXTPOOL
    returning
      value(TEXTNODE) type ref to IF_IXML_ELEMENT .
  methods GET_DOCUMENTATION
    returning
      value(DOCNODE) type ref to IF_IXML_ELEMENT .
  methods GET_FM_DOCUMENTATION
    importing
      !FM_NAME type ANY
    returning
      value(DOCNODE) type ref to IF_IXML_ELEMENT .
  methods GET_INCLUDES
    importing
      !MAIN_PROG type SY-REPID
      !FCT_GROUP type TLIBT-AREA
    returning
      value(INCL_NODE) type ref to IF_IXML_ELEMENT .
  methods CREATE_SOURCE
    importing
      !SOURCE type TABLE_OF_STRINGS
      !ATTRIBS type TRDIR .
  methods ENQUEUE_ABAP
    raising
      ZCX_SAPLINK .
  methods GET_FUNCTION_MODULES
    importing
      !FCT_GROUP type TLIBG-AREA
    returning
      value(FM_NODE) type ref to IF_IXML_ELEMENT .
  methods TRANSPORT_COPY
    importing
      !AUTHOR type SYUNAME
      !DEVCLASS type DEVCLASS
    raising
      ZCX_SAPLINK .
  methods GET_DYNPRO
    returning
      value(DYNP_NODE) type ref to IF_IXML_ELEMENT .
  methods CREATE_DYNPRO
    importing
      !DYNP_NODE type ref to IF_IXML_ELEMENT .
  methods GET_PFSTATUS
    returning
      value(PFSTAT_NODE) type ref to IF_IXML_ELEMENT .
  methods CREATE_PFSTATUS
    importing
      !PFSTAT_NODE type ref to IF_IXML_ELEMENT .
endclass. "ZSAPLINK_FUNCTIONGROUP definition



*----------------------------------------------------------------------*
*       class ZSAPLINK_FUNCTIONGROUP implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZSAPLINK_FUNCTIONGROUP implementation.


method ACTUALIZE_OBJECT_TREE.
  DATA: l_offset TYPE i.
  DATA: l_tree_string TYPE string.

  CONCATENATE 'PG_' 'SAPL' objname INTO l_tree_string.

* If we supported namespaces, the following code would be required
*  FIND ALL OCCURRENCES OF '/' IN objname MATCH OFFSET l_offset.
*  IF sy-subrc = 0.
*    l_tree_string  = objname.
*    REPLACE SECTION OFFSET l_offset LENGTH 1 OF  l_tree_string  WITH '/SAPL'.
*    CONCATENATE 'PG_' l_tree_string  INTO l_tree_string.
*  ELSE.
*    CONCATENATE 'PG_' 'SAPL' objname INTO l_tree_string.
*  ENDIF.

  CALL FUNCTION 'WB_TREE_ACTUALIZE'
    EXPORTING
      tree_name = l_tree_string.

endmethod.


method CHECKEXISTS.
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

  select single area from tlibg into objname where area = objname.
  if sy-subrc = 0.
    exists = 'X'.
  endif.

endmethod.


method CREATEIXMLDOCFROMOBJECT.
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

*      Plugin created by:
*      Rich Heilman
*      rich.heilman.jr@gmail.com

  types: begin of t_tlibt,
           area type tlibt-area,
           spras type tlibt-spras,
           areat type tlibt-areat,
         end of t_tlibt.

  data rootnode            type ref to if_ixml_element.
  data mainprognode        type ref to if_ixml_element.
  data includesnode        type ref to if_ixml_element.
  data functgroupnode      type ref to if_ixml_element.
  data functionmodulesnode type ref to if_ixml_element.
  data docNode             type ref to if_ixml_element.
  data textpoolnode        type ref to if_ixml_element.
  data dynpronode          type ref to if_ixml_element.
  data statusnode          type ref to if_ixml_element.
  data sourcenode          type ref to if_ixml_element.
  data fmdocumenation      type ref to if_ixml_element.

  data rc                type sysubrc.
  data progattribs       type trdir.
  data progsource        type rswsourcet.
  data _objname(30)      type c.
  data sourcestring      type string.
  data _objtype          type string.
  data functiongroupname type  tlibg-area.
  data mainfgprogname    type sy-repid.
  DATA l_offset          TYPE i.
  data xtlibt            type t_tlibt.

  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).

* function groups in reserved namespace, not supported.
  IF objname(1) = '/'.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = 'Function Groups in / namespace are not supported'.
  ENDIF.

*  create main program name.  Other namespaces are not supported
  CONCATENATE 'SAPL' objname INTO mainfgprogname.

* If we did support namespaces, this is how we would
* build the main program name
*  FIND ALL OCCURRENCES OF '/' IN objname MATCH OFFSET l_offset.
*  IF sy-subrc = 0.
*    mainfgprogname = objname.
*    REPLACE SECTION OFFSET l_offset LENGTH 1 OF mainfgprogname WITH '/SAPL'.
*  ELSE.
*    CONCATENATE 'SAPL' objname INTO mainfgprogname.
*  ENDIF.

* Set function group name
  functiongroupname = objname.

* Get main program attributes
  select single * from trdir
           into progattribs
                where name = mainfgprogname.
  if sy-subrc <> 0.
    clear ixmldocument.
    RAISE EXCEPTION type zcx_saplink
      EXPORTING
        textid = zcx_saplink=>not_found.
  endif.

* Get Function group attributes
  clear xtlibt.
  select single * from tlibt
             into corresponding fields of xtlibt
                     where spras = sy-langu
                       and area  = functiongroupname.
  if sy-subrc <> 0.
    RAISE EXCEPTION type zcx_saplink
      EXPORTING
        textid = zcx_saplink=>not_found.
  endif.

  setattributesfromstructure( node = rootnode
                              structure =  xtlibt  ).

  _objname = objname.
  objname  = mainfgprogname.    " Main program is object

* Write main program for function group.
  mainprognode = xmldoc->create_element( 'mainprogram' ).
  setattributesfromstructure( node = mainprognode
                              structure =   progattribs  ).

  sourcenode = xmldoc->create_element( 'source' ).
  read report mainfgprogname into progsource.
  sourcestring = buildsourcestring( sourcetable = progsource ).
  rc = sourcenode->if_ixml_node~set_value( sourcestring ).

  textpoolnode =  get_textpool( ).
  rc = mainprognode->append_child( textpoolnode ).

  docNode = get_documentation( ).
  rc = rootNOde->append_child( docNode ).

  dynpronode = get_dynpro( ).
  rc = mainprognode->append_child( dynpronode ).

  statusnode =  get_pfstatus( ).
  rc = mainprognode->append_child( statusnode ).

  rc = mainprognode->append_child( sourcenode ).
  rc = rootnode->append_child( mainprognode ).

* Get the includes
  includesnode = get_includes( main_prog = mainfgprogname
                               fct_group = functiongroupname ).
  rc = rootnode->append_child( includesnode ).

* Get function modules data.
  functionmodulesnode = get_function_modules( functiongroupname ).
  rc = rootnode->append_child( functionmodulesnode ).

  rc = xmldoc->append_child( rootnode ).

  ixmldocument = xmldoc.
  objname      =  _objname.

endmethod.


method CREATEOBJECTFROMIXMLDOC.
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

*      Plugin created by:
*      Rich Heilman
*      rich.heilman.jr@gmail.com

  TYPES: BEGIN OF t_tlibt,
           area TYPE tlibt-area,
           spras TYPE tlibt-spras,
           areat TYPE tlibt-areat,
         END OF t_tlibt.

  DATA rootnode      TYPE REF TO if_ixml_element.
  DATA sourcenode    TYPE REF TO if_ixml_element.
  DATA textnode      TYPE REF TO if_ixml_element.
  DATA docnode       TYPE REF TO if_ixml_element.
  DATA dynpnode      TYPE REF TO if_ixml_element.
  DATA statnode      TYPE REF TO if_ixml_element.

  DATA mainprog_node        TYPE REF TO if_ixml_element.
  DATA functionmodule_node  TYPE REF TO if_ixml_element.
  DATA functionmodules_node TYPE REF TO if_ixml_element.
  DATA includes_node        TYPE REF TO if_ixml_element.
  DATA fmdoc_node      TYPE REF TO if_ixml_element.

  DATA progattribs   TYPE trdir.
  DATA source        TYPE string.
  DATA sourcetable   TYPE table_of_strings.
  DATA _objname(30)  TYPE c.
  DATA _objtype      TYPE string.
  DATA checkexists   TYPE flag.

  DATA xtlibt TYPE t_tlibt.
  DATA xstext TYPE tftit-stext.

  DATA functiongroupname TYPE  tlibg-area.

  _objtype = getobjecttype( ).
  xmldoc   = ixmldocument.

  rootnode = xmldoc->find_from_name( _objtype ).

  _objname = objname.

  getstructurefromattributes(
           EXPORTING
                node      = rootnode
           CHANGING
                structure = xtlibt ).

  functiongroupname = xtlibt-area.

* function groups in reserved namespace, not supported.
  IF functiongroupname(1) = '/'.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = 'Function Groups in / namespace are not supported'.
  ENDIF.

  objname = functiongroupname.

  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

* Insert the function group
  xstext = xtlibt-areat.
  CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
    EXPORTING
      function_pool           = xtlibt-area
      short_text              = xstext
      devclass                = devclass
    EXCEPTIONS
      name_already_exists     = 1
      name_not_correct        = 2
      function_already_exists = 3
      invalid_function_pool   = 4
      invalid_name            = 5
      too_many_functions      = 6
      no_modify_permission    = 7
      no_show_permission      = 8
      enqueue_system_failure  = 9
      canceled_in_corr        = 10
      undefined_error         = 11
      OTHERS                  = 12.

* Create the function modules
  functionmodules_node  = rootnode->find_from_name( 'functionmodules' ).
  create_function_modules( fm_node = functionmodules_node
                           fct_group =  functiongroupname ).

* Create Includes
  includes_node  = rootnode->find_from_name( 'includeprograms' ).
  create_includes( devclass = devclass
                   incl_node = includes_node ).

* Update main program..... with include statements, dynpros, gui status
  mainprog_node  = rootnode->find_from_name( 'mainprogram' ).

  getstructurefromattributes(
         EXPORTING
            node      = mainprog_node
         CHANGING
            structure = progattribs ).

  objname = progattribs-name.     " Main Program Name is now the object

* Update the main program
  enqueue_abap( ).
  transport_copy( author = progattribs-cnam
                  devclass = devclass ).

* Source
  sourcenode  = mainprog_node->find_from_name( 'source' ).
  source      = sourcenode->get_value( ).
  sourcetable = buildtablefromstring( source ).
  create_source( source = sourcetable
                 attribs = progattribs ).

* Documentation
  docnode = rootnode->find_from_name( 'functionGroupDocumentation' ).
  create_documentation( docnode ).

* text pool
  textnode = mainprog_node->find_from_name( 'textPool' ).
  create_textpool( textnode ).

* Dynpros
  dynpnode = mainprog_node->find_from_name( 'dynpros' ).
  create_dynpro( dynpnode ).

* Gui status, titles
  statnode = mainprog_node->find_from_name( 'pfstatus' ).
  create_pfstatus( statnode ).

  dequeue_abap( ).

* Rebuild tree structure for SE80
  actualize_object_tree( ).

* successful install
  objname = functiongroupname.
  name = objname.

endmethod.


method CREATE_DOCUMENTATION.

  DATA txtline_node     TYPE REF TO if_ixml_element.
  DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
  DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

  DATA lang_node     TYPE REF TO if_ixml_element.
  DATA lang_filter   TYPE REF TO if_ixml_node_filter.
  DATA lang_iterator TYPE REF TO if_ixml_node_iterator.

  data obj_name type DOKHL-OBJECT.
  data prog_name type string.
  data language  type string.
  data obj_langu type DOKHL-LANGU.
  data lv_str type string.
  data rc type sy-subrc.

  DATA lt_lines  TYPE TABLE OF tline.
  FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

  if docnode is not bound.
    return.
  endif.

  prog_name = docNode->get_attribute( name = 'OBJECT' ).
  obj_name = prog_name.

* If no prog name, then there was no program documenation, just return.
  if prog_name is initial.
    return.
  endif.

* Get languages from XML
  FREE: lang_filter, lang_iterator, lang_node.
  lang_filter = docNode->create_filter_name( `language` ).
  lang_iterator = docNode->create_iterator_filtered( lang_filter ).
  lang_node ?= lang_iterator->get_next( ).
  WHILE lang_node IS NOT INITIAL.

    refresh lt_lines.
    language = lang_node->get_attribute( name = 'SPRAS' ).
    obj_langu = language.

* Get TextLines from XML
    FREE: txtline_filter, txtline_iterator, txtline_node.
    txtline_filter = lang_node->create_filter_name( `textLine` ).
    txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
    txtline_node ?= txtline_iterator->get_next( ).
    WHILE txtline_node IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
      me->getstructurefromattributes(
              EXPORTING   node      = txtline_node
              CHANGING    structure = <ls_lines> ).
      txtline_node ?= txtline_iterator->get_next( ).
    ENDWHILE.

* Delete any documentation that may currently exist.
    CALL FUNCTION 'DOCU_DEL'
      EXPORTING
        id       = 'RE'   "<-- Report/program documentation
        langu    = obj_langu
        object   = obj_name
        typ      = 'E'
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.

* Now update with new documentation text
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id       = 'RE'
        langu    = obj_langu
        object   = obj_name
        typ      = 'E'
      TABLES
        line     = lt_lines
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = `Program Documentation object import failed`.
    ENDIF.

    lang_node ?= lang_iterator->get_next( ).
  ENDWHILE.

endmethod.


method CREATE_DYNPRO.
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

  types: begin of tdyn_head_temp.
         include type d020s.
  types: dtext type d020t-dtxt.
  types: end of tdyn_head_temp.

  data: idyn_fldl type table of d021s,
        idyn_flow type table of d022s,
        idyn_mcod type table of d023s.

  data: xdyn_head type  d020s,
        xdyn_fldl type  d021s,
        xdyn_flow type  d022s,
        xdyn_mcod type  d023s.

  data: xdyn_text_string type string.
  data: xdyn_text        type d020t-dtxt .
  data: xdyn_head_temp   type tdyn_head_temp.

  data _objname type trobj_name.

  data dynpros_node       type ref to if_ixml_element.
  data dynpros_filter     type ref to if_ixml_node_filter.
  data dynpros_iterator   type ref to if_ixml_node_iterator.

  data dynpro_node        type ref to if_ixml_element.
  data dynpro_filter      type ref to if_ixml_node_filter.
  data dynpro_iterator    type ref to if_ixml_node_iterator.

  data dynfldl_node       type ref to if_ixml_element.
  data dynfldl_filter     type ref to if_ixml_node_filter.
  data dynfldl_iterator   type ref to if_ixml_node_iterator.

  data dynmcod_node       type ref to if_ixml_element.
  data dynmcod_filter     type ref to if_ixml_node_filter.
  data dynmcod_iterator   type ref to if_ixml_node_iterator.

  data dynflow_node       type ref to if_ixml_element.

  data xdynpro_flow_source type string.
  data idynpro_flow_source type table_of_strings.

  _objname = objname.

  dynpros_node =  dynp_node.
  check dynpros_node is not initial.

  free: dynpro_filter, dynpro_iterator, dynpro_node.
  dynpro_filter = dynpros_node->create_filter_name( 'dynpro' ).
  dynpro_iterator =
        dynpros_node->create_iterator_filtered( dynpro_filter ).
  dynpro_node ?= dynpro_iterator->get_next( ).

  while dynpro_node is not initial.

    clear:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
    refresh:  idyn_fldl, idyn_flow, idyn_mcod.

* Get the header data for the screen.
    call method getstructurefromattributes
      exporting
        node      = dynpro_node
      changing
        structure = xdyn_head_temp.

    xdyn_head    = xdyn_head_temp.
    xdyn_text    = xdyn_head_temp-dtext.

* Retrieve field list
    free: dynfldl_filter, dynfldl_iterator, dynfldl_node.
    dynfldl_filter = dynpro_node->create_filter_name( 'dynprofield' ).
    dynfldl_iterator =
        dynpro_node->create_iterator_filtered( dynfldl_filter ).
    dynfldl_node ?= dynfldl_iterator->get_next( ).
    while dynfldl_node is not initial.
      call method getstructurefromattributes
        exporting
          node      = dynfldl_node
        changing
          structure = xdyn_fldl.
      append xdyn_fldl to idyn_fldl.
      dynfldl_node ?= dynfldl_iterator->get_next( ).
    endwhile.

* Retrieve matchcode data.
    free: dynmcod_filter, dynmcod_iterator, dynmcod_node.
    dynmcod_filter = dynpro_node->create_filter_name( 'dynprofield' ).
    dynmcod_iterator =
         dynpro_node->create_iterator_filtered( dynmcod_filter ).
    dynmcod_node ?= dynmcod_iterator->get_next( ).
    while dynmcod_node is not initial.
      call method getstructurefromattributes
        exporting
          node      = dynmcod_node
        changing
          structure = xdyn_mcod.
      append xdyn_mcod to idyn_mcod.
      dynmcod_node ?= dynmcod_iterator->get_next( ).
    endwhile.

* retieve flow logic source.
    clear xdynpro_flow_source.  refresh idynpro_flow_source.
    clear xdyn_flow.            refresh idyn_flow.
    free dynflow_node.
    dynflow_node = dynpro_node->find_from_name( 'dynproflowsource' ).
    xdynpro_flow_source  = dynflow_node->get_value( ).
    idynpro_flow_source = buildtablefromstring( xdynpro_flow_source ).
    loop at idynpro_flow_source into xdyn_flow.
      append xdyn_flow  to idyn_flow.
    endloop.

* Build dynpro from data
    call function 'RPY_DYNPRO_INSERT_NATIVE'
      exporting
*       suppress_corr_checks           = ' '
*       CORRNUM                        = ' '
        header                         = xdyn_head
        dynprotext                     = xdyn_text
*       SUPPRESS_EXIST_CHECKS          = ' '
*       USE_CORRNUM_IMMEDIATEDLY       = ' '
*       SUPPRESS_COMMIT_WORK           = ' '
      tables
        fieldlist                      = idyn_fldl
        flowlogic                      = idyn_flow
        params                         = idyn_mcod
     exceptions
        cancelled                      = 1
        already_exists                 = 2
        program_not_exists             = 3
        not_executed                   = 4
        others                         = 5.
    if sy-subrc <> 0.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>system_error.
    endif.

    dynpro_node ?= dynpro_iterator->get_next( ).

  endwhile.

endmethod.


method CREATE_FM_DOCUMENTATION.

  DATA txtline_node     TYPE REF TO if_ixml_element.
  DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
  DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

  DATA lang_node     TYPE REF TO if_ixml_element.
  DATA lang_filter   TYPE REF TO if_ixml_node_filter.
  DATA lang_iterator TYPE REF TO if_ixml_node_iterator.

  data obj_name type DOKHL-OBJECT.
  data fm_parm_name type string.
  data language  type string.
  data obj_langu type DOKHL-LANGU.
  data lv_str type string.
  data rc type sy-subrc.

  DATA lt_lines  TYPE TABLE OF tline.
  FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

  if docnode is not bound.
    return.
  endif.

  fm_parm_name = docNode->get_attribute( name = 'OBJECT' ).
  obj_name = fm_parm_name.

* If no fm_parm_name, then there was no documenation, just return.
  if fm_parm_name is initial.
    return.
  endif.

* Get languages from XML
  FREE: lang_filter, lang_iterator, lang_node.
  lang_filter = docNode->create_filter_name( `language` ).
  lang_iterator = docNode->create_iterator_filtered( lang_filter ).
  lang_node ?= lang_iterator->get_next( ).
  WHILE lang_node IS NOT INITIAL.

    refresh lt_lines.
    language = lang_node->get_attribute( name = 'SPRAS' ).
    obj_langu = language.

* Get TextLines from XML
    FREE: txtline_filter, txtline_iterator, txtline_node.
    txtline_filter = lang_node->create_filter_name( `textLine` ).
    txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
    txtline_node ?= txtline_iterator->get_next( ).
    WHILE txtline_node IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
      me->getstructurefromattributes(
              EXPORTING   node      = txtline_node
              CHANGING    structure = <ls_lines> ).
      txtline_node ?= txtline_iterator->get_next( ).
    ENDWHILE.

* Delete any documentation that may currently exist.
    CALL FUNCTION 'DOCU_DEL'
      EXPORTING
        id       = 'FU'   "<-- function module documentation
        langu    = obj_langu
        object   = obj_name
        typ      = 'T'
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.

* Now update with new documentation text
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id       = 'FU'
        langu    = obj_langu
        object   = obj_name
        typ      = 'T'
      TABLES
        line     = lt_lines
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = `Program Documentation object import failed`.
    ENDIF.

    lang_node ?= lang_iterator->get_next( ).
  ENDWHILE.

endmethod.


method CREATE_FUNCTION_MODULES.
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

  TYPES: BEGIN OF tfunct_head,
           name TYPE rs38l-name,
           global TYPE rs38l-global,
           remote TYPE rs38l-remote,
           utask  TYPE rs38l-utask,
           stext  TYPE tftit-stext,
           area   TYPE rs38l-area,
           END OF tfunct_head.

  DATA functionmodules_node TYPE REF TO if_ixml_element.

  DATA source      TYPE string.
  DATA sourcetable TYPE table_of_strings.

  DATA functiongroupname TYPE  tlibg-area.
  DATA mainfgprogname    TYPE trdir-name.

  DATA xfunct_head TYPE tfunct_head.
  DATA iimport     TYPE TABLE OF rsimp.
  DATA ichange     TYPE TABLE OF rscha.
  DATA iexport     TYPE TABLE OF rsexp.
  DATA itables     TYPE TABLE OF rstbl.
  DATA iexcepl     TYPE TABLE OF rsexc.
  DATA idocume     TYPE TABLE OF rsfdo.
  DATA isource     TYPE TABLE OF rssource.
  DATA isource_new TYPE  rsfb_source.

  DATA ximport     TYPE  rsimp.
  DATA xchange     TYPE  rscha.
  DATA xexport     TYPE  rsexp.
  DATA xtables     TYPE  rstbl.
  DATA xexcepl     TYPE  rsexc.
  DATA xdocume     TYPE  rsfdo.
  DATA xsource     TYPE  rssource.
  DATA xsource_new LIKE LINE OF isource_new.

  DATA node          TYPE REF TO if_ixml_element.
  DATA filter        TYPE REF TO if_ixml_node_filter.
  DATA iterator      TYPE REF TO if_ixml_node_iterator.

  DATA im_node       TYPE REF TO if_ixml_element.
  DATA im_filter     TYPE REF TO if_ixml_node_filter.
  DATA im_iterator   TYPE REF TO if_ixml_node_iterator.

  DATA ex_node       TYPE REF TO if_ixml_element.
  DATA ex_filter     TYPE REF TO if_ixml_node_filter.
  DATA ex_iterator   TYPE REF TO if_ixml_node_iterator.

  DATA ch_node       TYPE REF TO if_ixml_element.
  DATA ch_filter     TYPE REF TO if_ixml_node_filter.
  DATA ch_iterator   TYPE REF TO if_ixml_node_iterator.

  DATA ta_node       TYPE REF TO if_ixml_element.
  DATA ta_filter     TYPE REF TO if_ixml_node_filter.
  DATA ta_iterator   TYPE REF TO if_ixml_node_iterator.

  DATA el_node       TYPE REF TO if_ixml_element.
  DATA el_filter     TYPE REF TO if_ixml_node_filter.
  DATA el_iterator   TYPE REF TO if_ixml_node_iterator.

  DATA dm_node       TYPE REF TO if_ixml_element.
  DATA dm_filter     TYPE REF TO if_ixml_node_filter.
  DATA dm_iterator   TYPE REF TO if_ixml_node_iterator.

  DATA sc_node       TYPE REF TO if_ixml_element.
  DATA sc_filter     TYPE REF TO if_ixml_node_filter.
  DATA sc_iterator   TYPE REF TO if_ixml_node_iterator.

  DATA scn_node       TYPE REF TO if_ixml_element.
  DATA scn_filter     TYPE REF TO if_ixml_node_filter.
  DATA scn_iterator   TYPE REF TO if_ixml_node_iterator.

  DATA fmdoc_node     TYPE REF TO if_ixml_element.

  functionmodules_node = fm_node.
  functiongroupname    = fct_group.

  IF functionmodules_node  IS NOT INITIAL.

    FREE: filter, iterator, node.
    filter =
         functionmodules_node->create_filter_name( 'functionmodule' ).
    iterator = functionmodules_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.

      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xfunct_head.

      REFRESH: iimport, ichange, iexport,
               itables, iexcepl, idocume, isource, isource_new.

* Get importing
      FREE: im_filter, im_iterator, im_node.
      im_filter = node->create_filter_name( 'importing' ).
      im_iterator = node->create_iterator_filtered( im_filter ).
      im_node ?= im_iterator->get_next( ).
      WHILE im_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = im_node
          CHANGING
            structure = ximport.
        APPEND ximport TO iimport.
        im_node ?= im_iterator->get_next( ).
      ENDWHILE.

* Get exporting
      FREE: ex_filter, ex_iterator, ex_node.
      ex_filter = node->create_filter_name( 'exporting' ).
      ex_iterator = node->create_iterator_filtered( ex_filter ).
      ex_node ?= ex_iterator->get_next( ).
      WHILE ex_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = ex_node
          CHANGING
            structure = xexport.
        APPEND xexport TO iexport.
        ex_node ?= ex_iterator->get_next( ).
      ENDWHILE.

* Get changing
      FREE: ch_filter, ch_iterator, ch_node.
      ch_filter = node->create_filter_name( 'changing' ).
      ch_iterator = node->create_iterator_filtered( ch_filter ).
      ch_node ?= ch_iterator->get_next( ).
      WHILE ch_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = ch_node
          CHANGING
            structure = xchange.
        APPEND xchange TO ichange.
        ch_node ?= ch_iterator->get_next( ).
      ENDWHILE.

* Get tables
      FREE: ta_filter, ta_iterator, ta_node.
      ta_filter = node->create_filter_name( 'tables' ).
      ta_iterator = node->create_iterator_filtered( ta_filter ).
      ta_node ?= ta_iterator->get_next( ).
      WHILE ta_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = ta_node
          CHANGING
            structure = xtables.
        APPEND xtables TO itables.
        ta_node ?= ta_iterator->get_next( ).
      ENDWHILE.

* Get exception list
      FREE: el_filter, el_iterator, el_node.
      el_filter = node->create_filter_name( 'exceptions' ).
      el_iterator = node->create_iterator_filtered( el_filter ).
      el_node ?= el_iterator->get_next( ).
      WHILE el_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = el_node
          CHANGING
            structure = xexcepl.
        APPEND xexcepl TO iexcepl.
        el_node ?= el_iterator->get_next( ).
      ENDWHILE.

* Get documentation
      FREE: dm_filter, dm_iterator, dm_node.
      dm_filter = node->create_filter_name( 'documentation' ).
      dm_iterator = node->create_iterator_filtered( dm_filter ).
      dm_node ?= dm_iterator->get_next( ).
      WHILE dm_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = dm_node
          CHANGING
            structure = xdocume.
        APPEND xdocume TO idocume.
        dm_node ?= dm_iterator->get_next( ).
      ENDWHILE.

* Get fm source

      FREE: sc_filter, sc_iterator, sc_node.
      sc_filter = node->create_filter_name( 'fm_source' ).
      sc_iterator = node->create_iterator_filtered( sc_filter ).
      sc_node ?= sc_iterator->get_next( ).
      WHILE sc_node IS NOT INITIAL.
        source = sc_node->get_value( ).
        sourcetable = buildtablefromstring( source ).
        LOOP AT sourcetable INTO xsource.
          APPEND xsource TO isource.
        ENDLOOP.
        sc_node ?= sc_iterator->get_next( ).
      ENDWHILE.

* Get fm source new
      FREE: scn_filter, scn_iterator, scn_node.
      scn_filter = node->create_filter_name( 'fm_source_new' ).
      scn_iterator = node->create_iterator_filtered( scn_filter ).
      scn_node ?= scn_iterator->get_next( ).
      WHILE scn_node IS NOT INITIAL.
        source = scn_node->get_value( ).
        sourcetable = buildtablefromstring( source ).
        LOOP AT sourcetable INTO xsource_new.
          APPEND xsource_new TO isource_new.
        ENDLOOP.
        scn_node ?= scn_iterator->get_next( ).
      ENDWHILE.

* INsert the function module
      CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
        EXPORTING
          funcname                = xfunct_head-name
          function_pool           = functiongroupname
          interface_global        = xfunct_head-global
          remote_call             = xfunct_head-remote
          update_task             = xfunct_head-utask
          short_text              = xfunct_head-stext
          save_active             = ' ' "<-- Need to set inactive
          new_source              = isource_new
        TABLES
          import_parameter        = iimport
          export_parameter        = iexport
          tables_parameter        = itables
          changing_parameter      = ichange
          exception_list          = iexcepl
          parameter_docu          = idocume
          source                  = isource
        EXCEPTIONS
          double_task             = 1
          error_message           = 2
          function_already_exists = 3
          invalid_function_pool   = 4
          invalid_name            = 5
          too_many_functions      = 6
          no_modify_permission    = 7
          no_show_permission      = 8
          enqueue_system_failure  = 9
          canceled_in_corr        = 10
          OTHERS                  = 11.

* Create function module documentation
      fmdoc_node = node->find_from_name( 'functionModuleDocumentation' ).
      create_fm_documentation( fmdoc_node ).

      node ?= iterator->get_next( ).
    ENDWHILE.

  ENDIF.

endmethod.


method CREATE_INCLUDES.
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

  types: begin of tinclude,
         name(40),
         end of tinclude.

  data iinclude type table of tinclude.
  data xinclude type tinclude.

  data inc_node       type ref to if_ixml_element.
  data inc_filter     type ref to if_ixml_node_filter.
  data inc_iterator   type ref to if_ixml_node_iterator.

  data progattribs type trdir.

  data includes_node     type ref to if_ixml_element.
  data includesourcenode type ref to if_ixml_element.

  data source      type string.
  data sourcetable type table_of_strings.

  includes_node = incl_node.

  check includes_node is not initial.

  free: inc_filter, inc_iterator, inc_node.
  inc_filter = includes_node->create_filter_name( 'include' ).
  inc_iterator = includes_node->create_iterator_filtered( inc_filter ).
  inc_node ?= inc_iterator->get_next( ).

  while inc_node is not initial.

    getstructurefromattributes(
          exporting
             node      = inc_node
          changing
             structure = progattribs ).

    includesourcenode = inc_node->find_from_name( 'include_source' ).
    source      = includesourcenode->get_value( ).
    sourcetable = buildtablefromstring( source ).

    objname = progattribs-name.   " Include Program Name is the object

    enqueue_abap( ).
    transport_copy( author = progattribs-cnam
                                       devclass = devclass ).
    create_source( source = sourcetable
                                      attribs = progattribs ).
    dequeue_abap( ).

    inc_node  ?=  inc_iterator->get_next( ).

  endwhile.

endmethod.


method CREATE_PFSTATUS.
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

  data: ista type table of rsmpe_stat,
        ifun type table of rsmpe_funt,
        imen type table of rsmpe_men,
        imtx type table of rsmpe_mnlt,
        iact type table of rsmpe_act,
        ibut type table of rsmpe_but,
        ipfk type table of rsmpe_pfk,
        iset type table of rsmpe_staf,
        idoc type table of rsmpe_atrt,
        itit type table of rsmpe_titt,
        ibiv type table of rsmpe_buts.

  data: xsta type rsmpe_stat,
        xfun type rsmpe_funt,
        xmen type rsmpe_men,
        xmtx type rsmpe_mnlt,
        xact type rsmpe_act,
        xbut type rsmpe_but,
        xpfk type rsmpe_pfk,
        xset type rsmpe_staf,
        xdoc type rsmpe_atrt,
        xtit type rsmpe_titt,
        xbiv type rsmpe_buts.

  data xtrkey type trkey.
  data xadm   type rsmpe_adm.
  data _program type  trdir-name.
  data _objname type trobj_name.

  data stat_node  type ref to if_ixml_element.
  data node       type ref to if_ixml_element.
  data filter     type ref to if_ixml_node_filter.
  data iterator   type ref to if_ixml_node_iterator.

  _objname = objname.

  stat_node =  pfstat_node.
  check stat_node is not initial.

* read pfstatus_sta node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_sta' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xsta.
    append xsta to ista.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_fun node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_fun' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xfun.
    append xfun to ifun.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_men node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_men' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xmen.
    append xmen to imen.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_mtx node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_mtx' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xmtx.
    append xmtx to imtx.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_act node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_act' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xact.
    append xact to iact.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_but node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_but' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xbut.
    append xbut to ibut.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_pfk node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_pfk' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xpfk.
    append xpfk to ipfk.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_set node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_set' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xset.
    append xset to iset.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_doc node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_doc' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xdoc.
    append xdoc to idoc.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_tit node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_tit' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xtit.
    append xtit to itit.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_biv node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_biv' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xbiv.
    append xbiv to ibiv.
    node ?= iterator->get_next( ).
  endwhile.

* Update the gui status
  _program = _objname.

  xtrkey-obj_type = 'PROG'.
  xtrkey-obj_name = _program.
  xtrkey-sub_type = 'CUAD'.
  xtrkey-sub_name = _program.

  call function 'RS_CUA_INTERNAL_WRITE'
    exporting
      program   = _program
      language  = sy-langu
      tr_key    = xtrkey
      adm       = xadm
      state     = 'I'
    tables
      sta       = ista
      fun       = ifun
      men       = imen
      mtx       = imtx
      act       = iact
      but       = ibut
      pfk       = ipfk
      set       = iset
      doc       = idoc
      tit       = itit
      biv       = ibiv
    exceptions
      not_found = 1
      others    = 2.

  if sy-subrc <> 0.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  endif.

endmethod.


method CREATE_SOURCE.
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
data _objName type TROBJ_NAME.
data progLine type PROGDIR.
data titleInfo type trdirti.
data reportLine type string.
data miniReport type table_of_strings.

  _objName = objName.
  call function 'RS_INSERT_INTO_WORKING_AREA'
        exporting
             OBJECT   = 'REPS'
             OBJ_NAME = _objName
        exceptions
             WRONG_OBJECT_NAME = 1.
   INSERT REPORT _objName FROM source STATE 'I'
     program type attribs-subc.  "added to handle includes, etc.
   MOVE 'I' TO progline-STATE.
   move-corresponding attribs to progline.
   modify progdir from progline.
*  Are you kidding me?!?  No idea why you need to do this!!
   CONCATENATE 'REPORT' _objName '.' INTO reportLine SEPARATED BY SPACE.
   append reportline to miniReport.
   INSERT REPORT _objName FROM miniReport STATE 'A'
     program type attribs-subc. "added to handle includes, etc.
   MOVE 'A' TO progline-STATE.
   modify progdir from progline.

endmethod.


method CREATE_TEXTPOOL.
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
data textPoolTable type standard table of textPool.
data textPoolRow type textPool.
data langIterator type ref to if_ixml_node_iterator.
data filter type ref to if_ixml_node_filter.
data textFilter type ref to if_ixml_node_filter.
data textIterator type ref to if_ixml_node_iterator.
data langNode type ref to if_ixml_element.
data aTextNode type ref to if_ixml_element.
data _objName type TROBJ_NAME.
data lang type spras.
data langNodeExists type flag.
data logonLanguageExists type flag.
data _state(1) type c.

  _objName = objName.

  filter = textPoolNode->create_filter_name( 'language' ).
  langIterator = textPoolNode->create_iterator_filtered( filter ).
  langNode ?= langIterator->get_next( ).

  while langNode is not initial.
    langNodeExists = 'X'.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
         EXPORTING
              OBJECT   = 'REPT'
              OBJ_NAME = _objName
         EXCEPTIONS
              OTHERS   = 0.

    refresh textPoolTable.
    textIterator = langNode->create_iterator( ).
    aTextNode ?= textIterator->get_next( ).
*For some reason the 1st one is blank... not sure why.
    aTextNode ?= textIterator->get_next( ).
    while aTextNode is not initial.
      call method GETSTRUCTUREFROMATTRIBUTES
            exporting
              node = aTextNode
            changing
              structure = textPoolRow.
      append textPoolRow to textPoolTable.
      aTextNode ?= textIterator->get_next( ).
    endwhile.
    if textPoolTable is not initial.
      lang = langNode->get_attribute( 'SPRAS' ).
      if lang = sy-langu.
        logonLanguageExists = 'X'.
        _state = 'I'.
      else.
*       seems that if a textpool is inserted as inactive for language
*       other than the logon language, it is lost upon activation
*       not sure inserting as active is best solution,but seems to work
        _state = 'A'.
      endif.
    endif.
    insert textpool _objName from textPooltable language lang
    state _state.
    langNode ?= langIterator->get_next( ).
  endwhile.
endmethod.


method DELETEOBJECT.
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

  data area type RS38L-AREA.

  area = objName.

  call function 'RS_FUNCTION_POOL_DELETE'
   EXPORTING
      AREA                         = area
*   CORRNUM                      = ' '
*   TEXT                         = ' '
*   UNAME                        = ' '
      WITH_KORR                    = ' '
*   WB_FB_MANAGER                =
      SUPPRESS_POPUPS              = 'X'
*   SKIP_PROGRESS_IND            = ' '
* IMPORTING
*   E_CORRNUM                    =
 EXCEPTIONS
   CANCELED_IN_CORR             = 1
   ENQUEUE_SYSTEM_FAILURE       = 2
   FUNCTION_EXIST               = 3
   NOT_EXECUTED                 = 4
   NO_MODIFY_PERMISSION         = 5
   NO_SHOW_PERMISSION           = 6
   PERMISSION_FAILURE           = 7
   POOL_NOT_EXIST               = 8
   CANCELLED                    = 9
   OTHERS                       = 10.
  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


endmethod.


method DEQUEUE_ABAP.
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

  call function 'RS_ACCESS_PERMISSION'
       exporting
            global_lock              = 'X'
            mode                     = 'FREE'
            object                   = objName
            object_class             = 'ABAP'
       exceptions
            canceled_in_corr         = 1
            enqueued_by_user         = 3
            enqueue_system_failure   = 4
            locked_by_author         = 5
            illegal_parameter_values = 6
            no_modify_permission     = 7
            no_show_permission       = 8
            permission_failure       = 9.

  if sy-subrc <> 0.
    case sy-subrc.
      when 7 or 8 or 9.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>not_authorized.
      when 5.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>error_message
            msg = 'object locked'.
      when others.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>system_error.
    endcase.
  endif.

endmethod.


method ENQUEUE_ABAP.
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


  call function 'RS_ACCESS_PERMISSION'
       exporting
*            authority_check          = authority_check
            global_lock              = 'X'
            mode                     = 'INSERT'
*            master_language          = trdir-rload
            object                   = objName
            object_class             = 'ABAP'
*       importing
*            transport_key            = trkey_global
*            new_master_language      = trdir-rload
*            devclass                 = devclass_local
       exceptions
            canceled_in_corr         = 1
            enqueued_by_user         = 3
            enqueue_system_failure   = 4
            locked_by_author         = 5
            illegal_parameter_values = 6
            no_modify_permission     = 7
            no_show_permission       = 8
            permission_failure       = 9.

  if sy-subrc <> 0.
    case sy-subrc.
      when 7 or 8 or 9.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>not_authorized.
      when 5.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>error_message
            msg = 'object locked'.
      when others.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>system_error.
    endcase.
  endif.

endmethod.


method GETOBJECTTYPE.
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
  objectType = 'FUGR'. " Function Group
endmethod.


method GET_DOCUMENTATION.

  data languageNode   type ref to if_ixml_element.
  DATA txtlines_node TYPE REF TO if_ixml_element.
  DATA rc            TYPE sysubrc.
  DATA _objtype      TYPE string.

  Types: BEGIN OF t_dokhl,
          id          TYPE dokhl-id,
          object      TYPE dokhl-object,
          langu       type dokhl-langu,
          typ         TYPE dokhl-typ,
          dokversion  TYPE dokhl-dokversion,
         END OF t_dokhl.

  data lt_dokhl type table of t_dokhl.
  data ls_dokhl like line of lt_dokhl.

  DATA lt_lines TYPE TABLE OF tline.
  DATA ls_lines LIKE LINE OF lt_lines.

  data lv_str type string.
  DATA _objname TYPE e071-obj_name.

  _objname = objname.

* Check against database
  SELECT  id object langu typ dokversion
        INTO corresponding fields of table lt_dokhl
           FROM dokhl
             WHERE id = 'RE'
                AND object = _objname.

* Use only most recent version.
  sort lt_dokhl by id object langu typ ascending dokversion descending.
  delete adjacent duplicates from lt_dokhl comparing id object typ langu.

  docNode = xmlDoc->create_element( 'functionGroupDocumentation' ).

* Make sure there is at least one record here.
  clear ls_dokhl.
  read table lt_dokhl into ls_dokhl index 1.
  if sy-subrc <> 0.
    return.
  endif.

* Set docNode object attribute
  lv_str = ls_dokhl-object.
  rc = docNode->set_attribute( name = 'OBJECT' value = lv_Str ).

  Loop at lt_dokhl into ls_dokhl.

* Create language node, and set attribute
    languageNode = xmlDoc->create_element( 'language' ).
    lv_str = ls_dokhl-langu.
    rc = languageNode->set_attribute( name = 'SPRAS' value = lv_Str ).

* Read the documentation text
    CALL FUNCTION 'DOCU_READ'
      EXPORTING
        id      = ls_dokhl-id
        langu   = ls_dokhl-langu
        object  = ls_dokhl-object
        typ     = ls_dokhl-typ
        version = ls_dokhl-dokversion
      TABLES
        line    = lt_lines.

* Write records to XML node
    LOOP AT lt_lines INTO ls_lines.
      txtlines_node = xmlDoc->create_element( `textLine` ).
      me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
      rc = languageNode->append_child( txtlines_node ).
    ENDLOOP.
    rc = docNode->append_child( languageNode ) .
  Endloop.

endmethod.


method GET_DYNPRO.
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

  types: begin of tdynp,
         prog type d020s-prog,
         dnum type d020s-dnum,
         end of tdynp.

  data: idyn_fldl type table of d021s,
        idyn_flow type table of d022s,
        idyn_mcod type table of d023s.

  data: xdyn_head type  d020s,
        xdyn_fldl type  d021s,
        xdyn_flow type  d022s,
        xdyn_mcod type  d023s.

  data idynp type table of tdynp.
  data xdynp type tdynp.

  data xdyn_text type d020t-dtxt.
  data xdyn_text_string type string.

  data _objname type trobj_name.
  data rc type sy-subrc .

  data iflowsource type rswsourcet.
  data xflowsource like line of iflowsource.
  data flowsourcestring type string.

  data dynnr_node type ref to if_ixml_element.
  data dynpromatchnode type ref to if_ixml_element.
  data dynprofieldsnode type ref to if_ixml_element.
  data dynproflownode type ref to if_ixml_element.

  _objname = objname.

* Get all dynpros for program object
  clear xdynp.  refresh idynp.
  select prog dnum into table idynp
                from d020s
                   where prog = _objname
                     and type <> 'S'    " No Selection Screens
                     and type <> 'J'.   " No selection subscreens
  check sy-subrc  = 0 .

  dynp_node = xmldoc->create_element( 'dynpros' ).

  loop at idynp into xdynp.

* Retrieve dynpro imformation
    dynnr_node =  xmldoc->create_element( 'dynpro' ).

    clear:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
    refresh:  idyn_fldl, idyn_flow, idyn_mcod.

    call function 'RPY_DYNPRO_READ_NATIVE'
      exporting
        progname                    = xdynp-prog
        dynnr                       = xdynp-dnum
*       SUPPRESS_EXIST_CHECKS       = ' '
*       SUPPRESS_CORR_CHECKS        = ' '
    importing
        HEADER                      = xdyn_head
        dynprotext                  = xdyn_text
     tables
        fieldlist                   = idyn_fldl
        flowlogic                   = idyn_flow
        params                      = idyn_mcod
*       FIELDTEXTS                  =
     exceptions
        cancelled                   = 1
        not_found                   = 2
        permission_error            = 3
        others                      = 4.

    check sy-subrc = 0.

* Add heading information for screen.
    setattributesfromstructure(
                     node = dynnr_node structure =  xdyn_head  ).
* Add the dynpro text also.
    xdyn_text_string =  xdyn_text.
    rc = dynnr_node->set_attribute(
               name = 'DTEXT'  value = xdyn_text_string ).
    rc = dynp_node->append_child( dynnr_node ).

* Add fields information for screen.
    if not idyn_fldl[] is initial.
      loop at idyn_fldl into xdyn_fldl.
        dynprofieldsnode = xmldoc->create_element( 'dynprofield' ).
        setattributesfromstructure(
                 node = dynprofieldsnode structure =  xdyn_fldl ).
        rc = dynnr_node->append_child( dynprofieldsnode ).
      endloop.
    endif.

* Add flow logic of screen
    if not idyn_flow[] is initial.
      clear xflowsource. refresh  iflowsource.
      loop at idyn_flow into xdyn_flow.
        xflowsource  = xdyn_flow.
        append xflowsource to iflowsource.
      endloop.

      dynproflownode = xmldoc->create_element( 'dynproflowsource' ).
      flowsourcestring = buildsourcestring( sourcetable = iflowsource ).
      rc = dynproflownode->if_ixml_node~set_value( flowsourcestring ).
      rc = dynnr_node->append_child( dynproflownode  ).
    endif.

* Add matchcode information for screen.
    if not idyn_mcod[] is initial.
      loop at idyn_mcod into xdyn_mcod.
        check not xdyn_mcod-type is initial
          and not xdyn_mcod-content is initial.
        dynpromatchnode = xmldoc->create_element( 'dynpromatchcode' ).
        setattributesfromstructure(
                 node = dynpromatchnode structure =  xdyn_mcod ).
        rc = dynnr_node->append_child( dynpromatchnode ).
      endloop.
    endif.

  endloop.

endmethod.


method GET_FM_DOCUMENTATION.

  DATA languagenode   TYPE REF TO if_ixml_element.
  DATA txtlines_node TYPE REF TO if_ixml_element.
  DATA rc            TYPE sysubrc.
  DATA _objtype      TYPE string.

  TYPES: BEGIN OF t_dokhl,
          id          TYPE dokhl-id,
          object      TYPE dokhl-object,
          langu       TYPE dokhl-langu,
          typ         TYPE dokhl-typ,
          dokversion  TYPE dokhl-dokversion,
         END OF t_dokhl.

  DATA lt_dokhl TYPE TABLE OF t_dokhl.
  DATA ls_dokhl LIKE LINE OF lt_dokhl.

  DATA lt_lines TYPE TABLE OF tline.
  DATA ls_lines LIKE LINE OF lt_lines.

  DATA lv_str TYPE string.
  DATA _objname TYPE e071-obj_name.

  _objname = fm_name.

* Check against database
  SELECT  id object langu typ dokversion
        INTO CORRESPONDING FIELDS OF TABLE lt_dokhl
           FROM dokhl
             WHERE id = 'FU'
                AND object = _objname.

* Use only most recent version.
  SORT lt_dokhl BY id object langu typ ASCENDING dokversion DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_dokhl COMPARING id object typ langu.

  docnode = xmldoc->create_element( 'functionModuleDocumentation' ).

* Make sure there is at least one record here.
  CLEAR ls_dokhl.
  READ TABLE lt_dokhl INTO ls_dokhl INDEX 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Set docNode object attribute
  lv_str = ls_dokhl-object.
  rc = docnode->set_attribute( name = 'OBJECT' value = lv_str ).

  LOOP AT lt_dokhl INTO ls_dokhl.

* Create language node, and set attribute
    languagenode = xmldoc->create_element( 'language' ).
    lv_str = ls_dokhl-langu.
    rc = languagenode->set_attribute( name = 'SPRAS' value = lv_str ).

* Read the documentation text
    CALL FUNCTION 'DOCU_READ'
      EXPORTING
        id      = ls_dokhl-id
        langu   = ls_dokhl-langu
        object  = ls_dokhl-object
        typ     = ls_dokhl-typ
        version = ls_dokhl-dokversion
      TABLES
        line    = lt_lines.

* Write records to XML node
    LOOP AT lt_lines INTO ls_lines.
      txtlines_node = xmldoc->create_element( `textLine` ).
      me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
      rc = languagenode->append_child( txtlines_node ).
    ENDLOOP.
    rc = docnode->append_child( languagenode ) .
  ENDLOOP.

endmethod.


method GET_FUNCTION_MODULES.
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

  TYPES: BEGIN OF tfunct_head,
         name   TYPE rs38l-name,
         global TYPE rs38l-global,
         remote TYPE rs38l-remote,
         utask  TYPE rs38l-utask,
         stext  TYPE tftit-stext,
         area   TYPE rs38l-area,
         END OF tfunct_head.

  DATA xfunct_head TYPE tfunct_head.
  DATA iimport     TYPE TABLE OF rsimp.
  DATA ichange     TYPE TABLE OF rscha.
  DATA iexport     TYPE TABLE OF rsexp.
  DATA itables     TYPE TABLE OF rstbl.
  DATA iexcepl     TYPE TABLE OF rsexc.
  DATA idocume     TYPE TABLE OF rsfdo.
  DATA isource     TYPE TABLE OF rssource.
  DATA isource_new TYPE rsfb_source .

  DATA ximport     TYPE  rsimp.
  DATA xchange     TYPE  rscha.
  DATA xexport     TYPE  rsexp.
  DATA xtables     TYPE  rstbl.
  DATA xexcepl     TYPE  rsexc.
  DATA xdocume     TYPE  rsfdo.
  DATA xsource     TYPE  rssource.
  DATA xsource_new LIKE LINE OF isource_new.

  DATA functionmodulesnode TYPE REF TO if_ixml_element.
  DATA functionmodulenode  TYPE REF TO if_ixml_element.
  DATA importsnode TYPE REF TO if_ixml_element.
  DATA changesnode TYPE REF TO if_ixml_element.
  DATA exportsnode TYPE REF TO if_ixml_element.
  DATA tablesnode  TYPE REF TO if_ixml_element.
  DATA exceplnode  TYPE REF TO if_ixml_element.
  DATA documsnode  TYPE REF TO if_ixml_element.
  DATA fmsrcenode  TYPE REF TO if_ixml_element.
  DATA fmsrcenewnode  TYPE REF TO if_ixml_element.
  DATA fmdocumenation TYPE REF TO if_ixml_element.
  DATA fmparmdocumenation TYPE REF TO if_ixml_element.

  DATA functiongroupname TYPE  tlibg-area.

  DATA ifunct TYPE TABLE OF  rs38l_incl.
  DATA xfunct TYPE  rs38l_incl.

  DATA rc           TYPE sysubrc.
  DATA progattribs  TYPE trdir.
  DATA progsource   TYPE rswsourcet.
  DATA _objname(30) TYPE c.
  DATA sourcestring TYPE string.
  DATA function_deleted    TYPE c.
  DATA endfunction_deleted TYPE c.
  DATA lv_len TYPE i.

  functiongroupname = fct_group.

* Now get the function pool contents
  CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
    EXPORTING
      function_pool           = functiongroupname
    TABLES
      functab                 = ifunct
    EXCEPTIONS
      function_pool_not_found = 1
      OTHERS                  = 2.

* Now write out function modules data.
  functionmodulesnode = xmldoc->create_element( 'functionmodules' ).

  LOOP AT ifunct INTO xfunct.

    functionmodulenode = xmldoc->create_element( 'functionmodule' ).
    xfunct_head-name =  xfunct-funcname.

    REFRESH: iimport, ichange, iexport,
             itables, iexcepl, idocume, isource, isource_new.

* Read the function module data
    CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
      EXPORTING
        functionname       = xfunct_head-name
      IMPORTING
        global_flag        = xfunct_head-global
        remote_call        = xfunct_head-remote
        update_task        = xfunct_head-utask
        short_text         = xfunct_head-stext
*       FUNCTION_POOL      =
      TABLES
        import_parameter   = iimport
        changing_parameter = ichange
        export_parameter   = iexport
        tables_parameter   = itables
        exception_list     = iexcepl
        documentation      = idocume
        source             = isource
      CHANGING
        new_source         = isource_new
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3
        OTHERS             = 4.

* Set the header attributes
    setattributesfromstructure(
               node = functionmodulenode
               structure =  xfunct_head  ).

* IMports
    IF NOT iimport[] IS INITIAL.
      LOOP AT iimport INTO ximport.
        importsnode = xmldoc->create_element( 'importing' ).
        setattributesfromstructure(
                 node = importsnode structure =  ximport ).
        rc = functionmodulenode->append_child( importsnode ).
      ENDLOOP.
    ENDIF.

* Exports
    IF NOT iexport[] IS INITIAL.
      LOOP AT iexport INTO xexport.
        exportsnode = xmldoc->create_element( 'exporting' ).
        setattributesfromstructure(
                 node = exportsnode structure =  xexport ).
        rc = functionmodulenode->append_child( exportsnode ).
      ENDLOOP.
    ENDIF.

* Changing
    IF NOT ichange[] IS INITIAL.
      LOOP AT ichange INTO xchange.
        changesnode = xmldoc->create_element( 'changing' ).
        setattributesfromstructure(
                 node = changesnode structure =  xchange ).
        rc = functionmodulenode->append_child( changesnode ).
      ENDLOOP.
    ENDIF.

* Tables
    IF NOT itables[] IS INITIAL.
      LOOP AT itables INTO xtables.
        tablesnode = xmldoc->create_element( 'tables' ).
        setattributesfromstructure(
                 node = tablesnode structure =  xtables ).
        rc = functionmodulenode->append_child( tablesnode ).
      ENDLOOP.
    ENDIF.

* Exception list
    IF NOT iexcepl[] IS INITIAL.
      LOOP AT iexcepl INTO xexcepl.
        exceplnode = xmldoc->create_element( 'exceptions' ).
        setattributesfromstructure(
                 node = exceplnode structure =  xexcepl ).
        rc = functionmodulenode->append_child( exceplnode ).
      ENDLOOP.
    ENDIF.

* Documentation - this is short text
    IF NOT idocume[] IS INITIAL.
      LOOP AT idocume INTO xdocume .
        documsnode = xmldoc->create_element( 'documentation' ).
        setattributesfromstructure(
                 node = documsnode structure =  xdocume  ).

        rc = functionmodulenode->append_child( documsnode ).
      ENDLOOP.
    ENDIF.

* Source code for function module
    IF NOT isource[] IS INITIAL.

* Get rid of the FUNCTION and ENDFUNCTION statements.
* And the signature comments
* All of this will be inserted automatically, when imported.
      CLEAR: function_deleted, endfunction_deleted.
      LOOP AT isource INTO xsource.
        IF xsource+0(2) = '*"'.
          DELETE isource INDEX sy-tabix.
          CONTINUE.
        ENDIF.
        SEARCH xsource FOR 'FUNCTION'.
        "Got it and not a comment?
        IF sy-subrc  = 0 AND xsource+0(1) <> '*' AND
           function_deleted NE 'X'.
          DELETE isource INDEX sy-tabix.
          function_deleted = 'X'.
          CONTINUE.
        ENDIF.
        SEARCH xsource FOR 'ENDFUNCTION'.
        IF sy-subrc  = 0.
          DELETE isource INDEX sy-tabix.
          endfunction_deleted = 'X'.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      fmsrcenode = xmldoc->create_element( 'fm_source' ).
      REFRESH progsource.
      LOOP AT isource INTO xsource.
        APPEND xsource TO progsource.
      ENDLOOP.
      sourcestring = buildsourcestring( sourcetable = progsource ).
      rc = fmsrcenode->if_ixml_node~set_value( sourcestring ).
      rc = functionmodulenode->append_child( fmsrcenode ).

    ENDIF.

* Source code for function module
    IF NOT isource_new[] IS INITIAL.

* Get rid of the FUNCTION and ENDFUNCTION statements.
* And the signature comments
* All of this will be inserted automatically, when imported.
      CLEAR: function_deleted, endfunction_deleted.
      LOOP AT isource_new INTO xsource_new.
        CHECK xsource_new IS NOT INITIAL.
        CLEAR lv_len.
        lv_len = strlen( xsource_new ).
        IF lv_len GE 2.
          IF xsource_new+0(2) = '*"'.
            DELETE isource_new INDEX sy-tabix.
            CONTINUE.
          ENDIF.
        ENDIF.
        SEARCH xsource_new FOR 'FUNCTION'.
        "Got it and not a comment?
        IF sy-subrc  = 0 AND xsource_new+0(1) <> '*' AND
           function_deleted NE 'X'.
          DELETE isource_new INDEX sy-tabix.
          function_deleted = 'X'.
          CONTINUE.
        ENDIF.
        SEARCH xsource_new FOR 'ENDFUNCTION'.
        IF sy-subrc  = 0.
          DELETE isource_new INDEX sy-tabix.
          endfunction_deleted = 'X'.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      fmsrcenewnode = xmldoc->create_element( 'fm_source_new' ).
      REFRESH progsource.
      LOOP AT isource_new INTO xsource_new.
        APPEND xsource_new TO progsource.
      ENDLOOP.
      sourcestring = buildsourcestring( sourcetable = progsource ).
      rc = fmsrcenewnode->if_ixml_node~set_value( sourcestring ).
      rc = functionmodulenode->append_child( fmsrcenewnode ).

    ENDIF.

* Get function module documentation
    fmdocumenation = get_fm_documentation( xfunct-funcname ).
    rc = functionmodulenode->append_child( fmdocumenation ).

* Add to functionmodules node
    rc = functionmodulesnode->append_child( functionmodulenode ).

  ENDLOOP.


  fm_node = functionmodulesnode.

endmethod.


method GET_INCLUDES.
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

  types: begin of tinclude,
         name(40),
         end of tinclude.

  data iinclude type table of tinclude.
  data xinclude type tinclude.

  data ifunct type table of  rs38l_incl.
  data xfunct type  rs38l_incl.

  data functiongroupname type  tlibg-area.
  data mainfgprogname    type sy-repid.

  data includenode  type ref to if_ixml_element.
  data includesnode type ref to if_ixml_element.
  data includesourcenode type ref to if_ixml_element.

  data progattribs  type trdir.
  data rc           type sysubrc.
  data progsource   type rswsourcet.
  data _objname(30) type c.
  data sourcestring type string.

  functiongroupname = fct_group.
  mainfgprogname    = main_prog.

  CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
    EXPORTING
      function_pool           = functiongroupname
    TABLES
      functab                 = ifunct
    EXCEPTIONS
      function_pool_not_found = 1
      others                  = 2.

* Get all includes
  CALL FUNCTION 'RS_GET_ALL_INCLUDES'
    EXPORTING
      program      = mainfgprogname
    TABLES
      includetab   = iinclude
    EXCEPTIONS
      not_existent = 1
      no_program   = 2
      others       = 3.

* Get rid of any includes that are for the function modules
* and any includes that are in SAP namespace
  loop at iinclude into xinclude.
    read table ifunct
             into xfunct
                   with key include = xinclude-name.
    if sy-subrc  = 0.
      delete iinclude where name = xinclude-name.
      continue.
    endif.
    select single * from trdir
            into progattribs
                   where name = xinclude-name.
    if progattribs-cnam = 'SAP'.
      delete iinclude where name = xinclude-name.
      continue.
    endif.
    if xinclude-name(2) <> 'LZ'
       and xinclude-name(2) <> 'LY'
       and xinclude-name(1) <> 'Z'
       and xinclude-name(1) <> 'Y'.
      delete iinclude where name = xinclude-name.
      continue.
    endif.
  endloop.

* Write out include programs.....
  includesnode = xmldoc->create_element( 'includeprograms' ).

  loop at iinclude into xinclude.

    includenode = xmldoc->create_element( 'include' ).
    select single * from trdir
            into progattribs
                   where name = xinclude-name.
    setattributesfromstructure(
               node = includenode
               structure =  progattribs  ).

    includesourcenode = xmldoc->create_element( 'include_source' ).
    read report xinclude-name into progsource.
    sourcestring = buildsourcestring( sourcetable = progsource ).
    rc = includesourcenode->if_ixml_node~set_value( sourcestring ).
    rc = includenode->append_child( includesourcenode ).
    rc = includesnode->append_child( includenode ).

  endloop.

  incl_node = includesnode.

endmethod.


method GET_PFSTATUS.
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

  data: ista type table of rsmpe_stat,
        ifun type table of rsmpe_funt,
        imen type table of rsmpe_men,
        imtx type table of rsmpe_mnlt,
        iact type table of rsmpe_act,
        ibut type table of rsmpe_but,
        ipfk type table of rsmpe_pfk,
        iset type table of rsmpe_staf,
        idoc type table of rsmpe_atrt,
        itit type table of rsmpe_titt,
        ibiv type table of rsmpe_buts.

  data: xsta type rsmpe_stat,
        xfun type rsmpe_funt,
        xmen type rsmpe_men,
        xmtx type rsmpe_mnlt,
        xact type rsmpe_act,
        xbut type rsmpe_but,
        xpfk type rsmpe_pfk,
        xset type rsmpe_staf,
        xdoc type rsmpe_atrt,
        xtit type rsmpe_titt,
        xbiv type rsmpe_buts.

  data sta_node type ref to if_ixml_element.
  data fun_node type ref to if_ixml_element.
  data men_node type ref to if_ixml_element.
  data mtx_node type ref to if_ixml_element.
  data act_node type ref to if_ixml_element.
  data but_node type ref to if_ixml_element.
  data pfk_node type ref to if_ixml_element.
  data set_node type ref to if_ixml_element.
  data doc_node type ref to if_ixml_element.
  data tit_node type ref to if_ixml_element.
  data biv_node type ref to if_ixml_element.

  data _objname type trobj_name.
  data _program type  trdir-name.
  data rc type sy-subrc.

  _objname = objname.
  _program = objname.

  call function 'RS_CUA_INTERNAL_FETCH'
    exporting
      program         = _program
      language        = sy-langu
    tables
      sta             = ista
      fun             = ifun
      men             = imen
      mtx             = imtx
      act             = iact
      but             = ibut
      pfk             = ipfk
      set             = iset
      doc             = idoc
      tit             = itit
      biv             = ibiv
    exceptions
      not_found       = 1
      unknown_version = 2
      others          = 3.

  check sy-subrc = 0.

* if there is a gui status or gui title present, then
* create pfstatus node.
  if ista[] is not initial
     or itit[] is not initial.
    pfstat_node = xmldoc->create_element( 'pfstatus' ).
  endif.


* if ista is filled, assume there are one or more
* gui statuses
  if ista[] is not initial.

    loop at ista into xsta.
      sta_node = xmldoc->create_element( 'pfstatus_sta' ).
      setattributesfromstructure(
               node = sta_node
               structure =  xsta ).
      rc = pfstat_node->append_child( sta_node ).
    endloop.

    loop at ifun into xfun.
      fun_node = xmldoc->create_element( 'pfstatus_fun' ).
      setattributesfromstructure(
               node = fun_node
               structure =  xfun ).
      rc = pfstat_node->append_child( fun_node ).
    endloop.

    loop at imen into xmen.
      men_node = xmldoc->create_element( 'pfstatus_men' ).
      setattributesfromstructure(
               node = men_node
               structure =  xmen ).
      rc = pfstat_node->append_child( men_node ).
    endloop.

    loop at imtx into xmtx.
      mtx_node = xmldoc->create_element( 'pfstatus_mtx' ).
      setattributesfromstructure(
               node = mtx_node
               structure =  xmtx ).
      rc = pfstat_node->append_child( mtx_node ).
    endloop.

    loop at iact into xact.
      act_node = xmldoc->create_element( 'pfstatus_act' ).
      setattributesfromstructure(
               node = act_node
               structure =  xact ).
      rc = pfstat_node->append_child( act_node ).
    endloop.

    loop at ibut into xbut.
      but_node = xmldoc->create_element( 'pfstatus_but' ).
      setattributesfromstructure(
               node = but_node
               structure =  xbut ).
      rc = pfstat_node->append_child( but_node ).
    endloop.

    loop at ipfk into xpfk.
      pfk_node = xmldoc->create_element( 'pfstatus_pfk' ).
      setattributesfromstructure(
               node = pfk_node
               structure =  xpfk ).
      rc = pfstat_node->append_child( pfk_node ).
    endloop.

    loop at iset into xset.
      set_node = xmldoc->create_element( 'pfstatus_set' ).
      setattributesfromstructure(
               node = set_node
               structure =  xset ).
      rc = pfstat_node->append_child( set_node ).
    endloop.

    loop at idoc into xdoc.
      doc_node = xmldoc->create_element( 'pfstatus_doc' ).
      setattributesfromstructure(
               node = doc_node
               structure =  xdoc ).
      rc = pfstat_node->append_child( doc_node ).
    endloop.


    loop at ibiv into xbiv.
      biv_node = xmldoc->create_element( 'pfstatus_biv' ).
      setattributesfromstructure(
               node = biv_node
               structure =  xbiv ).
      rc = pfstat_node->append_child( biv_node ).
    endloop.

  endif.


* It itit is filled, assume one or more titles
  if itit[] is not initial.

    loop at itit into xtit.
      tit_node = xmldoc->create_element( 'pfstatus_tit' ).
      setattributesfromstructure(
               node = tit_node
               structure =  xtit ).
      rc = pfstat_node->append_child( tit_node ).
    endloop.

  endif.

endmethod.


method GET_TEXTPOOL.
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

data aText type ref to if_ixml_element.
data textPoolTable type standard table of TEXTPOOL.
data textPoolRow type textPool.
data languageList type instLang.
data aLanguage type SPRAS.
data _objName(30) type c.
data rc type i.
data sTemp type string.
data languageNode type ref to if_ixml_element.

  _objName = objName.


  textNode = xmlDoc->create_element( 'textPool' ).

  CALL FUNCTION 'RS_TEXTLOG_GET_PARAMETERS'
        changing
          INSTALLED_LANGUAGES = languageList.

  loop at languageList into aLanguage.
    read textpool _objName into textPoolTable language aLanguage.
    if sy-subrc = 0.
      languageNode = xmlDoc->create_Element( 'language' ).
      sTemp = aLanguage.
      rc = languageNode->set_attribute( name = 'SPRAS' value = sTemp ).
      loop at textPoolTable into textPoolRow.
        aText = xmlDoc->create_element( 'textElement' ).
        setAttributesFromStructure( node = aText structure =
        textPoolRow ).
        rc = languageNode->append_child( aText ).
      endloop.
      rc = textNode->append_child( languageNode ).
    endif.
  endloop.

endmethod.


method TRANSPORT_COPY.
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


  CALL FUNCTION 'RS_CORR_INSERT'
       EXPORTING
            AUTHOR              = author
            GLOBAL_LOCK         = 'X'
            OBJECT              = objName
            OBJECT_CLASS        = 'ABAP'
            DEVCLASS            = devClass
*            KORRNUM             = CORRNUMBER_LOCAL
            MASTER_LANGUAGE     = sy-langu
*            PROGRAM             = PROGRAM_LOCAL
            MODE                = 'INSERT'
*       IMPORTING
*            AUTHOR              = UNAME
*            KORRNUM             = CORRNUMBER_LOCAL
*            DEVCLASS            = DEVCLASS_LOCAL
       EXCEPTIONS
            CANCELLED           = 1
            PERMISSION_FAILURE  = 2
            UNKNOWN_OBJECTCLASS = 3.

  if sy-subrc <> 0.
    case sy-subrc.
      when 2.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>not_authorized.
      when others.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>system_error.
    endcase.
  endif.

endmethod.
endclass. "ZSAPLINK_FUNCTIONGROUP implementation