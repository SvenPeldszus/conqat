class ZSAPLINK_NUGGET definition
  public
  final
  create public .

public section.

  methods ADDOBJECTTONUGGET
    importing
      !OBJNAME type STRING optional
      !OBJTYPE type STRING optional
      !XMLDOCUMENT type ref to IF_IXML_DOCUMENT optional
    raising
      ZCX_SAPLINK .
  methods CREATEIXMLDOCFROMNUGGET
    returning
      value(IXMLDOCUMENT) type ref to IF_IXML_DOCUMENT .
  methods GETNEXTOBJECT
    returning
      value(IXMLDOCUMENT) type ref to IF_IXML_DOCUMENT .
  methods RESET .
  class-methods CREATEEMPTYXML
    importing
      !NUGGETNAME type STRING
    returning
      value(IXMLDOCUMENT) type ref to IF_IXML_DOCUMENT .
  class-methods GETNUGGETINFO
    importing
      !IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
    returning
      value(NAME) type STRING .
  methods CONSTRUCTOR
    importing
      !NAME type STRING optional
      !IXMLDOCUMENT type ref to IF_IXML_DOCUMENT optional .
  methods CHECKOBJECTEXISTS
    importing
      !OBJNAME type STRING
      !OBJTYPE type STRING
    returning
      value(RETVAL) type SY-SUBRC .
  methods DELETEOBJECTFROMNUGGET
    importing
      !OBJNAME type STRING
      !OBJTYPE type STRING
    returning
      value(RETVAL) type SY-SUBRC .
protected section.

  data IXML type ref to IF_IXML .
  data XMLDOC type ref to IF_IXML_DOCUMENT .
private section.

  data ITERATOR type ref to IF_IXML_NODE_ITERATOR .
  data NUGGNAME type STRING .
  data STREAMFACTORY type ref to IF_IXML_STREAM_FACTORY .
  data XMLDATA type STRING .
  data WT_OBJECTS type TT_OBJECTS .
  data W_INDEX type SY-TABIX .
endclass. "ZSAPLINK_NUGGET definition



*----------------------------------------------------------------------*
*       class ZSAPLINK_NUGGET implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZSAPLINK_NUGGET implementation.


method ADDOBJECTTONUGGET.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
types: begin of t_objectTable,
         classname type string,
         object type ko100-object,
         text type ko100-text,
       end of t_objectTable.
data rootNode type ref to if_ixml_element.
data saplink type ref to zsaplink.
data objectTable type table of t_objectTable.
data objectLine type t_objectTable.
data ixmlDocument type ref to if_ixml_document.
data rval type i.
data objElement type ref to if_ixml_element.

  rootNode = xmlDoc->GET_ROOT_ELEMENT( ).

  if xmlDocument is initial.
    call method zsaplink=>getplugins( changing objectTable = objectTable ).

    read table objectTable into objectLine with key object = ObjType.
    if sy-subrc <> 0.
      raise exception type zcx_saplink
            exporting textid = zcx_saplink=>no_plugin.
    endif.
    create object saplink type (objectLine-classname) exporting name =
    objName.
    ixmlDocument = saplink->CREATEIXMLDOCFROMOBJECT( ).
  else.
    ixmlDocument = xmlDocument.
  endif.
  objElement = ixmlDocument->get_root_element( ).
  rval = rootNode->append_Child( objElement ).

endmethod.


method CHECKOBJECTEXISTS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data: docFilter   type ref to if_ixml_node_filter,
        docIterator type ref to if_ixml_node_iterator,
        currentNode type ref to if_ixml_node,
        rootAttr    type ref to IF_IXML_NAMED_NODE_MAP,
        AttrNode    type ref to IF_IXML_NODE,

        nodeName    type string,
        existsFlag  type flag.

* create a filter to traverse the nugget by object type like CLAS or PROG
  docFilter = xmlDoc->create_filter_name_ns( objType ).
* apply the filter to the iterator
  docIterator = xmlDoc->create_iterator_filtered( docFilter ).
* get the first object of that type in the nugget
  currentNode = docIterator->get_next( ).

*  if this node is not blank proceed to check the attributes
  while currentNode is not initial.
* get object name
    rootAttr = currentNode->GET_ATTRIBUTES( ).
    AttrNode = rootAttr->GET_ITEM( 0 ).
    nodeName = AttrNode->GET_VALUE( ).
*   if the name of the node is the same as the passed parameter, set the flag
    if nodeName = objname.
      existsFlag = 'X'.
    endif.
    currentNode = docIterator->get_next( ).
  endwhile.
  if existsFlag = 'X'.
    retval = 0.
  else.
    retval = 4.
  endif.
endmethod.


method CONSTRUCTOR.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
data rootNode type ref to if_ixml_element.
data rval type i.
*data xmlDoc type ref to if_ixml_document.

  if name is not initial.
    NuggName = name.
    ixml = cl_ixml=>create( ).
    xmlDoc = ixml->create_document( ).
*  may need this from create empty nugget
*dan this was commented out, any ideas why??  Uncommented for Zake.
    rootNode = xmlDoc->create_element( 'nugget' ).
    rval = rootNode->SET_ATTRIBUTE( name = 'name' value = nuggName ).
    rval = xmlDoc->append_child( rootNode ).

    streamFactory = ixml->CREATE_STREAM_FACTORY( ).
  elseif ixmlDocument is not initial.
    ixml = cl_ixml=>create( ).
    xmlDoc = ixmlDocument.
    rootNode = xmlDoc->get_root_element( ).
    nuggName = rootNode->get_attribute( 'name' ).
    streamFactory = ixml->CREATE_STREAM_FACTORY( ).
  else.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  endif.
endmethod.


method CREATEEMPTYXML.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
data ixml type ref to if_IXML.
data rootNode type ref to if_ixml_element.
data rval type i.
data xmlDoc type ref to if_ixml_document.

  ixml = cl_ixml=>create( ).
  xmlDoc = ixml->create_document( ).
  rootNode = xmlDoc->create_element( 'nugget' ).
  rval = rootNode->SET_ATTRIBUTE( name = 'name' value = nuggetName ).
  rval = xmlDoc->append_child( rootNode ).
  ixmlDocument = xmlDoc.

endmethod.


method CREATEIXMLDOCFROMNUGGET.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  ixmlDocument = xmlDoc.
endmethod.


method DELETEOBJECTFROMNUGGET.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data: docFilter   type ref to if_ixml_node_filter,
        docIterator type ref to if_ixml_node_iterator,
        currentNode type ref to if_ixml_node,
        rootAttr    type ref to IF_IXML_NAMED_NODE_MAP,
        AttrNode    type ref to IF_IXML_NODE,

        nodeName    type string.

* create a filter to traverse the nugget by object type like CLAS or PROG
  docFilter = xmlDoc->create_filter_name_ns( objType ).
* apply the filter to the iterator
  docIterator = xmlDoc->create_iterator_filtered( docFilter ).
* get the first object of that type in the nugget
  currentNode = docIterator->get_next( ).

*  if this node is not blank proceed to check the attributes
  while currentNode is not initial.
* get object name
    rootAttr = currentNode->GET_ATTRIBUTES( ).
    AttrNode = rootAttr->GET_ITEM( 0 ).
    nodeName = AttrNode->GET_VALUE( ).
*   if the name of the node is the same as the passed parameter, delete the node
    if nodeName = objname.
      currentNode->remove_node( ).
      retVal = sy-subrc.
      return.
    endif.
    currentNode = docIterator->get_next( ).
  endwhile.
endmethod.


method GETNEXTOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA anode TYPE REF TO if_ixml_node.
  DATA stemp TYPE string.
  DATA rootnode TYPE REF TO if_ixml_node.
  DATA namefilter TYPE REF TO if_ixml_node_filter.
  DATA parentfilter TYPE REF TO if_ixml_node_filter.
  DATA currentnode TYPE REF TO if_ixml_node.
  DATA newnode TYPE REF TO if_ixml_node.
  DATA: rval TYPE i,
        lo_object TYPE REF TO  zsaplink,
        l_tabix TYPE i,
        ls_objects TYPE ts_objects.

  IF wt_objects IS INITIAL.
    IF iterator IS  INITIAL.
      namefilter = xmldoc->create_filter_name_ns( name = 'nugget' ).
      parentfilter = xmldoc->create_filter_parent( namefilter ).
      iterator = xmldoc->create_iterator_filtered( parentfilter ).
    ENDIF.
    currentnode ?= iterator->get_next( ).

    WHILE currentnode IS NOT INITIAL.
      add 1 to l_tabix.
      ixmldocument = ixml->create_document( ).
      newnode = currentnode->clone( ).
      rval = ixmldocument->append_child( newnode ).
      CLEAR ls_objects.

      zsaplink=>checkobject(
        EXPORTING
          i_ixmldocument = ixmldocument
        IMPORTING
*                   e_objtype      =
*                   e_objname      =
*                   e_pluginexists =
*                   e_objectexists =
           e_targetobject = lo_object
             ).
      IF lo_object IS BOUND.
        ls_objects-nugget_level = lo_object->nugget_level.
      ELSE.
* We will not handle this here
        ls_objects-nugget_level = 0.
      ENDIF.
      ls_objects-sort = l_tabix.
      ls_objects-xmldocument = ixmldocument.
      INSERT ls_objects INTO TABLE wt_objects.
      currentnode ?= iterator->get_next( ).
    ENDWHILE.
  ENDIF.
  ADD 1 TO w_index.
  READ TABLE wt_objects INTO ls_objects INDEX w_index.

  IF sy-subrc = 0.
    ixmldocument = ls_objects-xmldocument.
  ELSE.
    CLEAR ixmldocument.
  ENDIF.

endmethod.


method GETNUGGETINFO.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
data rootNode type ref to if_ixml_element.
    rootNode = ixmlDocument->get_root_element( ).
    name = rootNode->get_attribute( 'name' ).
endmethod.


method RESET.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  if iterator is not initial.
    refresh: wt_objects.
    clear w_index.
    iterator->reset( ).
  endif.
endmethod.
endclass. "ZSAPLINK_NUGGET implementation