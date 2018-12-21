*&---------------------------------------------------------------------*
*& Report ZSAPLINK_ADT_ENH_INSTALLER
*& SAPlink ADT Enhancement Implementation Installer
*&---------------------------------------------------------------------*
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink for ABAP in Eclipse.                 |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

REPORT  zsaplink_adt_enh_installer.

TYPE-POOLS: abap.

" For testing without AiE we set this variable
DATA: lv_nugget_path(300) TYPE c VALUE 'C:\Projects\SAPlinkADT\trunk\org.saplink.install\files\SAPlinkADT-Enhancement.nugg'.

" When we run in AiE then the placeholder was replaced
IF cl_adt_gui_event_dispatcher=>is_adt_environment( ) = abap_true.
  lv_nugget_path = 'C:\Users\ladmin\.eclipse\org.eclipse.platform_3.7.0_118372976\plugins\org.saplink.install_1.0.27\files\SAPlinkADT-Enhancement.nugg'.
ENDIF.

" Make sure that SAPlink is executed in the Nugget tab
EXPORT abap_false TO MEMORY ID 'ISSLNK'.

" Export result to memory to avoid an additional screen the user must close manually
SUBMIT zsaplink
  WITH nugfile = lv_nugget_path
  WITH import  = abap_true
  EXPORTING LIST TO MEMORY
  AND RETURN.

" Trigger the Nugget
IF cl_adt_gui_event_dispatcher=>is_adt_environment( ) = abap_true.
  cl_adt_gui_event_dispatcher=>send_test_event(
    EXPORTING
      value            = 'org.saplink.saplinkadt.enhancement.installation.finished'
  ).
ENDIF.