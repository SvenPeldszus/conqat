/*-----------------------------------------------------------------------+
 | eu.cqse.conqat.engine.abap
 |                                                                       |
   $Id: EAbapObjectType.java 49262 2014-05-13 15:53:37Z heinemann $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.abap;

/**
 * Type of ABAP source code object used by ConQAT.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49262 $
 * @ConQAT.Rating GREEN Hash: 699687033073E27646A1B9CF379D090E
 */
public enum EAbapObjectType {
	/** Object type of programs */
	PROG,
	/** Object type of classes */
	CLAS,
	/** Object type of interfaces */
	INTF,
	/** Object type of function groups */
	FUGR;
}
