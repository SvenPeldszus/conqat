/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 the ConQAT Project                                   |
|                                                                          |
| Licensed under the Apache License, Version 2.0 (the "License");          |
| you may not use this file except in compliance with the License.         |
| You may obtain a copy of the License at                                  |
|                                                                          |
|    http://www.apache.org/licenses/LICENSE-2.0                            |
|                                                                          |
| Unless required by applicable law or agreed to in writing, software      |
| distributed under the License is distributed on an "AS IS" BASIS,        |
| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. |
| See the License for the specific language governing permissions and      |
| limitations under the License.                                           |
+-------------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.shallowparser.languages.delphi;

import static org.conqat.engine.sourcecode.shallowparser.languages.delphi.DelphiShallowParser.EDelphiParserStates.IN_CONST;
import static org.conqat.engine.sourcecode.shallowparser.languages.delphi.DelphiShallowParser.EDelphiParserStates.IN_ENUM;
import static org.conqat.engine.sourcecode.shallowparser.languages.delphi.DelphiShallowParser.EDelphiParserStates.IN_METHOD;
import static org.conqat.engine.sourcecode.shallowparser.languages.delphi.DelphiShallowParser.EDelphiParserStates.IN_METHOD_HEAD;
import static org.conqat.engine.sourcecode.shallowparser.languages.delphi.DelphiShallowParser.EDelphiParserStates.IN_TYPE;
import static org.conqat.engine.sourcecode.shallowparser.languages.delphi.DelphiShallowParser.EDelphiParserStates.IN_TYPE_DECL;
import static org.conqat.engine.sourcecode.shallowparser.languages.delphi.DelphiShallowParser.EDelphiParserStates.IN_VAR;
import static org.conqat.engine.sourcecode.shallowparser.languages.delphi.DelphiShallowParser.EDelphiParserStates.TOP_LEVEL;
import static org.conqat.lib.scanner.ETokenType.ABSTRACT;
import static org.conqat.lib.scanner.ETokenType.BEGIN;
import static org.conqat.lib.scanner.ETokenType.CASE;
import static org.conqat.lib.scanner.ETokenType.CDECL;
import static org.conqat.lib.scanner.ETokenType.CLASS;
import static org.conqat.lib.scanner.ETokenType.COLON;
import static org.conqat.lib.scanner.ETokenType.COMMA;
import static org.conqat.lib.scanner.ETokenType.CONST;
import static org.conqat.lib.scanner.ETokenType.CONSTRUCTOR;
import static org.conqat.lib.scanner.ETokenType.DESTRUCTOR;
import static org.conqat.lib.scanner.ETokenType.DO;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.DOUBLE_DOT;
import static org.conqat.lib.scanner.ETokenType.DYNAMIC;
import static org.conqat.lib.scanner.ETokenType.ELSE;
import static org.conqat.lib.scanner.ETokenType.END;
import static org.conqat.lib.scanner.ETokenType.EQ;
import static org.conqat.lib.scanner.ETokenType.EXTERNAL;
import static org.conqat.lib.scanner.ETokenType.FINALIZATION;
import static org.conqat.lib.scanner.ETokenType.FLOATING_POINT_LITERAL;
import static org.conqat.lib.scanner.ETokenType.FOR;
import static org.conqat.lib.scanner.ETokenType.FORWARD;
import static org.conqat.lib.scanner.ETokenType.FUNCTION;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.IF;
import static org.conqat.lib.scanner.ETokenType.IMPLEMENTATION;
import static org.conqat.lib.scanner.ETokenType.INHERITED;
import static org.conqat.lib.scanner.ETokenType.INITIALIZATION;
import static org.conqat.lib.scanner.ETokenType.INTEGER_LITERAL;
import static org.conqat.lib.scanner.ETokenType.INTERFACE;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.OF;
import static org.conqat.lib.scanner.ETokenType.OVERLOAD;
import static org.conqat.lib.scanner.ETokenType.OVERRIDE;
import static org.conqat.lib.scanner.ETokenType.PACKED;
import static org.conqat.lib.scanner.ETokenType.PASCAL;
import static org.conqat.lib.scanner.ETokenType.PRIVATE;
import static org.conqat.lib.scanner.ETokenType.PROCEDURE;
import static org.conqat.lib.scanner.ETokenType.PROGRAM;
import static org.conqat.lib.scanner.ETokenType.PROTECTED;
import static org.conqat.lib.scanner.ETokenType.PUBLIC;
import static org.conqat.lib.scanner.ETokenType.PUBLISHED;
import static org.conqat.lib.scanner.ETokenType.RECORD;
import static org.conqat.lib.scanner.ETokenType.REGISTER;
import static org.conqat.lib.scanner.ETokenType.REPEAT;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SAFECALL;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;
import static org.conqat.lib.scanner.ETokenType.SET;
import static org.conqat.lib.scanner.ETokenType.STDCALL;
import static org.conqat.lib.scanner.ETokenType.STRICT;
import static org.conqat.lib.scanner.ETokenType.STRING_LITERAL;
import static org.conqat.lib.scanner.ETokenType.THEN;
import static org.conqat.lib.scanner.ETokenType.TYPE;
import static org.conqat.lib.scanner.ETokenType.UNIT;
import static org.conqat.lib.scanner.ETokenType.UNTIL;
import static org.conqat.lib.scanner.ETokenType.USES;
import static org.conqat.lib.scanner.ETokenType.VAR;
import static org.conqat.lib.scanner.ETokenType.VARARGS;
import static org.conqat.lib.scanner.ETokenType.VIRTUAL;
import static org.conqat.lib.scanner.ETokenType.WHILE;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowParserBase;
import org.conqat.engine.sourcecode.shallowparser.languages.delphi.DelphiShallowParser.EDelphiParserStates;
import org.conqat.lib.commons.region.Region;
import org.conqat.lib.scanner.ETokenType;

/**
 * A shallow parser for Delphi.
 * 
 * This parser currently recognizes types (classes, records, enumerations,
 * ranges, sets), procedures, functions and global directives like
 * initialization and finalization sections. Within methods, control structures
 * and simple statements can be parsed. The parser does not parse into the
 * statements. Nested methods are supported.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 51689 $
 * @ConQAT.Rating RED Hash: B5FD29CC1AF0C04D524B20D4850F3619
 */
// TODO (BH): NOte to self: just a shallow review. perform full review in next round
public class DelphiShallowParser extends ShallowParserBase<EDelphiParserStates> {
	
	/** All possible states of the DelphiShallowParser. */
	public static enum EDelphiParserStates {
		
		/** Top-level state. */
		TOP_LEVEL,

		/** Inside a type-section. */
		IN_TYPE,

		/** Inside a type declaration, for example a class or a record. */
		IN_TYPE_DECL,

		/** Inside an enum declaration. */
		IN_ENUM,

		/** Inside a variable-section. */
		IN_VAR,

		/** Inside a const-section. */
		IN_CONST,

		/** Inside a method's head/variable declarations. */
		IN_METHOD_HEAD,

		/** Inside a method's implementation. */
		IN_METHOD
	}

	/** Constructor. */
	public DelphiShallowParser() {
		super(EDelphiParserStates.class, TOP_LEVEL);
		createTopLevelRules();
		createTypeRules();
		createEnumRules();
		createTypeDeclRules();
		createMethodHeadRules();
		createVarRules();
		createConstRules();
		createMethodRules();
	}

	/** Returns a set of all token types that mark a new top-level section. */
	// TODO (BH): Why protected method. Do you expect to override? Why not just constant?
	protected EnumSet<ETokenType> getTopLevelKeywords() {
		return EnumSet.of(PROGRAM, UNIT, INTERFACE, IMPLEMENTATION, TYPE, VAR,
				PROCEDURE, FUNCTION, BEGIN, END, INITIALIZATION, FINALIZATION);
	}

	/**
	 * Returns a set of all token types that mark the beginning of a
	 * method/function/procedure declaration.
	 */
	// TODO (BH): Why protected method. Do you expect to override? Why not just constant?
	protected EnumSet<ETokenType> getMethodKeywords() {
		return EnumSet.of(FUNCTION, PROCEDURE, CONSTRUCTOR, DESTRUCTOR);
	}

	/**
	 * Returns a set of all token types that can be used as modifiers for
	 * methods/functions/procedures.
	 */
	// TODO (BH): Why protected method. Do you expect to override? Why not just constant?
	protected EnumSet<ETokenType> getMethodModifiers() {
		return EnumSet
				.of(ABSTRACT, CDECL, DYNAMIC, FORWARD, OVERLOAD, OVERRIDE,
						PASCAL, REGISTER, SAFECALL, STDCALL, VARARGS, VIRTUAL);
	}

	/** Returns a set of all token types that can be used as access modifiers. */
	// TODO (BH): Why protected method. Do you expect to override? Why not just constant?
	protected EnumSet<ETokenType> getAccessModifiers() {
		return EnumSet.of(PUBLIC, PROTECTED, PRIVATE, PUBLISHED);
	}

	/** Returns a set of all token types that can be used as case literals. */
	// TODO (BH): Why protected method. Do you expect to override? Why not just constant?
	protected EnumSet<ETokenType> getCaseLiterals() {
		return EnumSet.of(IDENTIFIER, INTEGER_LITERAL, FLOATING_POINT_LITERAL);
	}

	// TODO (BH): I think almost all of the following methods can be private
	
	/** Create rules for all top-level structures. */
	protected void createTopLevelRules() {
		// program or unit declaration
		inState(TOP_LEVEL).sequence(EnumSet.of(PROGRAM, UNIT))
				.skipTo(SEMICOLON).createNode(EShallowEntityType.META, 0, 1)
				.endNode();

		// uses declaration
		inState(TOP_LEVEL).sequence(USES).skipTo(SEMICOLON)
				.createNode(EShallowEntityType.META, 0).endNode();

		// interface or implementation declaration
		inState(TOP_LEVEL).sequence(EnumSet.of(INTERFACE, IMPLEMENTATION))
				.createNode(EShallowEntityType.META, 0).endNode();

		// list of type declarations
		inState(TOP_LEVEL).sequence(TYPE).parseUntil(IN_TYPE)
				.sequenceBefore(getTopLevelKeywords());

		// variable declaration
		inState(TOP_LEVEL).sequence(VAR).parseUntil(IN_VAR)
				.sequenceBefore(getTopLevelKeywords());

		// const declaration
		inState(TOP_LEVEL).sequence(CONST).parseUntil(IN_CONST)
				.sequenceBefore(getTopLevelKeywords());

		// function/procedure implementation (nested functions are processed in
		// IN_METHOD state)
		inState(TOP_LEVEL, IN_METHOD).sequence(getMethodKeywords(), IDENTIFIER)
				.optional(DOT, IDENTIFIER)
				.createNode(EShallowEntityType.METHOD, 0, new Region(1, -1))
				.skipNested(LPAREN, RPAREN).skipTo(SEMICOLON)
				.parseUntil(IN_METHOD_HEAD).sequence(END, SEMICOLON).endNode();

		// global begin
		inState(TOP_LEVEL).sequence(BEGIN)
				.createNode(EShallowEntityType.METHOD, "global-begin")
				.parseUntil(IN_METHOD).sequence(END, SEMICOLON).endNode();

		// initialization/finalization
		inState(TOP_LEVEL).sequence(EnumSet.of(INITIALIZATION, FINALIZATION))
				.createNode(EShallowEntityType.METHOD, 0).parseUntil(IN_METHOD)
				.sequenceBefore(getTopLevelKeywords()).endNode();
	}

	/**
	 * Create rules for parsing types like classes, records, enumerations,
	 * function pointers, ranges, sets and meta-classes.
	 */
	protected void createTypeRules() {
		RecognizerBase<EDelphiParserStates> typeAlternative = inState(IN_TYPE)
				.sequence(IDENTIFIER).skipTo(EQ);

		// function pointer declaration
		typeAlternative.sequence(EnumSet.of(PROCEDURE, FUNCTION))
				.createNode(EShallowEntityType.TYPE, "procedure", 0)
				.skipNested(LPAREN, RPAREN).skipTo(SEMICOLON).endNode();

		// enum declaration
		typeAlternative.sequence(LPAREN)
				.createNode(EShallowEntityType.TYPE, "enum", 0)
				.parseUntil(IN_ENUM).sequence(RPAREN, SEMICOLON).endNode();

		// range declaration
		EnumSet<ETokenType> rangeTypes = EnumSet.of(INTEGER_LITERAL,
				STRING_LITERAL);
		typeAlternative.sequence(rangeTypes, DOUBLE_DOT, rangeTypes, SEMICOLON)
				.createNode(EShallowEntityType.TYPE, "range", 0).endNode();

		// set declaration
		typeAlternative.sequence(SET, OF)
				.createNode(EShallowEntityType.TYPE, "set", 0)
				.skipTo(SEMICOLON).endNode();

		// meta-class type
		typeAlternative.sequence(CLASS, OF, CLASS, TYPE, SEMICOLON)
				.createNode(EShallowEntityType.TYPE, "meta-class", 0).endNode();

		// class declaration with inheritance
		typeAlternative.sequence(CLASS, LPAREN, IDENTIFIER, RPAREN)
				.createNode(EShallowEntityType.TYPE, "class", 0)
				.parseUntil(IN_TYPE_DECL).sequence(END, SEMICOLON).endNode();

		// class/record declaration
		typeAlternative.optional(PACKED).sequence(EnumSet.of(CLASS, RECORD))
				.createNode(EShallowEntityType.TYPE, -1, 0)
				.parseUntil(IN_TYPE_DECL).sequence(END, SEMICOLON).endNode();

	}

	/** Create rules for parsing enumerations. */
	protected void createEnumRules() {
		// beginning enum-literal
		inState(IN_ENUM).sequence(IDENTIFIER).optional(EQ, INTEGER_LITERAL)
				.createNode(EShallowEntityType.ATTRIBUTE, "enum-literal", 0)
				.endNode();

		// enum-literal separated with comma
		inState(IN_ENUM).sequence(COMMA, IDENTIFIER)
				.optional(EQ, INTEGER_LITERAL)
				.createNode(EShallowEntityType.ATTRIBUTE, "enum-literal", 1)
				.endNode();
	}

	/** Create rules for parsing classes and records. */
	protected void createTypeDeclRules() {
		// access modifiers
		inState(IN_TYPE_DECL).optional(STRICT).sequence(getAccessModifiers())
				.createNode(EShallowEntityType.META, new Region(0, -1))
				.endNode();

		// function/procedure declaration
		RecognizerBase<EDelphiParserStates> methodBase = inState(IN_TYPE_DECL)
				.sequence(getMethodKeywords(), IDENTIFIER)
				.createNode(EShallowEntityType.METHOD, 0, 1)
				.skipNested(LPAREN, RPAREN).skipTo(SEMICOLON);
		appendMethodModifierRules(methodBase);

		// attribute declaration
		inState(IN_TYPE_DECL).sequence(IDENTIFIER).skipTo(SEMICOLON)
				.createNode(EShallowEntityType.ATTRIBUTE, "attribute", 0)
				.endNode();
	}

	/**
	 * Create rules for parsing the head of methods/procedures/functions. The
	 * head of methods/procedures/functions can contain variable, constant and
	 * type declarations.
	 */
	protected void createMethodHeadRules() {
		EnumSet<ETokenType> headEnd = EnumSet.of(BEGIN, VAR, CONST, TYPE);
		// variable declaration within method
		inState(IN_METHOD_HEAD).sequence(VAR).parseUntil(IN_VAR)
				.sequenceBefore(headEnd);

		// const declaration within method
		inState(IN_METHOD_HEAD).sequence(CONST).parseUntil(IN_CONST)
				.sequenceBefore(headEnd);

		// type declaration within method
		inState(IN_METHOD_HEAD).sequence(TYPE)
				.parseUntil(EDelphiParserStates.IN_TYPE)
				.sequenceBefore(headEnd);

		// begin of method
		inState(IN_METHOD_HEAD).sequence(BEGIN).parseUntil(IN_METHOD)
				.sequenceBefore(END, SEMICOLON);
	}

	/** Create rules for parsing variable declarations within a var-section. */
	protected void createVarRules() {
		// variable declaration
		inState(IN_VAR)
				.sequence(IDENTIFIER, COLON)
				.skipTo(SEMICOLON)
				.createNode(EShallowEntityType.STATEMENT,
						"variable-declaration", 0).endNode();
	}

	/** Create rules for parsing const declarations within a const-section. */
	protected void createConstRules() {
		// const declaration
		inState(IN_CONST)
				.sequence(IDENTIFIER)
				.optional(COLON)
				.skipTo(EQ)
				.skipTo(SEMICOLON)
				.createNode(EShallowEntityType.STATEMENT, "const-declaration",
						0).endNode();
	}

	/** Create rules for parsing the body of a method/procedure/function. */
	protected void createMethodRules() {
		// anonymous block
		inState(IN_METHOD).sequence(BEGIN)
				.createNode(EShallowEntityType.STATEMENT, "anonymous block")
				.parseUntil(IN_METHOD).sequence(END, SEMICOLON).endNode();

		createIfElseRules();
		createLoopRules();
		createCaseRules();
		createSimpleStatementRules();
	}

	/**
	 * Appends rules for parsing method/function/procedure modifiers to a
	 * recognizer.
	 * 
	 * @param recognizer
	 *            the recognizer to append rules to.
	 */
	protected void appendMethodModifierRules(
			RecognizerBase<EDelphiParserStates> recognizer) {
		RecognizerBase<EDelphiParserStates> modifierAlternative = recognizer
				.repeated(getMethodModifiers(), SEMICOLON);
		// parse external directive
		modifierAlternative.sequence(EXTERNAL).skipTo(SEMICOLON).endNode();
		// the last modifier was the end
		modifierAlternative.endNode();
	}

	/** Create rules for parsing if/else structures. */
	protected void createIfElseRules() {
		RecognizerBase<EDelphiParserStates> alternative = inState(IN_METHOD)
				.sequence(IF).createNode(EShallowEntityType.STATEMENT, 0)
				.skipTo(THEN);
		alternative.sequence(BEGIN).parseUntil(IN_METHOD).sequence(END)
				.endNode();
		alternative.parseOnce(IN_METHOD).endNode();

		RecognizerBase<EDelphiParserStates> elseIfAlternative = inState(
				IN_METHOD).sequence(ELSE, IF)
				.createNode(EShallowEntityType.STATEMENT, new int[] { 0, 1 })
				.skipTo(THEN);
		endWithPossibleContinuation(elseIfAlternative.sequence(BEGIN)
				.parseUntil(IN_METHOD).sequence(END), EnumSet.of(ELSE));
		endWithPossibleContinuation(elseIfAlternative.parseOnce(IN_METHOD),
				EnumSet.of(ELSE));

		RecognizerBase<EDelphiParserStates> elseAlternative = inState(IN_METHOD)
				.sequence(ELSE).createNode(EShallowEntityType.STATEMENT, 0);
		elseAlternative.sequence(BEGIN).parseUntil(IN_METHOD)
				.sequence(END, SEMICOLON).endNode();
		elseAlternative.parseOnce(IN_METHOD).endNode();
	}

	/** Create rules for parsing loops. */
	protected void createLoopRules() {
		// repeat until loop
		inState(IN_METHOD).sequence(REPEAT)
				.createNode(EShallowEntityType.STATEMENT, 0)
				.parseUntil(IN_METHOD).sequence(UNTIL).skipTo(SEMICOLON)
				.endNode();

		// while/for loop
		RecognizerBase<EDelphiParserStates> whileForAlternative = inState(
				IN_METHOD).sequence(EnumSet.of(WHILE, FOR))
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DO);
		whileForAlternative.sequence(BEGIN).parseUntil(IN_METHOD)
				.sequence(END, SEMICOLON).endNode();
		whileForAlternative.parseOnce(IN_METHOD).endNode();
	}

	/** Create rules for parsing case structures. */
	protected void createCaseRules() {
		// case statement
		inState(IN_METHOD).sequence(CASE).skipTo(OF)
				.createNode(EShallowEntityType.STATEMENT, 0)
				.parseUntil(IN_METHOD).sequence(END, SEMICOLON).endNode();

		// case label
		EnumSet<ETokenType> validCaseLiterals = getCaseLiterals();
		inState(IN_METHOD).sequence(validCaseLiterals)
				.repeated(COMMA, validCaseLiterals).sequence(COLON)
				.createNode(EShallowEntityType.META, "case-label", 0).endNode();
	}

	/** Create rules for parsing simple statements within methods. */
	protected void createSimpleStatementRules() {
		// simple statement
		inState(IN_METHOD)
				.sequence(EnumSet.of(IDENTIFIER, INHERITED))
				.skipTo(SEMICOLON)
				.createNode(EShallowEntityType.STATEMENT, "simple-statement", 0)
				.endNode();
	}
}