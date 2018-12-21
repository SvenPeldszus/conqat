/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AbapPatterns.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap;

import static org.conqat.lib.scanner.ETokenType.*;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.AbapPatterns;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;

/**
 * Keeps a list of {@link TokenPattern}s used for parsing ABAP statements.
 * 
 * This is in a separate class, since there are quite a lot of patterns.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 3ADEC1C9613CF5D6C30C9F0939803582
 */
/* package */class AbapPatterns {

	/** Matches a field symbol. */
	/* package */static final TokenPattern FIELD_SYMBOL_PATTERN = new TokenPattern()
			.sequence(LT, ETokenClass.IDENTIFIER, GT);

	/** Matches both a field symbol and an identifier. */
	/* package */static final TokenPattern FIELD_SYMBOL_OR_DATA_PATTERN = new TokenPattern()
			.alternative(FIELD_SYMBOL_PATTERN, new TokenPattern()
					.notPrecededBy(LT).sequence(ETokenClass.IDENTIFIER),
					new TokenPattern().sequence(ETokenClass.IDENTIFIER)
							.notFollowedBy(GT));

	/**
	 * Matches a list of parameters of the form <code>a = b c = d e = f</code>
	 * e.g. after a <code>changing</code> declaration in a method invocation.
	 */
	private static final TokenPattern LIST_OF_PARAMETERS_PATTERN = new TokenPattern()
			.alternative(
					// normal assignment syntax
					new TokenPattern().sequence(IDENTIFIER, EQ)
							.sequence(FIELD_SYMBOL_OR_DATA_PATTERN).group(0),
					// weird stuff with variables named like
					// keywords
					new TokenPattern()
							.alternative(IDENTIFIER, ETokenClass.KEYWORD)
							.sequence(EQ)
							.alternative(
									ETokenClass.KEYWORD,
									new TokenPattern().sequence(
											FIELD_SYMBOL_OR_DATA_PATTERN)
											.group(0),
									new TokenPattern().sequence(LT,
											ETokenClass.KEYWORD, GT)));

	/** Matches both a field symbol/variable and a literal. */
	/* package */static final TokenPattern VARIABLE_OR_LITERAL_PATTERN = new TokenPattern()
			.optional(LT)
			.sequence(EnumSet.of(ETokenClass.IDENTIFIER, ETokenClass.LITERAL))
			.optional(GT);

	/**
	 * Matches the common statement endings "assigning a" and
	 * "reference into b", where a and b are assigned "other".
	 */
	/* package */static final TokenPattern ASSIGNING_OR_REFERENCE_INTO_PATTERN = new TokenPattern()
			.skipTo(ASSIGNING, REFERENCE).optional(INTO)
			.sequence(FIELD_SYMBOL_OR_DATA_PATTERN).group(0);

	/**
	 * Matches the importing, receiving and changing parameters of a method
	 * invocation, all of which are parameters that may be changed by the called
	 * method.
	 * 
	 * The matched variable names are in group 0. In certain situations, this
	 * group string may be empty, in which case it should be ignored.
	 */
	/* package */static final TokenPattern METHOD_PARAMETER_WRITES_PATTERN = new TokenPattern()
			.sequence(EnumSet.of(IMPORTING, RECEIVING, CHANGING)).repeated(
					LIST_OF_PARAMETERS_PATTERN);

	/**
	 * The {@link TokenPattern} group which stores variable names that are
	 * assigned OTHER.
	 */
	/* package */static final int OTHER_ASSIGNMENT = 0;

	/**
	 * The {@link TokenPattern} group which stores variable names that are
	 * assigned a variable or literal.
	 */
	/* package */static final int ASSIGNMENT_LEFT = 1;

	/**
	 * The {@link TokenPattern} group which stores variable names or literals
	 * that are assigned to variables in group
	 * {@value AbapPatterns#ASSIGNMENT_LEFT}.
	 */
	/* package */static final int ASSIGNMENT_RIGHT = 2;

	/**
	 * The {@link TokenPattern} group which stores field symbol names that are
	 * assigned <code>null</code>.
	 */
	/* package */static final int NULLED_FIELD_SYMBOL = 3;

	/**
	 * The {@link TokenPattern} group which stores reference variable names that
	 * are assigned <code>null</code>.
	 */
	/* package */static final int NULLED_REFERENCE = 4;

	/**
	 * The {@link TokenPattern} group which stores variable names that are
	 * assigned a new object (i.e. non-null).
	 */
	/* package */static final int NEW_ASSIGNMENT = 5;

	/**
	 * The {@link TokenPattern} group which stores variable names that are
	 * assigned an exception object (i.e. non-null).
	 */
	/* package */static final int EXCEPTION_ASSIGNMENT = 6;

	/**
	 * The patterns that identify variable assignments. All patterns should be
	 * matched in order and the first matching pattern will indicate the
	 * assignment that happened.
	 * 
	 * Each pattern yields a match containing one of the following groups:
	 * <ul>
	 * <li> {@link AbapPatterns#OTHER_ASSIGNMENT}
	 * <li> {@link AbapPatterns#ASSIGNMENT_LEFT}
	 * <li> {@link AbapPatterns#ASSIGNMENT_RIGHT}
	 * <li> {@link AbapPatterns#NULLED_FIELD_SYMBOL}
	 * <li> {@link AbapPatterns#NULLED_REFERENCE}
	 * <li> {@link AbapPatterns#NEW_ASSIGNMENT}
	 * <li> {@link AbapPatterns#EXCEPTION_ASSIGNMENT}
	 * </ul>
	 * 
	 * Note that chain assignments using the "=" operator are not covered by
	 * these patterns.
	 */
	/* package */static final List<TokenPattern> ASSIGNMENT_PATTERNS = new ArrayList<TokenPattern>();
	static {
		ASSIGNMENT_PATTERNS.add(patternStart(CATCH).skipTo(INTO)
				.sequence(ETokenClass.IDENTIFIER).group(6));
		ASSIGNMENT_PATTERNS.add(patternStart(CREATE).skipTo(
				ETokenClass.IDENTIFIER).group(5));
		nthIdentifierPattern(2, CALL, TRANSACTION);
		nthIdentifierPattern(1, CLEANUP);
		reverseAssignmentPatterns(MOVE, EXACT);
		reverseAssignmentPatterns(MOVE);
		reverseAssignmentPatterns(MOVE_CORRESPONDING, EXACT);
		reverseAssignmentPatterns(MOVE_CORRESPONDING);
		reverseAssignmentPatterns(UNPACK);
		ASSIGNMENT_PATTERNS.add(patternStart(ASSIGN)
				.sequence(ETokenClass.IDENTIFIER).group(2)
				.sequence(ARROW, MULT).sequence(EnumSet.of(TO, QUESTION_TO))
				.sequence(FIELD_SYMBOL_OR_DATA_PATTERN).group(1));
		reverseAssignmentPatterns(ASSIGN);
		ASSIGNMENT_PATTERNS.add(patternStart(UNASSIGN).sequence(
				FIELD_SYMBOL_PATTERN).group(3));
		ASSIGNMENT_PATTERNS.add(patternStart(GET, REFERENCE)
				.skipTo(VARIABLE_OR_LITERAL_PATTERN).group(2)
				.skipTo(VARIABLE_OR_LITERAL_PATTERN).group(1));
		ASSIGNMENT_PATTERNS.add(patternStart(CLEAR).skipTo(IDENTIFIER).group(1)
				.notFollowedBy(GT).skipTo(NULL, VARIABLE_OR_LITERAL_PATTERN)
				.group(2));
		ASSIGNMENT_PATTERNS.add(patternStart(CLEAR).skipTo(IDENTIFIER).group(4)
				.endOfStream());
		nthIdentifierPattern(1, FREE);
		nthIdentifierPattern(2, ADD);
		nthIdentifierPattern(2, SUBTRACT);
		nthIdentifierPattern(1, MULTIPLY);
		nthIdentifierPattern(1, DIVIDE);
		nthIdentifierPattern(2, ADD_CORRESPONDING);
		nthIdentifierPattern(2, SUBTRACT_CORRESPONDING);
		nthIdentifierPattern(1, MULTIPLY_CORRESPONDING);
		nthIdentifierPattern(1, DIVIDE_CORRESPONDING);
		ASSIGNMENT_PATTERNS.add(patternStart(CONCATENATE).skipTo(INTO)
				.sequence(FIELD_SYMBOL_OR_DATA_PATTERN).group(0));
		nthIdentifierPattern(1, CONDENSE);
		nthIdentifierPattern(2, CONVERT, TEXT);
		ASSIGNMENT_PATTERNS.add(patternStart(EnumSet.of(FIND, REPLACE))
				.repeated(
						new TokenPattern()
								.skipTo(RESULTS, COUNT, OFFSET, LENGTH)
								.sequence(ETokenClass.IDENTIFIER).group(0))
				.optional(
						new TokenPattern().skipTo(SUBMATCHES)
								.repeated(ETokenClass.IDENTIFIER).group(0))
				.endOfStream());
		nthIdentifierPattern(3, GET, BIT);
		nthIdentifierPattern(1, OVERLAY);
		nthIdentifierPattern(2, SET, BIT);
		nthIdentifierPattern(1, SHIFT);
		ASSIGNMENT_PATTERNS
				.add(patternStart(SPLIT).skipTo(INTO).notFollowedBy(TABLE)
						.repeated(ETokenClass.IDENTIFIER).group(0));
		nthIdentifierPattern(1, TRANSLATE);
		nthIdentifierPattern(2, WRITE);
		assigningOrReferencePattern(APPEND);
		nthIdentifierPattern(1, LOOP, AT, SCREEN);
		assigningOrReferenceOrIntoPattern(LOOP, AT);
		assigningOrReferencePattern(COLLECT);
		assigningOrReferencePattern(INSERT);
		assigningOrReferencePattern(MODIFY);
		assigningOrReferenceOrIntoPattern(READ, TABLE);
		nthIdentifierPattern(1, EXTRACT);
		allExceptFirstNIdentifiersPattern(1, DESCRIBE, FIELD);
		allExceptFirstNIdentifiersPattern(1, DESCRIBE, TABLE);
		allExceptFirstNIdentifiersPattern(2, DESCRIBE, DISTANCE);
		allExceptFirstNIdentifiersPattern(0, GET,
				EnumSet.of(CURSOR, PF_STATUS, LOCALE, TIME, BADI));
		allExceptFirstNIdentifiersPattern(0, DESCRIBE, LIST);
		readLinePattern();
		nthIdentifierPattern(2, MESSAGE);
		selectLikePattern(FETCH, NEXT, CURSOR);
		nthIdentifierPattern(1, OPEN, CURSOR);
		selectLikePattern(SELECT);
		allExceptFirstNIdentifiersPattern(1, GET, DATASET);
		ASSIGNMENT_PATTERNS.add(patternStart(READ, DATASET)
				.skipTo(INTO)
				.sequence(ETokenClass.IDENTIFIER)
				.group(0)
				.optional(
						new TokenPattern().skipTo(ACTUAL).optional(LENGTH)
								.sequence(ETokenClass.IDENTIFIER).group(0)));
		nthIdentifierPattern(2, GET, PARAMETER);
		allAfterStopWordPattern(INTO, CONVERT, EnumSet.of(DATE, TIME));
		allExceptFirstNIdentifiersPattern(1, GENERATE, SUBROUTINE, POOL);
		allAfterStopWordPattern(MAXIMUM, INSERT, REPORT);
		allExceptFirstNIdentifiersPattern(1, READ, REPORT);
		ASSIGNMENT_PATTERNS.add(patternStart(READ, TEXTPOOL).skipTo(INTO)
				.sequence(ETokenClass.IDENTIFIER).group(0));
		ASSIGNMENT_PATTERNS.add(patternStart(SYNTAX_CHECK).repeated(
				new TokenPattern()
						.skipTo(MESSAGE, LINE_KEYWORD, INCLUDE, OFFSET,
								MESSAGE_ID, WORD)
						.sequence(ETokenClass.IDENTIFIER).group(0)));
		ASSIGNMENT_PATTERNS.add(patternStart(GET, PROPERTY).skipTo(EQ)
				.sequence(ETokenClass.IDENTIFIER).group(0));
		ASSIGNMENT_PATTERNS.add(patternStart(CALL, TRANSFORMATION).skipTo(
				RESULT).alternative(
				new TokenPattern().sequence(XML)
						.sequence(ETokenClass.IDENTIFIER).group(0),
				new TokenPattern().repeated(LIST_OF_PARAMETERS_PATTERN)));
	}

	/**
	 * Creates the assignment patterns for "reverse" assignments of the form
	 * "keyword a to b", which correspond to an assignment "b = a".
	 */
	private static void reverseAssignmentPatterns(Object... matchTerms) {
		// assignment of a literal or variable
		ASSIGNMENT_PATTERNS.add(patternStart(matchTerms)
				.sequence(VARIABLE_OR_LITERAL_PATTERN).group(2)
				.sequence(EnumSet.of(TO, QUESTION_TO))
				.sequence(FIELD_SYMBOL_OR_DATA_PATTERN).group(1));
		// assignment of an expression
		ASSIGNMENT_PATTERNS.add(patternStart(matchTerms)
				.skipTo(TO, QUESTION_TO).sequence(FIELD_SYMBOL_OR_DATA_PATTERN)
				.group(0));
	}

	/**
	 * Creates the assignment pattern for statements that look like a "select"
	 * and that begin with the given match terms.
	 */
	private static void selectLikePattern(Object... matchTerms) {
		TokenPattern severalWritesPattern = new TokenPattern()
				.sequence(LPAREN)
				.sequence(ETokenClass.IDENTIFIER)
				.group(0)
				.repeated(
						new TokenPattern().sequence(COMMA)
								.sequence(ETokenClass.IDENTIFIER).group(0));
		ASSIGNMENT_PATTERNS.add(patternStart(matchTerms)
				.skipTo(INTO, APPENDING)
				.optional(CORRESPONDING, FIELDS, OF)
				.notFollowedBy(TABLE)
				.alternative(
						new TokenPattern().sequence(ETokenClass.IDENTIFIER)
								.group(0), severalWritesPattern));
	}

	/**
	 * Creates the assignment pattern for the "read line" statement, which is
	 * quite complicated.
	 * 
	 * It may optionally contain the term "line value into a", which assigns to
	 * a. It may also optionally contain the term "field value b", which assigns
	 * to identifiers in b. b is a list of terms "c into d", which assigns to d
	 * or simply "e", which assigns to e.
	 */
	private static void readLinePattern() {
		TokenPattern lineValueSubpattern = new TokenPattern()
				.skipTo(LINE_KEYWORD).sequence(VALUE, INTO)
				.sequence(ETokenClass.IDENTIFIER).group(0);
		TokenPattern fieldValueAssignmentListSubpattern = new TokenPattern()
				.optional(
						new TokenPattern().sequence(ETokenClass.IDENTIFIER)
								.sequence(INTO))
				.sequence(ETokenClass.IDENTIFIER).group(0);
		TokenPattern fieldValueSubpattern = new TokenPattern().skipTo(FIELD)
				.sequence(VALUE).repeated(fieldValueAssignmentListSubpattern);
		ASSIGNMENT_PATTERNS.add(patternStart(READ, LINE_KEYWORD).alternative(
				lineValueSubpattern, fieldValueSubpattern));
	}

	/**
	 * Creates a pattern starting with the given match terms which extracts an
	 * "other" write to the nth found identifier.
	 */
	private static void nthIdentifierPattern(int n, Object... matchTerms) {
		CCSMAssert.isTrue(n > 0,
				"Cannot create a pattern for the 0th identifier");
		TokenPattern pattern = patternStart(matchTerms);
		for (int i = 0; i < n; i++) {
			pattern.skipTo(ETokenClass.IDENTIFIER);
		}
		ASSIGNMENT_PATTERNS.add(pattern.group(0));
	}

	/**
	 * Creates a pattern starting with the given match terms which extracts
	 * "other" writes to all identifiers, except the first N.
	 */
	private static void allExceptFirstNIdentifiersPattern(int n,
			Object... matchTerms) {
		TokenPattern pattern = patternStart(matchTerms);
		for (int i = 0; i < n; i++) {
			pattern.skipTo(ETokenClass.IDENTIFIER);
		}
		TokenPattern identiferPattern = new TokenPattern().skipTo(
				FIELD_SYMBOL_OR_DATA_PATTERN).group(0);
		pattern.sequence(identiferPattern);
		pattern.repeated(identiferPattern);
		ASSIGNMENT_PATTERNS.add(pattern);
	}

	/**
	 * Creates a pattern starting with the given match terms which ends with the
	 * {@link #ASSIGNING_OR_REFERENCE_INTO_PATTERN}.
	 */
	private static void assigningOrReferencePattern(Object... matchTerms) {
		ASSIGNMENT_PATTERNS.add(patternStart(matchTerms).optional(
				ASSIGNING_OR_REFERENCE_INTO_PATTERN).endOfStream());
	}

	/**
	 * Creates a pattern starting with the given match terms which either ends
	 * with the {@link #ASSIGNING_OR_REFERENCE_INTO_PATTERN} or with "into a",
	 * where a is the assigned variable.
	 */
	private static void assigningOrReferenceOrIntoPattern(Object... matchTerms) {
		ASSIGNMENT_PATTERNS.add(patternStart(matchTerms)
				.sequence(ETokenClass.IDENTIFIER).repeated(ARROW, IDENTIFIER)
				.optional(ARROWSTAR).sequence(INTO)
				.sequence(ETokenClass.IDENTIFIER).group(0));
		ASSIGNMENT_PATTERNS.add(patternStart(matchTerms).skipTo(
				ASSIGNING_OR_REFERENCE_INTO_PATTERN));
	}

	/**
	 * Creates a pattern starting with the given match terms which skips
	 * everything up to the first occurrence of the stop word and then extracts
	 * an "other" write for the first found identifier.
	 */
	private static void allAfterStopWordPattern(Object stopWord,
			Object... matchTerms) {
		ASSIGNMENT_PATTERNS.add(patternStart(matchTerms).skipTo(stopWord)
				.repeated(
						new TokenPattern().skipTo(ETokenClass.IDENTIFIER)
								.group(0)));
	}

	/**
	 * Creates a pattern starting with the given match terms.
	 */
	private static TokenPattern patternStart(Object... matchTerms) {
		return new TokenPattern().beginningOfStream().sequence(matchTerms);
	}

}
