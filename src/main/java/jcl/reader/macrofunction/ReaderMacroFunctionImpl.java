/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.reader.AttributeType;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.SyntaxType;
import jcl.reader.struct.ReaderVariables;

/**
 * Abstract implementation definition for all Reader defined macro functions that read character macros based off of a
 * provided {@link Integer} code point.
 */
public abstract class ReaderMacroFunctionImpl extends FunctionStruct implements ReaderMacroFunction {

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		// TODO: do this
		return null;
	}

	/**
	 * Gets the {@link SyntaxType} for the provided {@code codePoint} from the {@link ReaderVariables#READTABLE} value.
	 *
	 * @param codePoint
	 * 		the value to get the {@link SyntaxType} for from the {@link ReaderVariables#READTABLE} value
	 *
	 * @return the {@link SyntaxType} for the provided {@code codePoint} from {@link ReaderVariables#READTABLE} value
	 */
	private static SyntaxType getSyntaxType(final int codePoint) {
		return ReaderVariables.READTABLE.getValue().getSyntaxType(codePoint);
	}

	/**
	 * Gets the {@link AttributeType} for the provided {@code codePoint} from the {@link ReaderVariables#READTABLE}
	 * value.
	 *
	 * @param codePoint
	 * 		the value to get the {@link AttributeType} for from the {@link ReaderVariables#READTABLE} value
	 *
	 * @return the {@link AttributeType} for the provided {@code codePoint} from the {@link ReaderVariables#READTABLE}
	 * value
	 */
	private static AttributeType getAttributeType(final int codePoint) {
		final IntegerStruct readBase = ReaderVariables.READ_BASE.getValue();
		return ReaderVariables.READTABLE.getValue().getAttributeType(codePoint, readBase);
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE} based on the current readtable.
	 *
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE}; false otherwise
	 */
	protected static boolean isWhitespace(final int codePoint) {
		return isSyntaxType(codePoint, SyntaxType.WHITESPACE);
	}

	/**
	 * Determines if the provided {@code codePoint} matches one of the provided {@link SyntaxType}s based on the
	 * current readtable.
	 *
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 * @param syntaxTypes
	 * 		the list of {@link SyntaxType}s to check against the provided {@code codePoint}
	 *
	 * @return true if the provided {@code codePoint} matches one of the provided {@link SyntaxType}s; false otherwise
	 */
	protected static boolean isSyntaxType(final int codePoint, final SyntaxType... syntaxTypes) {

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE} or {@link
	 * SyntaxType#TERMINATING} based on the current readtable.
	 *
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE} or {@link
	 * SyntaxType#TERMINATING}; false otherwise
	 */
	protected static boolean isWhitespaceOrTerminating(final int codePoint) {
		return isSyntaxType(codePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING);
	}

	/**
	 * Determines if the provided {@code codePoint} matches one of the provided {@link AttributeType}s based on the
	 * current readtable.
	 *
	 * @param codePoint
	 * 		the character code point to verify {@link AttributeType} for
	 * @param attributeTypes
	 * 		the list of {@link AttributeType}s to check against the provided {@code codePoint}
	 *
	 * @return true if the provided {@code codePoint} matches one of the provided {@link AttributeType}s; false
	 * otherwise
	 */
	protected static boolean isAttributeType(final int codePoint, final AttributeType... attributeTypes) {

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}
}
