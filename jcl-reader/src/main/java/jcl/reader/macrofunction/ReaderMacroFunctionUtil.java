/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.number.IntegerStructImpl;
import jcl.lang.readtable.AttributeType;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.readtable.SyntaxType;

/**
 * Utility class for common operations for {@link ReaderMacroFunction}s.
 */
final class ReaderMacroFunctionUtil {

	/**
	 * Private constructor.
	 */
	private ReaderMacroFunctionUtil() {
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE} based on the current readtable.
	 *
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE}; false otherwise
	 */
	static boolean isWhitespace(final int codePoint) {
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
	static boolean isSyntaxType(final int codePoint, final SyntaxType... syntaxTypes) {

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
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
		return ReaderVariables.READTABLE.getVariableValue().getSyntaxType(codePoint);
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
	static boolean isWhitespaceOrTerminating(final int codePoint) {
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
	static boolean isAttributeType(final int codePoint, final AttributeType... attributeTypes) {

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
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
		final IntegerStructImpl readBase = ReaderVariables.READ_BASE.getVariableValue();
		return ReaderVariables.READTABLE.getVariableValue().getAttributeType(codePoint, readBase);
	}
}
