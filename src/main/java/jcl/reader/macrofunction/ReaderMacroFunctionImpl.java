/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.functions.FunctionStruct;
import jcl.reader.AttributeType;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.ReaderVariables;
import jcl.reader.SyntaxType;

import java.math.BigInteger;

/**
 * Defines a ReaderMacroFunction type.
 */
public abstract class ReaderMacroFunctionImpl extends FunctionStruct implements ReaderMacroFunction {

	/**
	 * Interpret the character stream from the provided {@link Reader} (up to End-of-File or new line) based on the
	 * provided {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the character code point that determines the macro function
	 * @param reader
	 * 		the {@link jcl.reader.Reader} used to read tokens
	 * @param numArg
	 * 		the optional number argument
	 *
	 * @return the parsed {@link LispStruct} token
	 */
	@Override
	public abstract LispStruct readMacro(int codePoint, Reader reader, BigInteger numArg);

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		// TODO: do this
		return null;
	}

	/**
	 * Gets the {@link SyntaxType} for the provided {@code codePoint} from the {@link #readtable} for the JCL Reader
	 * instance.
	 *
	 * @param codePoint
	 * 		the value to get the {@link SyntaxType} for from the {@link #readtable}
	 *
	 * @return the {@link SyntaxType} for the provided {@code codePoint} from the {@link #readtable}
	 */
	private static SyntaxType getSyntaxType(final int codePoint) {
		return ReaderVariables.READTABLE.getValue().getSyntaxType(codePoint);
	}

	/**
	 * Gets the {@link AttributeType} for the provided {@code codePoint} from the {@link #readtable} for the JCL Reader
	 * instance.
	 *
	 * @param codePoint
	 * 		the value to get the {@link AttributeType} for from the {@link #readtable}
	 *
	 * @return the {@link AttributeType} for the provided {@code codePoint} from the {@link #readtable}
	 */
	private static AttributeType getAttributeType(final int codePoint) {
		return ReaderVariables.READTABLE.getValue().getAttributeType(codePoint);
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
