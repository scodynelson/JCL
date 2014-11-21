package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.SyntaxType;
import jcl.structs.functions.FunctionStruct;

import java.math.BigInteger;

/**
 * Defines a ReaderMacroFunction type.
 */
public abstract class ReaderMacroFunction extends FunctionStruct {

	/**
	 * Interpret the character stream from the provided {@link Reader} (up to End-of-File or new line) based on the
	 * provided {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the character code point that determines the macro function
	 * @param reader
	 * 		the {@link Reader} used to read tokens
	 * @param numArg
	 * 		the optional number argument
	 *
	 * @return the parsed {@link LispStruct} token
	 */
	public abstract LispStruct readMacro(int codePoint, Reader reader, BigInteger numArg);

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null; // TODO: do this
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE} based on the current readtable
	 * available in the provided {@link Reader}.
	 *
	 * @param reader
	 * 		the {@link Reader} used to verify the provided {@code codePoint}'s {@link SyntaxType}
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE}; false otherwise
	 */
	protected static boolean isWhitespace(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE);
	}

	/**
	 * Determines if the provided {@code codePoint} matches one of the provided {@link SyntaxType}s based on the
	 * current readtable available in the provided {@link Reader}.
	 *
	 * @param reader
	 * 		the {@link Reader} used to verify the provided {@code codePoint}'s {@link SyntaxType}
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 * @param syntaxTypes
	 * 		the list of {@link SyntaxType}s to check against the provided {@code codePoint}
	 *
	 * @return true if the provided {@code codePoint} matches one of the provided {@link SyntaxType}s; false otherwise
	 */
	protected static boolean isSyntaxType(final Reader reader, final int codePoint, final SyntaxType... syntaxTypes) {

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (reader.getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE} or {@link
	 * SyntaxType#TERMINATING}
	 * based on the current readtable available in the provided {@link Reader}.
	 *
	 * @param reader
	 * 		the {@link Reader} used to verify the provided {@code codePoint}'s {@link SyntaxType}
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#WHITESPACE} or {@link
	 * SyntaxType#TERMINATING}; false otherwise
	 */
	protected static boolean isWhitespaceOrTerminating(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING);
	}

	/**
	 * Determines if the provided {@code codePoint} matches one of the provided {@link AttributeType}s based on the
	 * current readtable available in the provided {@link Reader}.
	 *
	 * @param reader
	 * 		the {@link Reader} used to verify the provided {@code codePoint}'s {@link AttributeType}
	 * @param codePoint
	 * 		the character code point to verify {@link AttributeType} for
	 * @param attributeTypes
	 * 		the list of {@link AttributeType}s to check against the provided {@code codePoint}
	 *
	 * @return true if the provided {@code codePoint} matches one of the provided {@link AttributeType}s; false
	 * otherwise
	 */
	protected static boolean isAttributeType(final Reader reader, final int codePoint, final AttributeType... attributeTypes) {

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (reader.getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}
}
