package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.SyntaxType;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.streams.ReadResult;

import java.math.BigInteger;

/**
 * Defines a ReaderMacroFunction type.
 */
public abstract class ReaderMacroFunction extends FunctionStruct {

	/**
	 * Interpret the character stream (up to EOF or new line) as a token of the T type supplied.
	 *
	 * @param codePoint
	 * 		the character that determines the macro function
	 * @param reader
	 * 		the reader used to read characters
	 * @param numArg
	 * 		the optional number argument
	 *
	 * @return a LispStruct
	 *
	 * @throws ReaderErrorException
	 * 		if an error is encountered
	 */
	public abstract LispStruct readMacro(int codePoint, Reader reader, BigInteger numArg);

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null; // TODO: do this
	}

	protected static int getNextCodePoint(final Reader reader) {
		// NOTE: This will throw errors when it reaches an EOF
		final ReadResult readResult = reader.readChar();
		return readResult.getResult();
	}

	protected static boolean isSingleEscape(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.SINGLE_ESCAPE);
	}

	protected static boolean isSyntaxType(final Reader reader, final int codePoint, final SyntaxType... syntaxTypes) {

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (reader.getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}

	protected static boolean isMultipleEscape(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.SINGLE_ESCAPE);
	}

	protected static boolean isWhitespace(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE);
	}

	protected static boolean isWhitespaceOrTerminating(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING);
	}

	protected static boolean isPackageMarker(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.CONSTITUENT)
				&& isAttributeType(reader, codePoint, AttributeType.PACKAGEMARKER);
	}

	protected static boolean isAttributeType(final Reader reader, final int codePoint, final AttributeType... attributeTypes) {

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (reader.getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}
}
