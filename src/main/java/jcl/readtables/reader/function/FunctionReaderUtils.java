package jcl.readtables.reader.function;

import jcl.readtables.reader.Reader;
import jcl.syntax.AttributeType;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;

class FunctionReaderUtils {

	static int getNextCodePoint(final Reader reader) {
		// NOTE: This will throw errors when it reaches an EOF
		final ReadResult readResult = reader.readChar();
		return readResult.getResult();
	}

	static boolean isAttributeType(final Reader reader, final int codePoint, final AttributeType... attributeTypes) {

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (reader.getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}

	static boolean isSyntaxType(final Reader reader, final int codePoint, final SyntaxType... syntaxTypes) {

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (reader.getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}

	static boolean isSingleEscape(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.SINGLE_ESCAPE);
	}

	static boolean isMultipleEscape(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.SINGLE_ESCAPE);
	}

	static boolean isWhitespace(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE);
	}

	static boolean isWhitespaceOrTerminating(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING);
	}

	static boolean isPackageMarker(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.CONSTITUENT)
				&& isAttributeType(reader, codePoint, AttributeType.PACKAGEMARKER);
	}
}
