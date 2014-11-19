package jcl.reader.function.macrofunction;

import jcl.reader.Reader;
import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.SyntaxType;
import jcl.structs.streams.ReadResult;

abstract class MacroFunctionReader<R> {

	protected final Reader reader;

	MacroFunctionReader(final Reader reader) {
		this.reader = reader;
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
