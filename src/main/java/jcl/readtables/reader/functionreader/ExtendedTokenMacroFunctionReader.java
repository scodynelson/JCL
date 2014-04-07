package jcl.readtables.reader.functionreader;

import jcl.readtables.reader.Reader;
import jcl.readtables.reader.syntax.ReadExtendedToken;
import jcl.syntax.AttributeType;
import jcl.syntax.CaseSpec;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;

public class ExtendedTokenMacroFunctionReader {

	private final Reader reader;

	public ExtendedTokenMacroFunctionReader(final Reader reader) {
		this.reader = reader;
	}

	public ReadExtendedToken readExtendedToken(final boolean isEscaped) {

		final StringBuilder stringBuilder = new StringBuilder();

		ReadResult readResult = readToken(false, !isEscaped, stringBuilder, isEscaped);

		if (isEscaped) {
			readResult = readToken(false, false, stringBuilder, false);
		}

		boolean hasEscapes = false;
		boolean hasPackageDelimiter = false;

		while (!readResult.wasEOF()) {

			final int codePoint = readResult.getResult();
			if (isWhitespaceOrTerminating(codePoint)) {
				unreadToken(stringBuilder, codePoint);
				break;
			}

			if (isSingleEscape(codePoint)) {
				readSingleEscape(stringBuilder);
				hasEscapes = true;
			} else if (isMultipleEscape(codePoint)) {
				readMultipleEscape(stringBuilder);
				hasEscapes = true;
			}

			if (!hasPackageDelimiter) {
				hasPackageDelimiter = isPackageMarker(codePoint);
			}

			readResult = readToken(false, false, stringBuilder, false);
		}

		return new ReadExtendedToken(stringBuilder.toString(), hasEscapes, hasPackageDelimiter);
	}

	private void readSingleEscape(final StringBuilder stringBuilder) {
		readToken(true, false, stringBuilder, true);
	}

	private void readMultipleEscape(final StringBuilder stringBuilder) {

		ReadResult tempReadResult = reader.readChar(true, null, false);
		int tempCodePoint = tempReadResult.getResult();

		while (!isMultipleEscape(tempCodePoint)) {

			if (isSingleEscape(tempCodePoint)) {
				appendToken(tempReadResult, stringBuilder, false); // NOTE: This comes first so we build the token right
				readSingleEscape(stringBuilder);
			} else {
				appendToken(tempReadResult, stringBuilder, true);
			}

			tempReadResult = reader.readChar(true, null, false);
			tempCodePoint = tempReadResult.getResult();
		}
		appendToken(tempReadResult, stringBuilder, false);
	}

	private ReadResult readToken(final boolean eofErrorP, final boolean recursiveP, final StringBuilder stringBuilder,
								 final boolean isEscaped) {
		final ReadResult readResult = reader.readChar(eofErrorP, null, recursiveP);
		appendToken(readResult, stringBuilder, isEscaped);
		return readResult;
	}

	private void appendToken(final ReadResult readResult, final StringBuilder stringBuilder, final boolean isEscaped) {
		if (!readResult.wasEOF()) {
			int token = readResult.getResult();
			if (!isEscaped) {
				token = getTokenWithCase(token);
			}
			stringBuilder.appendCodePoint(token);
		}
	}

	private int getTokenWithCase(final int currentToken) {
		final CaseSpec readtableCase = reader.getReadtableCase();
		switch (readtableCase) {
			case UPCASE:
				return Character.toUpperCase(currentToken);
			case DOWNCASE:
				return Character.toLowerCase(currentToken);
			case INVERT:
				return Character.isUpperCase(currentToken) ? Character.toLowerCase(currentToken) : Character.toUpperCase(currentToken);
			case PRESERVE:
				return currentToken;
		}
		return currentToken;
	}

	private void unreadToken(final StringBuilder stringBuilder, final int codePoint) {
		reader.unreadChar(codePoint);
		stringBuilder.deleteCharAt(stringBuilder.length() - 1); // Remove the last character read from the builder
	}

	private boolean isSingleEscape(final int codePoint) {
		return MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.SINGLE_ESCAPE);
	}

	private boolean isMultipleEscape(final int codePoint) {
		return MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.SINGLE_ESCAPE);
	}

	private boolean isWhitespaceOrTerminating(final int codePoint) {
		return MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING);
	}

	private boolean isPackageMarker(final int codePoint) {
		return MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.CONSTITUENT)
				&& MacroFunctionReaderUtils.isAttributeType(reader, codePoint, AttributeType.PACKAGEMARKER);
	}
}
