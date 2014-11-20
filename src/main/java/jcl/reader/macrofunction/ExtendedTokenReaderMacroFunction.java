package jcl.reader.macrofunction;

import jcl.reader.Reader;
import jcl.reader.syntax.CaseSpec;
import jcl.reader.syntax.ReadExtendedToken;
import jcl.structs.streams.ReadResult;

abstract class ExtendedTokenReaderMacroFunction extends ReaderMacroFunction {

	private final boolean isEscaped;

	protected ExtendedTokenReaderMacroFunction(final boolean isEscaped) {
		this.isEscaped = isEscaped;
	}

	private static void readSingleEscape(final Reader reader, final StringBuilder stringBuilder) {
		readToken(reader, true, false, stringBuilder, true);
	}

	private static void readMultipleEscape(final Reader reader, final StringBuilder stringBuilder) {

		ReadResult tempReadResult = reader.readChar(true, null, false);
		int tempCodePoint = tempReadResult.getResult();

		while (!isMultipleEscape(reader, tempCodePoint)) {

			if (isSingleEscape(reader, tempCodePoint)) {
				appendToken(reader, tempReadResult, stringBuilder, false); // NOTE: This comes first so we build the token right
				readSingleEscape(reader, stringBuilder);
			} else {
				appendToken(reader, tempReadResult, stringBuilder, true);
			}

			tempReadResult = reader.readChar(true, null, false);
			tempCodePoint = tempReadResult.getResult();
		}
		appendToken(reader, tempReadResult, stringBuilder, false);
	}

	private static ReadResult readToken(final Reader reader, final boolean eofErrorP, final boolean recursiveP,
										final StringBuilder stringBuilder, final boolean isEscaped) {
		final ReadResult readResult = reader.readChar(eofErrorP, null, recursiveP);
		appendToken(reader, readResult, stringBuilder, isEscaped);
		return readResult;
	}

	private static void appendToken(final Reader reader, final ReadResult readResult,
									final StringBuilder stringBuilder, final boolean isEscaped) {
		if (!readResult.wasEOF()) {
			int token = readResult.getResult();
			if (!isEscaped) {
				token = getTokenWithCase(reader, token);
			}
			stringBuilder.appendCodePoint(token);
		}
	}

	private static int getTokenWithCase(final Reader reader, final int currentToken) {
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

	private static void unreadToken(final Reader reader, final StringBuilder stringBuilder, final int codePoint) {
		reader.unreadChar(codePoint);
		stringBuilder.deleteCharAt(stringBuilder.length() - 1); // Remove the last character read from the builder
	}

	protected ReadExtendedToken readExtendedToken(final Reader reader) {

		final StringBuilder stringBuilder = new StringBuilder();

		ReadResult readResult = readToken(reader, false, !isEscaped, stringBuilder, isEscaped);

		if (isEscaped) {
			readResult = readToken(reader, false, false, stringBuilder, false);
		}

		boolean hasEscapes = false;
		boolean hasPackageDelimiter = false;

		while (!readResult.wasEOF()) {

			final int codePoint = readResult.getResult();
			if (isWhitespaceOrTerminating(reader, codePoint)) {
				unreadToken(reader, stringBuilder, codePoint);
				break;
			}

			if (isSingleEscape(reader, codePoint)) {
				readSingleEscape(reader, stringBuilder);
				hasEscapes = true;
			} else if (isMultipleEscape(reader, codePoint)) {
				readMultipleEscape(reader, stringBuilder);
				hasEscapes = true;
			}

			if (!hasPackageDelimiter) {
				hasPackageDelimiter = isPackageMarker(reader, codePoint);
			}

			readResult = readToken(reader, false, false, stringBuilder, false);
		}

		return new ReadExtendedToken(stringBuilder.toString(), hasEscapes, hasPackageDelimiter);
	}
}
