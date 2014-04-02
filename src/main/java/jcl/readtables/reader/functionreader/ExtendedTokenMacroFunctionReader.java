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
			if (!hasPackageDelimiter) {
				hasPackageDelimiter = MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.CONSTITUENT)
						&& MacroFunctionReaderUtils.isAttributeType(reader, codePoint, AttributeType.PACKAGEMARKER);
			}

			if (MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.SINGLE_ESCAPE)) {
				readSingleEscape(stringBuilder);
				hasEscapes = true;
			} else if (MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.MULTIPLE_ESCAPE)) {
				readMultipleEscape(stringBuilder);
				hasEscapes = true;
			} else if (MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING)) {
				// TODO: We want to take "read-preserving-whitespace" into account here before unreading
				reader.unreadChar(codePoint);
				stringBuilder.deleteCharAt(stringBuilder.length() - 1); // Remove the last character read from the builder
				break;
			}

			readResult = readToken(false, false, stringBuilder, false);
		}

		return new ReadExtendedToken(stringBuilder.toString(), hasEscapes, hasPackageDelimiter);
	}

	private void readSingleEscape(final StringBuilder stringBuilder) {
		readToken(true, false, stringBuilder, true);
	}

	private void readMultipleEscape(final StringBuilder stringBuilder) {
		final CaseSpec readtableCase = reader.getReadtableCase();

		ReadResult tempReadResult = reader.readChar(true, null, false);
		int tempCodePoint = tempReadResult.getResult();

		while (!MacroFunctionReaderUtils.isSyntaxType(reader, tempCodePoint, SyntaxType.MULTIPLE_ESCAPE)) {

			if (MacroFunctionReaderUtils.isSyntaxType(reader, tempCodePoint, SyntaxType.SINGLE_ESCAPE)) {
				appendToken(tempReadResult, stringBuilder, false, readtableCase);
				readSingleEscape(stringBuilder);
			} else {
				appendToken(tempReadResult, stringBuilder, true, readtableCase);
			}

			tempReadResult = reader.readChar(true, null, false);
			tempCodePoint = tempReadResult.getResult();
		}
		appendToken(tempReadResult, stringBuilder, false, readtableCase);
	}

	private ReadResult readToken(final boolean eofErrorP, final boolean recursiveP, final StringBuilder stringBuilder,
								 final boolean isEscaped) {
		final ReadResult readResult = reader.readChar(eofErrorP, null, recursiveP);
		final CaseSpec readtableCase = reader.getReadtableCase();
		appendToken(readResult, stringBuilder, isEscaped, readtableCase);
		return readResult;
	}

	private static void appendToken(final ReadResult readResult, final StringBuilder stringBuilder, final boolean isEscaped,
									final CaseSpec readtableCase) {
		if (!readResult.wasEOF()) {
			int token = readResult.getResult();
			if (!isEscaped) {
				token = getTokenWithCase(token, readtableCase);
			}
			stringBuilder.appendCodePoint(token);
		}
	}

	private static int getTokenWithCase(final int currentToken, final CaseSpec readtableCase) {
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
}
