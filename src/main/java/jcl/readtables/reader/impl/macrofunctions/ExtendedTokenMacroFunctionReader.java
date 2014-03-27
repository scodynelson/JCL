package jcl.readtables.reader.impl.macrofunctions;

import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.ReadtableStruct;
import jcl.readtables.reader.Reader;
import jcl.readtables.reader.syntax.ReadExtendedToken;
import jcl.syntax.AttributeType;
import jcl.syntax.CaseSpec;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;
import org.apache.commons.collections4.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

public class ExtendedTokenMacroFunctionReader {

	private final Reader reader;

	public ExtendedTokenMacroFunctionReader(final Reader reader) {
		this.reader = reader;
	}

	public ReadExtendedToken readExtendedToken(final boolean eofErrorP) {

		final StringBuilder stringBuilder = new StringBuilder();
		final ReadResult readResult = readInternalToken(stringBuilder);

		if (readResult.wasEOF()) {
			if (eofErrorP) {
				throw new ReaderErrorException("EOF after escape character.");
			} else {
				return new ReadExtendedToken("", false, false);
			}
		} else {
			final InternalReadExtendedToken readExtendedToken = internalReadExtendedToken(stringBuilder, readResult, false);

			final List<Integer> escapeIndices = readExtendedToken.getEscapeIndices();
			final Integer colon = readExtendedToken.getFirstPackageDelimiter();

			final String tokenWithProperCase = getTokenWithProperCase(reader, stringBuilder, escapeIndices);

			return new ReadExtendedToken(tokenWithProperCase, CollectionUtils.isNotEmpty(escapeIndices), colon != null);
		}
	}

	private static String getTokenWithProperCase(final Reader reader, final StringBuilder token, final List<Integer> escapeIndices) {
		final ReadtableStruct readtable = reader.getReadtable();
		final CaseSpec readtableCase = readtable.getReadtableCase();

		final StringBuilder stringBuilder = new StringBuilder(token.length());
		for (int i = 0; i < token.length(); i++) {

			final int currentToken = token.codePointAt(i);
			if (escapeIndices.contains(i)) {
				stringBuilder.appendCodePoint(currentToken);
			} else {
				switch (readtableCase) {
					case UPCASE:
						final int upperCaseToken = Character.toUpperCase(currentToken);
						stringBuilder.appendCodePoint(upperCaseToken);
						break;
					case DOWNCASE:
						final int lowerCaseToken = Character.toLowerCase(currentToken);
						stringBuilder.appendCodePoint(lowerCaseToken);
						break;
					case INVERT:
						final int invertedCaseToken = Character.isUpperCase(currentToken) ? Character.toLowerCase(currentToken) : Character.toUpperCase(currentToken);
						stringBuilder.appendCodePoint(invertedCaseToken);
						break;
					case PRESERVE:
						stringBuilder.appendCodePoint(currentToken);
						break;
				}
			}
		}
		return stringBuilder.toString();
	}

	private InternalReadExtendedToken internalReadExtendedToken(final StringBuilder stringBuilder, final ReadResult firstChar,
																final boolean escapeFirstChar) {

		final List<Integer> escapes = new ArrayList<>();

		ReadResult firstResult = firstChar;

		if (escapeFirstChar) {
			escapes.add(stringBuilder.length() - 1);

			firstResult = readInternalToken(stringBuilder);
		}

		ReadResult readResult = firstResult;
		Integer colon = null;

		while (!readResult.wasEOF()) {

			final int codePoint = readResult.getResult();
			if (MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING)) {
				// TODO: We want to take "read-preserving-whitespace" into account here before unreading
				reader.unreadChar(codePoint);
				stringBuilder.deleteCharAt(stringBuilder.length() - 1);
				break;
			}

			if (MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.SINGLE_ESCAPE)) {
				final ReadResult nextReadResult = readInternalToken(stringBuilder);

				if (nextReadResult.wasEOF()) {
					throw new ReaderErrorException("EOF after escape character.");
				} else {
					escapes.add(stringBuilder.length() - 1);
				}
			} else if (MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.MULTIPLE_ESCAPE)) {
				while (true) {

					final ReadResult tempReadResult = readInternalToken(stringBuilder);

					if (tempReadResult.wasEOF()) {
						throw new ReaderErrorException("EOF inside extended token.");
					}

					final int tempCodePoint = tempReadResult.getResult();
					if (MacroFunctionReaderUtils.isSyntaxType(reader, tempCodePoint, SyntaxType.MULTIPLE_ESCAPE)) {
						break;
					} else if (MacroFunctionReaderUtils.isSyntaxType(reader, tempCodePoint, SyntaxType.SINGLE_ESCAPE)) {

						final ReadResult nextReadResult = readInternalToken(stringBuilder);

						if (nextReadResult.wasEOF()) {
							throw new ReaderErrorException("EOF after escape character.");
						} else {
							escapes.add(stringBuilder.length() - 1);
						}
					} else {
						escapes.add(stringBuilder.length() - 1);
					}
				}
			} else {
				if (MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.CONSTITUENT)
						&& MacroFunctionReaderUtils.isAttributeType(reader, codePoint, AttributeType.PACKAGEMARKER)
						&& (colon == null)) {
					colon = stringBuilder.length() - 1;
				}
			}

			readResult = readInternalToken(stringBuilder);
		}

		return new InternalReadExtendedToken(escapes, colon);
	}

	private ReadResult readInternalToken(final StringBuilder stringBuilder) {

		final ReadResult readResult = reader.readChar(false, null, true);
		if (!readResult.wasEOF()) {
			stringBuilder.appendCodePoint(readResult.getResult());
		}
		return readResult;
	}

	private static class InternalReadExtendedToken {

		private final List<Integer> escapeIndices;
		private final Integer firstPackageDelimiter;

		private InternalReadExtendedToken(final List<Integer> escapeIndices, final Integer firstPackageDelimiter) {
			this.escapeIndices = escapeIndices;
			this.firstPackageDelimiter = firstPackageDelimiter;
		}

		public List<Integer> getEscapeIndices() {
			return escapeIndices;
		}

		public Integer getFirstPackageDelimiter() {
			return firstPackageDelimiter;
		}
	}
}
