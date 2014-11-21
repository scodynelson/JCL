package jcl.reader.macrofunction;

import jcl.reader.Reader;
import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.CaseSpec;
import jcl.reader.syntax.SyntaxType;
import jcl.structs.streams.ReadResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Reader Macro Function for handling the reading of extended characters, handling proper character casing and
 * escaping.
 */
abstract class ExtendedTokenReaderMacroFunction extends ReaderMacroFunction {

	private final boolean isEscaped;

	/**
	 * Protected constructor.
	 *
	 * @param isEscaped
	 * 		whether or not characters read in by the JCL Reader should be considered escaped or not.
	 */
	protected ExtendedTokenReaderMacroFunction(final boolean isEscaped) {
		this.isEscaped = isEscaped;
	}

	/**
	 * Reads in and returns an extended token character containing the token, whether or not the token has escape
	 * characters, and whether or not the token has a package delimiter.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the token
	 *
	 * @return a {@link ReadExtendedToken} object containing the extended token information as read
	 */
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
				reader.unreadChar(codePoint);
				stringBuilder.deleteCharAt(stringBuilder.length() - 1); // Remove the last character read from the builder
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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	/**
	 * Reads in the next token considering a 'single escape' character code point.
	 *
	 * @param reader
	 * 		the {@link Reader} used when reading and appending the next token character
	 * @param stringBuilder
	 * 		the {@link StringBuilder} to append the next token character
	 */
	private static void readSingleEscape(final Reader reader, final StringBuilder stringBuilder) {
		readToken(reader, true, false, stringBuilder, true);
	}

	/**
	 * Reads in the next token considering a 'multiple escape' character code point. This is used in reading through
	 * comments specifically so that comments are properly escaped, but the next token is considered correctly.
	 *
	 * @param reader
	 * 		the {@link Reader} used when reading and appending the next token character(s)
	 * @param stringBuilder
	 * 		the {@link StringBuilder} to append the next token character(s)
	 */
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

	/**
	 * Reads the next token character from the provided {@link Reader}, using the provided {@code eofErrorP} and {@code
	 * recursiveP} to determine how the token character should be read. Then it appends the resulting {@link
	 * ReadResult} to the provided {@link StringBuilder} using the provided {@code isEscaped} value in determining the
	 * appropriate case of the token character to append.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read the next token character
	 * @param eofErrorP
	 * 		whether or not the reader will fail when an End-Of-File is reached
	 * @param recursiveP
	 * 		whether or not to recursively process the next read operation
	 * @param stringBuilder
	 * 		the {@link StringBuilder} to append the next token to
	 * @param isEscaped
	 * 		whether or not to attempt to modify the case of the read token
	 *
	 * @return the resulting {@link ReadResult} of the read operation performed by the {@link Reader}
	 */
	private static ReadResult readToken(final Reader reader, final boolean eofErrorP, final boolean recursiveP,
										final StringBuilder stringBuilder, final boolean isEscaped) {
		final ReadResult readResult = reader.readChar(eofErrorP, null, recursiveP);
		appendToken(reader, readResult, stringBuilder, isEscaped);
		return readResult;
	}

	/**
	 * Appends the token result in the provided {@link ReadResult} to the provided {@link StringBuilder}, making to to
	 * properly case the token based on the provided {@code isEscaped}.
	 *
	 * @param reader
	 * 		the {@link Reader} used in correctly casing the token if needed
	 * @param readResult
	 * 		the {@link ReadResult} containing the token result to append
	 * @param stringBuilder
	 * 		the {@link StringBuilder} to append the next token to
	 * @param isEscaped
	 * 		whether or not to attempt to modify the case of the read token
	 */
	private static void appendToken(final Reader reader, final ReadResult readResult, final StringBuilder stringBuilder,
									final boolean isEscaped) {
		if (!readResult.wasEOF()) {
			int token = readResult.getResult();
			if (!isEscaped) {
				token = getTokenWithCase(reader, token);
			}
			stringBuilder.appendCodePoint(token);
		}
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#SINGLE_ESCAPE} based on the current
	 * readtable available in the provided {@link Reader}.
	 *
	 * @param reader
	 * 		the {@link Reader} used to verify the provided {@code codePoint}'s {@link SyntaxType}
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#SINGLE_ESCAPE}; false otherwise
	 */
	private static boolean isSingleEscape(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.SINGLE_ESCAPE);
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#SINGLE_ESCAPE} based on the current
	 * readtable available in the provided {@link Reader}.
	 *
	 * @param reader
	 * 		the {@link Reader} used to verify the provided {@code codePoint}'s {@link SyntaxType}
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#SINGLE_ESCAPE}; false otherwise
	 */
	private static boolean isMultipleEscape(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.MULTIPLE_ESCAPE);
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#CONSTITUENT} and a {@link
	 * AttributeType#PACKAGEMARKER} based on the current readtable available in the provided {@link Reader}.
	 *
	 * @param reader
	 * 		the {@link Reader} used to verify the provided {@code codePoint}'s {@link SyntaxType}
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#CONSTITUENT} and a {@link
	 * AttributeType#PACKAGEMARKER}; false otherwise
	 */
	private static boolean isPackageMarker(final Reader reader, final int codePoint) {
		return isSyntaxType(reader, codePoint, SyntaxType.CONSTITUENT)
				&& isAttributeType(reader, codePoint, AttributeType.PACKAGEMARKER);
	}

	/**
	 * Transforms the provided {@code currentToken} value into it's correct case based on properties of the current
	 * readtable for the provided {@link Reader}.
	 *
	 * @param reader
	 * 		the {@link Reader} used to determine the proper token case to transform to
	 * @param currentToken
	 * 		the token to transform to the proper case
	 *
	 * @return the transformed token with the correct case
	 */
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

	/**
	 * Holds the results of an extended token read operation with the token string, whether or not the token has escape
	 * characters, and whether or not the token contains a package delimiter.
	 */
	protected static final class ReadExtendedToken {

		private final String token;
		private final boolean hasEscapes;
		private final boolean hasPackageDelimiter;

		/**
		 * Private constructor.
		 *
		 * @param token
		 * 		the token string
		 * @param hasEscapes
		 * 		whether or not the token has escape characters
		 * @param hasPackageDelimiter
		 * 		whether or not the token contains a package delimiter
		 */
		private ReadExtendedToken(final String token, final boolean hasEscapes, final boolean hasPackageDelimiter) {
			this.token = token;
			this.hasEscapes = hasEscapes;
			this.hasPackageDelimiter = hasPackageDelimiter;
		}

		/**
		 * Getter for {@link #token} property.
		 *
		 * @return {@link #token} property
		 */
		String getToken() {
			return token;
		}

		/**
		 * Getter for {@link #hasEscapes} property.
		 *
		 * @return {@link #hasEscapes} property
		 */
		boolean hasEscapes() {
			return hasEscapes;
		}

		/**
		 * Getter for {@link #hasPackageDelimiter} property.
		 *
		 * @return {@link #hasPackageDelimiter} property
		 */
		boolean hasPackageDelimiter() {
			return hasPackageDelimiter;
		}

		@Override
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
