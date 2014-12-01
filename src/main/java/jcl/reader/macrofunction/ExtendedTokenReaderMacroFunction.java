/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.reader.AttributeType;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableCase;
import jcl.reader.struct.SyntaxType;
import jcl.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Reader Macro Function for handling the reading of extended characters, handling proper character casing and
 * escaping.
 */
final class ExtendedTokenReaderMacroFunction {

	/**
	 * Private constructor.
	 */
	private ExtendedTokenReaderMacroFunction() {
	}

	/**
	 * Reads in and returns an extended token character containing the token, whether or not the token has escape
	 * characters, and whether or not the token has a package delimiter.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the token
	 * @param isEscaped
	 * 		whether or not characters read in by the JCL Reader should be considered escaped or not
	 *
	 * @return a {@link ReadExtendedToken} object containing the extended token information as read
	 */
	static ReadExtendedToken readExtendedToken(final Reader reader, final boolean isEscaped) {

		final StringBuilder stringBuilder = new StringBuilder();

		ReadPeekResult readResult = readToken(reader, false, !isEscaped, stringBuilder, isEscaped);

		if (isEscaped) {
			readResult = readToken(reader, false, false, stringBuilder, false);
		}

		boolean hasEscapes = false;
		boolean hasPackageDelimiter = false;

		while (!readResult.isEof()) {

			final int codePoint = readResult.getResult();
			if (ReaderMacroFunctionImpl.isWhitespaceOrTerminating(codePoint)) {
				reader.unreadChar(codePoint);

				// Makes sure to remove the last character read from the builder
				stringBuilder.deleteCharAt(stringBuilder.length() - 1);
				break;
			}

			if (isSingleEscape(codePoint)) {
				readSingleEscape(reader, stringBuilder);
				hasEscapes = true;
			} else if (isMultipleEscape(codePoint)) {
				readMultipleEscape(reader, stringBuilder);
				hasEscapes = true;
			}

			if (!hasPackageDelimiter) {
				hasPackageDelimiter = isPackageMarker(codePoint);
			}

			readResult = readToken(reader, false, false, stringBuilder, false);
		}

		return new ReadExtendedToken(stringBuilder.toString(), hasEscapes, hasPackageDelimiter);
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

		ReadPeekResult tempReadResult = reader.readChar(true, null, false);
		int tempCodePoint = tempReadResult.getResult();

		while (!isMultipleEscape(tempCodePoint)) {

			if (isSingleEscape(tempCodePoint)) {
				// NOTE: The following comes first so we build the token right
				appendToken(tempReadResult, stringBuilder, false);
				readSingleEscape(reader, stringBuilder);
			} else {
				appendToken(tempReadResult, stringBuilder, true);
			}

			tempReadResult = reader.readChar(true, null, false);
			tempCodePoint = tempReadResult.getResult();
		}
		appendToken(tempReadResult, stringBuilder, false);
	}

	/**
	 * Reads the next token character from the provided {@link Reader}, using the provided {@code eofErrorP} and {@code
	 * recursiveP} to determine how the token character should be read. Then it appends the resulting {@link
	 * ReadPeekResult} to the provided {@link StringBuilder} using the provided {@code isEscaped} value in determining
	 * the appropriate case of the token character to append.
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
	 * @return the resulting {@link ReadPeekResult} of the read operation performed by the {@link Reader}
	 */
	private static ReadPeekResult readToken(final Reader reader, final boolean eofErrorP, final boolean recursiveP,
	                                        final StringBuilder stringBuilder, final boolean isEscaped) {
		final ReadPeekResult readResult = reader.readChar(eofErrorP, null, recursiveP);
		appendToken(readResult, stringBuilder, isEscaped);
		return readResult;
	}

	/**
	 * Appends the token result in the provided {@link ReadPeekResult} to the provided {@link StringBuilder}, making to
	 * to properly case the token based on the provided {@code isEscaped}.
	 *
	 * @param readResult
	 * 		the {@link ReadPeekResult} containing the token result to append
	 * @param stringBuilder
	 * 		the {@link StringBuilder} to append the next token to
	 * @param isEscaped
	 * 		whether or not to attempt to modify the case of the read token
	 */
	private static void appendToken(final ReadPeekResult readResult, final StringBuilder stringBuilder, final boolean isEscaped) {
		if (!readResult.isEof()) {
			int token = readResult.getResult();
			if (!isEscaped) {
				token = getTokenWithCase(token);
			}
			stringBuilder.appendCodePoint(token);
		}
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#SINGLE_ESCAPE} based on the current
	 * readtable.
	 *
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#SINGLE_ESCAPE}; false otherwise
	 */
	private static boolean isSingleEscape(final int codePoint) {
		return ReaderMacroFunctionImpl.isSyntaxType(codePoint, SyntaxType.SINGLE_ESCAPE);
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#SINGLE_ESCAPE} based on the current
	 * readtable.
	 *
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#SINGLE_ESCAPE}; false otherwise
	 */
	private static boolean isMultipleEscape(final int codePoint) {
		return ReaderMacroFunctionImpl.isSyntaxType(codePoint, SyntaxType.MULTIPLE_ESCAPE);
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#CONSTITUENT} and a {@link
	 * AttributeType#PACKAGEMARKER} based on the current readtable.
	 *
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#CONSTITUENT} and a {@link
	 * AttributeType#PACKAGEMARKER}; false otherwise
	 */
	private static boolean isPackageMarker(final int codePoint) {
		return ReaderMacroFunctionImpl.isSyntaxType(codePoint, SyntaxType.CONSTITUENT)
				&& ReaderMacroFunctionImpl.isAttributeType(codePoint, AttributeType.PACKAGEMARKER);
	}

	/**
	 * Transforms the provided {@code currentToken} value into it's correct case based on properties of the current
	 * readtable.
	 *
	 * @param currentToken
	 * 		the token to transform to the proper case
	 *
	 * @return the transformed token with the correct case
	 */
	private static int getTokenWithCase(final int currentToken) {
		final ReadtableCase readtableCase = ReaderVariables.READTABLE.getValue().getReadtableCase();

		int properCaseToken = currentToken;
		switch (readtableCase) {
			case UPCASE:
				properCaseToken = Character.toUpperCase(currentToken);
				break;
			case DOWNCASE:
				properCaseToken = Character.toLowerCase(currentToken);
				break;
			case INVERT:
				properCaseToken = Character.isUpperCase(currentToken) ? Character.toLowerCase(currentToken) : Character.toUpperCase(currentToken);
				break;
			case PRESERVE:
				properCaseToken = currentToken;
				break;
		}
		return properCaseToken;
	}

	/**
	 * Holds the results of an extended token read operation with the token string, whether or not the token has escape
	 * characters, and whether or not the token contains a package delimiter.
	 */
	static final class ReadExtendedToken {

		/**
		 * The tokenized string value.
		 */
		private final String token;

		/**
		 * Whether or not the {@link #token} contains escape characters.
		 */
		private final boolean hasEscapes;

		/**
		 * Whether or not the {@link #token} contains a package delimiter.
		 */
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
		boolean isHasEscapes() {
			return hasEscapes;
		}

		/**
		 * Getter for {@link #hasPackageDelimiter} property.
		 *
		 * @return {@link #hasPackageDelimiter} property
		 */
		boolean isHasPackageDelimiter() {
			return hasPackageDelimiter;
		}

		@Override
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
