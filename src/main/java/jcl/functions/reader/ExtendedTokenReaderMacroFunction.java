/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.AttributeType;
import jcl.lang.InputStreamStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadCharResult;
import jcl.lang.ReadtableCase;
import jcl.lang.SyntaxType;
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;

/**
 * Reader Macro Function for handling the reading of extended characters, handling proper character casing and
 * escaping.
 */
@UtilityClass
final class ExtendedTokenReaderMacroFunction {

	/**
	 * Reads in and returns an extended token character containing the token, whether or not the token has escape
	 * characters, and whether or not the token has a package delimiter.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read the token from
	 * @param isEscaped
	 * 		whether or not characters read in by the JCL Reader should be considered escaped or not
	 *
	 * @return a {@link ReadExtendedToken} object containing the extended token information as read
	 */
	static ReadExtendedToken readExtendedToken(final InputStreamStruct inputStreamStruct, final boolean isEscaped) {

		final StringBuilder stringBuilder = new StringBuilder();

		ReadCharResult readResult = readToken(inputStreamStruct, false, stringBuilder, isEscaped);

		if (isEscaped) {
			readResult = readToken(inputStreamStruct, false, stringBuilder, true);
		}

		boolean hasEscapes = false;
		boolean hasPackageDelimiter = false;

		while (!readResult.isEof()) {

			final int codePoint = readResult.getResult();
			if (ReaderMacroFunctionUtil.isWhitespaceOrTerminating(codePoint)) {
				inputStreamStruct.unreadChar(codePoint);

				// Makes sure to remove the last character read from the builder
				stringBuilder.deleteCharAt(stringBuilder.length() - 1);
				break;
			}

			if (isSingleEscape(codePoint)) {
				readSingleEscape(inputStreamStruct, stringBuilder);
				hasEscapes = true;
			} else if (isMultipleEscape(codePoint)) {
				readMultipleEscape(inputStreamStruct, stringBuilder);
				hasEscapes = true;
			}

			if (!hasPackageDelimiter) {
				hasPackageDelimiter = isPackageMarker(codePoint);
			}

			readResult = readToken(inputStreamStruct, false, stringBuilder, isEscaped);
		}

		return new ReadExtendedToken(stringBuilder.toString(), hasEscapes, hasPackageDelimiter);
	}

	/**
	 * Reads in the next token considering a 'single escape' character code point.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} used when reading  the next token character
	 * @param stringBuilder
	 * 		the {@link StringBuilder} to append the next token character
	 */
	private static void readSingleEscape(final InputStreamStruct inputStreamStruct, final StringBuilder stringBuilder) {
		readToken(inputStreamStruct, true, stringBuilder, true);
	}

	/**
	 * Reads in the next token considering a 'multiple escape' character code point. This is used in reading through
	 * comments specifically so that comments are properly escaped, but the next token is considered correctly.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} used when read the next token character(s)
	 * @param stringBuilder
	 * 		the {@link StringBuilder} to append the next token character(s)
	 */
	private static void readMultipleEscape(final InputStreamStruct inputStreamStruct,
	                                       final StringBuilder stringBuilder) {

		ReadCharResult tempReadResult = inputStreamStruct.readChar(true, null);
		int tempCodePoint = tempReadResult.getResult();

		while (!isMultipleEscape(tempCodePoint)) {

			if (isSingleEscape(tempCodePoint)) {
				// NOTE: The following comes first so we build the token right
				appendToken(tempReadResult, stringBuilder, false);
				readSingleEscape(inputStreamStruct, stringBuilder);
			} else {
				appendToken(tempReadResult, stringBuilder, true);
			}

			tempReadResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE);
			tempCodePoint = tempReadResult.getResult();
		}
		appendToken(tempReadResult, stringBuilder, false);
	}

	/**
	 * Reads the next token character from the provided {@link InputStreamStruct}, using the provided {@code eofErrorP}
	 * to determine how the token character should be read. Then it appends the resulting {@link ReadCharResult} to the
	 * provided {@link StringBuilder} using the provided {@code isEscaped} value in determining the appropriate case of
	 * the token character to append.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read the next token character from
	 * @param eofErrorP
	 * 		whether or not the reader will fail when an End-Of-File is reached
	 * @param stringBuilder
	 * 		the {@link StringBuilder} to append the next token to
	 * @param isEscaped
	 * 		whether or not to attempt to modify the case of the read token
	 *
	 * @return the resulting {@link ReadCharResult} of the read operation performed by the {@link InputStreamStruct}
	 */
	private static ReadCharResult readToken(final InputStreamStruct inputStreamStruct,
	                                        final boolean eofErrorP,
	                                        final StringBuilder stringBuilder,
	                                        final boolean isEscaped) {
		final ReadCharResult readResult = inputStreamStruct.readChar(eofErrorP, NILStruct.INSTANCE);
		appendToken(readResult, stringBuilder, isEscaped);
		return readResult;
	}

	/**
	 * Appends the token result in the provided {@link ReadCharResult} to the provided {@link StringBuilder}, making to
	 * to properly case the token based on the provided {@code isEscaped}.
	 *
	 * @param readResult
	 * 		the {@link ReadCharResult} containing the token result to append
	 * @param stringBuilder
	 * 		the {@link StringBuilder} to append the next token to
	 * @param isEscaped
	 * 		whether or not to attempt to modify the case of the read token
	 */
	private static void appendToken(final ReadCharResult readResult,
	                                final StringBuilder stringBuilder,
	                                final boolean isEscaped) {
		if (!readResult.isEof()) {
			int codePoint = readResult.getResult();
			if (!isEscaped) {
				codePoint = getCodePointWithCase(codePoint);
			}
			stringBuilder.appendCodePoint(codePoint);
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
		return ReaderMacroFunctionUtil.isSyntaxType(codePoint, SyntaxType.SINGLE_ESCAPE);
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
		return ReaderMacroFunctionUtil.isSyntaxType(codePoint, SyntaxType.MULTIPLE_ESCAPE);
	}

	/**
	 * Determines if the provided {@code codePoint} is a {@link SyntaxType#CONSTITUENT} and a
	 * {@link AttributeType#PACKAGEMARKER} based on the current readtable.
	 *
	 * @param codePoint
	 * 		the character code point to verify {@link SyntaxType} for
	 *
	 * @return true if the provided {@code codePoint} is a {@link SyntaxType#CONSTITUENT} and a
	 * {@link AttributeType#PACKAGEMARKER}; false otherwise
	 */
	private static boolean isPackageMarker(final int codePoint) {
		return ReaderMacroFunctionUtil.isSyntaxType(codePoint, SyntaxType.CONSTITUENT)
				&& ReaderMacroFunctionUtil.isAttributeType(codePoint, AttributeType.PACKAGEMARKER);
	}

	/**
	 * Transforms the provided {@code codePoint} value into it's correct case based on properties of the current
	 * readtable.
	 *
	 * @param codePoint
	 * 		the code point to transform to the proper case
	 *
	 * @return the transformed token with the correct case
	 */
	private static int getCodePointWithCase(final int codePoint) {
		final ReadtableCase readtableCase = CommonLispSymbols.READTABLE_VAR.getVariableValue().getReadtableCase();

		int properCaseCodePoint = codePoint;
		switch (readtableCase) {
			case UPCASE:
				properCaseCodePoint = Character.toUpperCase(codePoint);
				break;
			case DOWNCASE:
				properCaseCodePoint = Character.toLowerCase(codePoint);
				break;
			case INVERT:
				properCaseCodePoint = Character.isUpperCase(codePoint)
				                      ? Character.toLowerCase(codePoint)
				                      : Character.toUpperCase(codePoint);
				break;
			case PRESERVE:
				properCaseCodePoint = codePoint;
				break;
		}
		return properCaseCodePoint;
	}

	/**
	 * Holds the results of an extended token read operation with the token string, whether or not the token has escape
	 * characters, and whether or not the token contains a package delimiter.
	 */
	static final class ReadExtendedToken {

		/**
		 * The tokenized string value.
		 */
		private final String tokenString;

		/**
		 * Whether or not the {@link #tokenString} contains escape characters.
		 */
		private final boolean hasEscapes;

		/**
		 * Whether or not the {@link #tokenString} contains a package delimiter.
		 */
		private final boolean hasPackageDelimiter;

		/**
		 * Private constructor.
		 *
		 * @param tokenString
		 * 		the token string
		 * @param hasEscapes
		 * 		whether or not the token has escape characters
		 * @param hasPackageDelimiter
		 * 		whether or not the token contains a package delimiter
		 */
		private ReadExtendedToken(final String tokenString, final boolean hasEscapes,
		                          final boolean hasPackageDelimiter) {
			this.tokenString = tokenString;
			this.hasEscapes = hasEscapes;
			this.hasPackageDelimiter = hasPackageDelimiter;
		}

		/**
		 * Getter for {@link #tokenString} property.
		 *
		 * @return {@link #tokenString} property
		 */
		String getTokenString() {
			return tokenString;
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
	}
}
