/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.reader.internal;

import java.util.List;

import jcl.lang.CharacterStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.NumberStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.readtable.AttributeType;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.readtable.SyntaxType;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadCharResult;
import jcl.reader.CommentStruct;
import jcl.util.CodePointConstants;
import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;

/**
 * This interface defines a set of anonymous classes that comprise the states of the Reader state machine as defined in
 * CLtL: Ch 22.1.1 pp 511-515. These states are active objects having a single {@code process} method. Each state
 * returns a State object that is the next state to process. The current Reader instance is passed to each State. The
 * Reader instance contains a reference to the current input Stream. A state processes according to the specification
 * and returns the next state. The states in CLtL are numbered. The following is a correspondence list between the
 * numbered states and the named states in this interface.
 * <ol start=0>
 * <li>ReadState
 * <li>IllegalCharState
 * <li>WhitespaceState
 * <li>MacroCharacterState
 * <li>SingleEscapeState
 * <li>MultipleEscapeState
 * <li>ConstituentState
 * <li>EvenMultiEscapeState
 * <li>OddMultiEscapeState
 * <li>TokenAccumulatedState
 * </ol>
 * For online specifications of these states, goto http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm
 * This site is the Reader Algorithm that is outlined within the CommonLisp HyperSpec (TM).
 */
@Log4j2
@UtilityClass
public class ReaderProcessor {

	public static LispStruct read(final InputStreamStruct inputStreamStruct,
	                              final boolean eofErrorP, final LispStruct eofValue) {
		final TokenBuilder tokenBuilder = new TokenBuilder(inputStreamStruct, eofErrorP, eofValue);
		return read(tokenBuilder);
	}

	/**
	 * Step 1 of the Reader Algorithm.
	 * <p>
	 * If at end of file, end-of-file processing is performed as specified in read. Otherwise, one character, x, is read
	 * from the input stream, and dispatched according to the syntax type of x to one of steps 2 to 7.
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	static LispStruct read(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		final ReadCharResult readResult = inputStreamStruct.readChar(isEofErrorP, eofValue);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			return readIllegalCharacter(tokenBuilder);
		}

		final int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		final LispStruct token;

		if (syntaxType == SyntaxType.WHITESPACE) {
			token = readWhitespace(tokenBuilder);
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			token = readMacroCharacter(tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {
			token = readSingleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			tokenBuilder.setMultiEscapedToken();
			token = readMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.CONSTITUENT) {
			token = readConstituent(tokenBuilder);
		} else {
			token = readIllegalCharacter(tokenBuilder);
		}

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().toJavaPBoolean()) {
			if (log.isDebugEnabled()) {
				log.debug("{} suppressed.", token);
			}
			return NILStruct.INSTANCE;
		}
		return token;
	}

	/**
	 * Step 2 of the Reader Algorithm.
	 * <p>
	 * If x is an invalid character, an error of type reader-error is signaled.
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	private static LispStruct readIllegalCharacter(final TokenBuilder tokenBuilder) {

		if (!tokenBuilder.isEofErrorP()) {
			return tokenBuilder.getEofValue();
		}

		final ReadCharResult readResult = tokenBuilder.getPreviousReadResult();

		if (readResult == null) {
			throw new ReaderErrorException("No token elements could be read.");
		}

		if (readResult.isEof()) {
			throw new ReaderErrorException("End-of-File was encountered.");
		}

		final int codePoint = readResult.getResult();
		if (codePoint != CodePointConstants.EXIT_CHAR) {
			throw new ReaderErrorException("Illegal Character was encountered: " + codePoint);
		}

		return NILStruct.INSTANCE;
	}

	/**
	 * Step 3 of the Reader Algorithm.
	 * <p>
	 * If x is a whitespace[2] character, then it is discarded and step 1 is re-entered.
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	private static LispStruct readWhitespace(final TokenBuilder tokenBuilder) {
		return read(tokenBuilder);
	}

	/**
	 * Step 4 of the Reader Algorithm.
	 * <p>
	 * If x is a terminating or non-terminating macro character then its associated reader macro function is called with
	 * two arguments, the input stream and x.
	 * <p>
	 * The reader macro function may read characters from the input stream; if it does, it will see those characters
	 * following the macro character. The Lisp reader may be invoked recursively from the reader macro function.
	 * </p>
	 * <p>
	 * The reader macro function must not have any side effects other than on the input stream; because of backtracking and
	 * restarting of the read operation, front ends to the Lisp reader (e.g., ``editors'' and ``rubout handlers'') may
	 * cause the reader macro function to be called repeatedly during the reading of a single expression in which x only
	 * appears once.
	 * </p>
	 * <p>
	 * The reader macro function may return zero values or one value. If one value is returned, then that value is returned
	 * as the result of the read operation; the algorithm is done. If zero values are returned, then step 1 is re-entered.
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	private static LispStruct readMacroCharacter(final TokenBuilder tokenBuilder) {

		// NOTE: This will throw errors when it reaches an EOF
		final ReadCharResult readResult = tokenBuilder.getPreviousReadResult();
		final int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final FunctionStruct readerMacroFunction = readtable.getMacroCharacter(codePoint);

		if (readerMacroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for character: " + codePoint + '.');
		}

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		final LispStruct token = readerMacroFunction.apply(
				inputStreamStruct,
				CharacterStruct.toLispCharacter(codePoint),
				NILStruct.INSTANCE
		);

		if (token instanceof CommentStruct) {
			if (log.isTraceEnabled()) {
				log.trace(((CommentStruct) token).getCommentString());
			}
			return read(tokenBuilder);
		} else if (token == null) {
			return read(tokenBuilder);
		} else {
			return token;
		}
	}

	/**
	 * Step 5 of the Reader Algorithm.
	 * <p>
	 * If x is a single escape character then the next character, y, is read, or an error of type end-of-file is signaled
	 * if at the end of file. y is treated as if it is a constituent whose only constituent trait is alphabetic[2]. y is
	 * used to begin a token, and step 8 is entered.
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	private static LispStruct readSingleEscape(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		final ReadCharResult readResult = inputStreamStruct.readChar(isEofErrorP, eofValue);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			return readIllegalCharacter(tokenBuilder);
		}

		final int codePoint = readResult.getResult();
		tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

		return readEvenMultipleEscape(tokenBuilder);
	}

	/**
	 * Step 6 of the Reader Algorithm.
	 * <p>
	 * If x is a multiple escape character then a token (initially containing no characters) is begun and step 9 is
	 * entered.
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	private static LispStruct readMultipleEscape(final TokenBuilder tokenBuilder) {
		return readOddMultipleEscape(tokenBuilder);
	}

	/**
	 * Step 7 of the Reader Algorithm.
	 * <p>
	 * If x is a constituent character, then it begins a token. After the token is read in, it will be interpreted either
	 * as a Lisp object or as being of invalid syntax. If the token represents an object, that object is returned as the
	 * result of the read operation. If the token is of invalid syntax, an error is signaled. If x is a character with
	 * case, it might be replaced with the corresponding character of the opposite case, depending on the readtable case of
	 * the current readtable. X is used to begin a token, and step 8 is entered.
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	private static LispStruct readConstituent(final TokenBuilder tokenBuilder) {

		final ReadCharResult readResult = tokenBuilder.getPreviousReadResult();
		// This 'codePoint' will not be 'null'. We check for EOFs after each 'read'.
		int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final ReadtableCase readtableCase = readtable.getReadtableCase();

		final IntegerStruct readBase = ReaderVariables.READ_BASE.getVariableValue();
		final AttributeType attributeType = readtable.getAttributeType(codePoint, readBase);

		codePoint = getProperCaseForCodePoint(codePoint, attributeType, readtableCase);
		tokenBuilder.addToTokenAttributes(codePoint, attributeType);

		return readEvenMultipleEscape(tokenBuilder);
	}

	/**
	 * Step 8 of the Reader Algorithm.
	 * <p>
	 * At this point a token is being accumulated, and an even number of multiple escape characters have been encountered.
	 * If at end of file, step 10 is entered. Otherwise, a character, y, is read, and one of the following actions is
	 * performed according to its syntax type:
	 * <p>
	 * If y is a constituent or non-terminating macro character:
	 * <p>
	 * -- If y is a character with case, it might be replaced with the corresponding character of the opposite case,
	 * depending on the readtable case of the current readtable.
	 * </p>
	 * <p>
	 * -- Y is appended to the token being built.
	 * </p>
	 * <p>
	 * -- Step 8 is repeated.
	 * </p>
	 * <p>
	 * If y is a single escape character, then the next character, z, is read, or an error of type end-of-file is signaled
	 * if at end of file. Z is treated as if it is a constituent whose only constituent trait is alphabetic. Z is appended
	 * to the token being built, and step 8 is repeated.
	 * </p>
	 * <p>
	 * If y is a multiple escape character, then step 9 is entered.
	 * </p>
	 * <p>
	 * If y is an invalid character, an error of type reader-error is signaled.
	 * </p>
	 * <p>
	 * If y is a terminating macro character, then it terminates the token. First the character y is unread, and then step
	 * 10 is entered.
	 * </p>
	 * <p>
	 * If y is a whitespace character, then it terminates the token. First the character y is unread if appropriate, and
	 * then step 10 is entered.
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	private static LispStruct readEvenMultipleEscape(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		ReadCharResult readResult = inputStreamStruct.readChar(isEofErrorP, eofValue);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			final boolean isMultiEscapedToken = tokenBuilder.isMultiEscapedToken();
			if (isMultiEscapedToken) {
				return readSymbolTokenAccumulated(tokenBuilder);
			} else {
				return readTokenAccumulated(tokenBuilder);
			}
		}

		int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		if ((syntaxType == SyntaxType.CONSTITUENT) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			return readConstituent(tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {

			readResult = inputStreamStruct.readChar(isEofErrorP, eofValue);
			tokenBuilder.setPreviousReadResult(readResult);

			if (readResult.isEof()) {
				return readIllegalCharacter(tokenBuilder);
			}

			codePoint = readResult.getResult();
			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			return readEvenMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			tokenBuilder.setMultiEscapedToken();
			return readOddMultipleEscape(tokenBuilder);
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.WHITESPACE)) {
			// NOTE from CLHS in regarding 'SyntaxType.WHITESPACE' characters:
			//      If a command interpreter takes single-character commands, but occasionally reads an object then if
			//      the whitespace[2] after a symbol is not discarded it might be interpreted as a command some time
			//      later after the symbol had been read.
			inputStreamStruct.unreadChar(codePoint);

			final boolean isMultiEscapedToken = tokenBuilder.isMultiEscapedToken();
			if (isMultiEscapedToken) {
				return readSymbolTokenAccumulated(tokenBuilder);
			} else {
				return readTokenAccumulated(tokenBuilder);
			}
		} else {
			return readIllegalCharacter(tokenBuilder);
		}
	}

	/**
	 * Step 9 of the Reader Algorithm.
	 * <p>
	 * At this point a token is being accumulated, and an odd number of multiple escape characters have been encountered.
	 * If at end of file, an error of type end-of-file is signaled. Otherwise, a character, y, is read, and one of the
	 * following actions is performed according to its syntax type:
	 * <p>
	 * If y is a constituent, macro, or whitespace character, y is treated as a constituent whose only constituent trait is
	 * alphabetic. Y is appended to the token being built, and step 9 is repeated.
	 * </p>
	 * <p>
	 * If y is a single escape character, then the next character, z, is read, or an error of type end-of-file is signaled
	 * if at end of file. Z is treated as a constituent whose only constituent trait is alphabetic. Z is appended to the
	 * token being built, and step 9 is repeated.
	 * </p>
	 * <p>
	 * If y is a multiple escape character, then step 8 is entered.
	 * </p>
	 * <p>
	 * If y is an invalid character, an error of type reader-error is signaled.
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	private static LispStruct readOddMultipleEscape(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		ReadCharResult readResult = inputStreamStruct.readChar(isEofErrorP, eofValue);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			return readIllegalCharacter(tokenBuilder);
		}

		int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		if ((syntaxType == SyntaxType.CONSTITUENT)
				|| (syntaxType == SyntaxType.WHITESPACE)
				|| (syntaxType == SyntaxType.TERMINATING)
				|| (syntaxType == SyntaxType.NON_TERMINATING)) {

			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			return readOddMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {

			readResult = inputStreamStruct.readChar(isEofErrorP, eofValue);
			tokenBuilder.setPreviousReadResult(readResult);

			if (readResult.isEof()) {
				return readIllegalCharacter(tokenBuilder);
			}

			codePoint = readResult.getResult();
			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			return readOddMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			tokenBuilder.setMultiEscapedToken();
			return readEvenMultipleEscape(tokenBuilder);
		} else {
			return readIllegalCharacter(tokenBuilder);
		}
	}

	/**
	 * Step 10 of the Reader Algorithm.
	 * <p>
	 * An entire token has been accumulated. The object represented by the token is returned as the result of the read
	 * operation, or an error of type reader-error is signaled if the token is not of valid syntax.
	 * </p>
	 * <p>
	 * This state is reached when we have accumulated a token, and it needs to be processed into either
	 * 1) Number/PotentialNumber
	 * 2) Symbol
	 * 3) Package with a Symbol
	 * </p>
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the token
	 *
	 * @return the parsed token
	 */
	private static LispStruct readTokenAccumulated(final TokenBuilder tokenBuilder) {

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().toJavaPBoolean()) {
			return null;
		}

		final List<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final String tokenString = convertTokenAttributesToString(tokenAttributes);
		if (".".equals(tokenString)) {
			throw new ReaderErrorException("Dot context error in '.'");
		}

		return readNumberTokenAccumulated(tokenBuilder);
	}

	private static LispStruct readNumberTokenAccumulated(final TokenBuilder tokenBuilder) {
		final NumberStruct numberToken = NumberTokenAccumulatedReaderState.getNumberToken(tokenBuilder);
		if (numberToken == null) {
			return readSymbolTokenAccumulated(tokenBuilder);
		} else {
			return numberToken;
		}
	}

	private static LispStruct readSymbolTokenAccumulated(final TokenBuilder tokenBuilder) {
		final SymbolStruct symbolToken = SymbolTokenAccumulatedReaderState.getSymbolToken(tokenBuilder);
		if (symbolToken == null) {
			return readIllegalCharacter(tokenBuilder);
		} else {
			return symbolToken;
		}
	}

	/*
		Helpers
	 */

	/**
	 * Converts the provided list of {@link TokenAttribute}s to a {@link String}.
	 *
	 * @param tokenAttributes
	 * 		the list of {@link TokenAttribute}s to convert to a {@link String}
	 *
	 * @return the {@link String} produced from the list of {@link TokenAttribute}s
	 */
	static String convertTokenAttributesToString(final List<TokenAttribute> tokenAttributes) {
		if (tokenAttributes.isEmpty()) {
			return "";
		}

		final StringBuilder stringBuilder = new StringBuilder();
		tokenAttributes.stream()
		               .mapToInt(TokenAttribute::getCodePoint)
		               .forEachOrdered(stringBuilder::appendCodePoint);
		return stringBuilder.toString();
	}

	/**
	 * Determines and returns the proper code point value based from the provided {@code codePoint} and using the
	 * provided {@code attributeType} and {@code caseSpec} properties.
	 *
	 * @param codePoint
	 * 		the code point value to properly case
	 * @param attributeType
	 * 		the {@link AttributeType} of the code point value used in determining the proper case value
	 * @param readtableCase
	 * 		the current readtable case used in determines the proper case value
	 *
	 * @return the proper code point value based from the provided {@code codePoint}
	 */
	static int getProperCaseForCodePoint(final int codePoint, final AttributeType attributeType,
	                                     final ReadtableCase readtableCase) {

		final int properCaseCodePoint;
		if (Character.isBmpCodePoint(codePoint)) {
			if ((readtableCase == ReadtableCase.UPCASE) && ((attributeType == AttributeType.ALPHADIGIT) || (attributeType == AttributeType.EXPONENTMARKER))) {
				properCaseCodePoint = Character.toUpperCase(codePoint);
			} else if (readtableCase == ReadtableCase.DOWNCASE) {
				properCaseCodePoint = Character.toLowerCase(codePoint);
			} else if (readtableCase == ReadtableCase.INVERT) {
				if (Character.isUpperCase(codePoint)) {
					properCaseCodePoint = Character.toLowerCase(codePoint);
				} else {
					properCaseCodePoint = Character.toUpperCase(codePoint);
				}
			} else {
				properCaseCodePoint = codePoint;
			}
		} else {
			properCaseCodePoint = codePoint;
		}
		return properCaseCodePoint;
	}

	/**
	 * Determines if the provided list of {@link TokenAttribute}s contains at least one token with an {@link
	 * AttributeType} equal to the provided {@code attributeType} value.
	 *
	 * @param tokenAttributes
	 * 		the list of {@link TokenAttribute}s containing the current tokens
	 * @param attributeType
	 * 		the {@link AttributeType} value to check for existence in the {@code tokenAttributes} list
	 *
	 * @return if the provided list of {@link TokenAttribute}s contains at least one token with an {@link AttributeType}
	 * equal to the provided {@code attributeType} value.
	 */
	static boolean hasAnyAttributeWithAttributeType(final List<TokenAttribute> tokenAttributes,
	                                                final AttributeType attributeType) {
		return tokenAttributes.stream()
		                      .map(TokenAttribute::getAttributeType)
		                      .anyMatch(currentAttributeType -> currentAttributeType == attributeType);
	}

	/**
	 * Determines if the provided list of {@link TokenAttribute}s contains no tokens with an {@link AttributeType}
	 * equal to the provided {@code attributeType} value.
	 *
	 * @param tokenAttributes
	 * 		the list of {@link TokenAttribute}s containing the current tokens
	 * @param attributeType
	 * 		the {@link AttributeType} value to check for existence in the {@code tokenAttributes} list
	 *
	 * @return if the provided list of {@link TokenAttribute}s contains no tokens with an {@link AttributeType} equal to
	 * the provided {@code attributeType} value.
	 */
	static boolean hasNoAttributesWithAttributeType(final List<TokenAttribute> tokenAttributes,
	                                                final AttributeType attributeType) {
		return tokenAttributes.stream()
		                      .map(TokenAttribute::getAttributeType)
		                      .noneMatch(currentAttributeType -> currentAttributeType == attributeType);
	}

	/**
	 * Gets the first occurrence of a token code point with an {@link AttributeType} equal to the provided {@code
	 * attributeType} value in the provided list of {@link TokenAttribute}s. If no token code points have an {@link
	 * AttributeType} that matches the provided {@code attributeType} value, null will be returned.
	 *
	 * @param tokenAttributes
	 * 		the list of {@link TokenAttribute}s containing the current tokens
	 * @param attributeType
	 * 		the {@link AttributeType} value used to locate the first matching token code point in the {@code
	 * 		tokenAttributes} list
	 *
	 * @return the first occurrence of a token code point with an {@link AttributeType} equal to the provided {@code
	 * attributeType} value in the provided list of {@link TokenAttribute}s or null if no such token code point can be
	 * found
	 */
	static Integer getTokenCodePointByAttribute(final List<TokenAttribute> tokenAttributes,
	                                            final AttributeType attributeType) {
		return tokenAttributes.stream()
		                      .filter(currentTokenAttribute -> currentTokenAttribute.getAttributeType() == attributeType)
		                      .map(TokenAttribute::getCodePoint)
		                      .findFirst()
		                      .orElse(null);
	}
}
