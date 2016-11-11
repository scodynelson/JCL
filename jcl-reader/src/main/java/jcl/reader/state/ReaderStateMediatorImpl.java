/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import java.util.List;

import jcl.lang.FunctionStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.readtable.AttributeType;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.readtable.SyntaxType;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.reader.Reader;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.util.CodePointConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Mediator implementation for {@link Reader} {@link ReaderState} invocations throughout the read process.
 */
@Component
public class ReaderStateMediatorImpl {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ReaderStateMediatorImpl.class);

	/**
	 * {@link NumberTokenAccumulatedReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private NumberTokenAccumulatedReaderState numberTokenAccumulatedReaderState;

	/**
	 * {@link SymbolTokenAccumulatedReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private SymbolTokenAccumulatedReaderState symbolTokenAccumulatedReaderState;

	/**
	 * Step 1 of the Reader Algorithm.
	 * <p>
	 * If at end of file, end-of-file processing is performed as specified in read. Otherwise, one character, x, is read
	 * from the input stream, and dispatched according to the syntax type of x to one of steps 2 to 7.
	 * </p>
	 */
	public LispStruct read(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		final ReadPeekResult readResult = inputStreamStruct.readChar(isEofErrorP, eofValue, true);
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

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", token);
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
	 */
	public LispStruct readIllegalCharacter(final TokenBuilder tokenBuilder) {

		if (!tokenBuilder.isEofErrorP()) {
			return tokenBuilder.getEofValue();
		}

		final ReadPeekResult readResult = tokenBuilder.getPreviousReadResult();

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
	 */
	public LispStruct readWhitespace(final TokenBuilder tokenBuilder) {
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
	 */
	public LispStruct readMacroCharacter(final TokenBuilder tokenBuilder) {

		// NOTE: This will throw errors when it reaches an EOF
		final ReadPeekResult readResult = tokenBuilder.getPreviousReadResult();
		final int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final FunctionStruct readerMacroFunction = readtable.getMacroCharacter(codePoint);

		if (readerMacroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for character: " + codePoint + '.');
		}

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		final LispStruct token = readerMacroFunction.apply(
				inputStreamStruct,
				LispStructFactory.toCharacter(codePoint),
				NILStruct.INSTANCE
		);

		if (token == null) {
			return read(tokenBuilder);
		} else {
			return token;
		}
	}

	/**
	 * Step 6 of the Reader Algorithm.
	 * <p>
	 * If x is a multiple escape character then a token (initially containing no characters) is begun and step 9 is
	 * entered.
	 * </p>
	 */
	public LispStruct readSingleEscape(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		final ReadPeekResult readResult = inputStreamStruct.readChar(isEofErrorP, eofValue, true);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			return readIllegalCharacter(tokenBuilder);
		}

		final int codePoint = readResult.getResult();
		tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

		return readEvenMultipleEscape(tokenBuilder);
	}

	public LispStruct readMultipleEscape(final TokenBuilder tokenBuilder) {
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
	 */
	public LispStruct readConstituent(final TokenBuilder tokenBuilder) {

		final ReadPeekResult readResult = tokenBuilder.getPreviousReadResult();
		// This 'codePoint' will not be 'null'. We check for EOFs after each 'read'.
		int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final ReadtableCase readtableCase = readtable.getReadtableCase();

		final IntegerStruct readBase = ReaderVariables.READ_BASE.getVariableValue();
		final AttributeType attributeType = readtable.getAttributeType(codePoint, readBase);

		codePoint = ReaderState.getProperCaseForCodePoint(codePoint, attributeType, readtableCase);
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
	 */
	public LispStruct readEvenMultipleEscape(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		ReadPeekResult readResult = inputStreamStruct.readChar(isEofErrorP, eofValue, true);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			final boolean isMultiEscapedToken = tokenBuilder.isMultiEscapedToken();
			if (isMultiEscapedToken) {
				return symbolTokenAccumulatedReaderState.process(tokenBuilder);
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

			readResult = inputStreamStruct.readChar(isEofErrorP, eofValue, true);
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
				return symbolTokenAccumulatedReaderState.process(tokenBuilder);
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
	 */
	public LispStruct readOddMultipleEscape(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		ReadPeekResult readResult = inputStreamStruct.readChar(isEofErrorP, eofValue, true);
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

			readResult = inputStreamStruct.readChar(isEofErrorP, eofValue, true);
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
	 */
	public LispStruct readTokenAccumulated(final TokenBuilder tokenBuilder) {

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return null;
		}

		final List<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final String tokenString = ReaderState.convertTokenAttributesToString(tokenAttributes);
		if (".".equals(tokenString)) {
			throw new ReaderErrorException("Dot context error in '.'");
		}

		return numberTokenAccumulatedReaderState.process(tokenBuilder);
	}
}
