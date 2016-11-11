/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.util.ArrayList;
import java.util.List;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.reader.Reader;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Reader Macro Function for handling the reading of lists, properly handling the encountering of a '.' character
 * according to list parsing rules of where in the list the '.' can appear and how many items must both precede and
 * follow it.
 */
@Component
final class ListReaderMacroFunction {

	private final Reader reader;

	@Autowired
	ListReaderMacroFunction(final Reader reader) {
		this.reader = reader;
	}

	/**
	 * Reads in an returns a properly parsed {@link ListStruct}, handling whitespaces and '.' characters. If a '.'
	 * character is encountered, proper parsing rules will be applied and a dotted list will be returned.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read in the {@link ListStruct}
	 *
	 * @return the properly parsed {@link ListStruct}
	 */
	ListStruct readList(final InputStreamStruct inputStreamStruct) {
		final List<LispStruct> currentTokenList = new ArrayList<>();

		boolean isDottedList = false;

		int codePoint = flushWhitespace(inputStreamStruct);
		while (codePoint != CodePointConstants.RIGHT_PARENTHESIS) {

			if (codePoint == CodePointConstants.FULL_STOP) {
				isDottedList = processDot(inputStreamStruct, currentTokenList);
				if (isDottedList) {
					break;
				}
			}

			inputStreamStruct.unreadChar(codePoint);

			final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
			if (token != null) {
				currentTokenList.add(token);
			}

			codePoint = flushWhitespace(inputStreamStruct);
		}

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (currentTokenList.isEmpty()) {
			return NILStruct.INSTANCE;
		}

		return isDottedList ? LispStructFactory.toDottedList(currentTokenList) : LispStructFactory.toProperList(currentTokenList);
	}

	/**
	 * Processes the '.' character when encountered in the reader, determining if the '.' was a lone '.' character or
	 * part of a symbol. If followed by either a whitespace or a 'terminating' character, it ensures that there is at
	 * least one element that appears before the '.' in the currently parsed list of tokens.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read in tokens following the '.' character
	 * @param currentTokenList
	 * 		the list of currently read and parsed {@link LispStruct} tokens
	 *
	 * @return true if the list to be created post processing of the '.' character is indeed a dotted list; false
	 * otherwise
	 */
	private boolean processDot(final InputStreamStruct inputStreamStruct, final List<LispStruct> currentTokenList) {

		boolean isDotted = false;

		// NOTE: This will throw errors when it reaches an EOF
		final ReadPeekResult readResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE, false);
		final int nextCodePoint = readResult.getResult();

		if (ReaderMacroFunctionUtil.isWhitespaceOrTerminating(nextCodePoint)) {
			if (currentTokenList.isEmpty()) {
				throw new ReaderErrorException("Nothing appears before . in list.");
			}

			isDotted = true;
			processAfterDot(inputStreamStruct, currentTokenList, nextCodePoint);
		} else {
			inputStreamStruct.unreadChar(nextCodePoint);
		}
		return isDotted;
	}

	/**
	 * Processes the token(s) following '.' in the list. Determines how many items follow the '.' and throws a {@link
	 * ReaderErrorException} if either no items or more than one item are there.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read in for the token(s) following the '.' in the list
	 * @param currentTokenList
	 * 		the current list of read {@link LispStruct}s in the list
	 * @param codePoint
	 * 		the next character code point following the '.' (at this point, either a whitespace or terminating character)
	 */
	private void processAfterDot(final InputStreamStruct inputStreamStruct, final List<LispStruct> currentTokenList, final int codePoint) {
		int firstCodePoint = codePoint;
		if (ReaderMacroFunctionUtil.isWhitespace(codePoint)) {
			firstCodePoint = flushWhitespace(inputStreamStruct);
		}

		LispStruct token = null;

		while (token == null) {

			if (firstCodePoint == CodePointConstants.RIGHT_PARENTHESIS) {
				throw new ReaderErrorException("Nothing appears after . in list.");
			}
			inputStreamStruct.unreadChar(firstCodePoint);

			// NOTE: This will throw errors when it reaches an EOF
			token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
			firstCodePoint = flushWhitespace(inputStreamStruct);
		}
		currentTokenList.add(token);

		while (firstCodePoint != CodePointConstants.RIGHT_PARENTHESIS) {
			inputStreamStruct.unreadChar(firstCodePoint);

			// NOTE: This will throw errors when it reaches an EOF
			token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
			if (token != null) {
				throw new ReaderErrorException("More than one object follows . in list: " + token);
			}

			firstCodePoint = flushWhitespace(inputStreamStruct);
		}
	}

	/**
	 * Gets the next code point value that is not a 'whitespace' character.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read in the next character tokens until a non-whitespace character is found
	 *
	 * @return the next code point value that is not a 'whitespace' character
	 */
	private static int flushWhitespace(final InputStreamStruct inputStreamStruct) {

		// NOTE: This will throw errors when it reaches an EOF
		ReadPeekResult readResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE, false);
		int codePoint = readResult.getResult();
		while (ReaderMacroFunctionUtil.isWhitespace(codePoint)) {
			readResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE, false);
			codePoint = readResult.getResult();
		}
		return codePoint;
	}
}
