package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.streams.ReadResult;
import jcl.structs.symbols.variables.Variable;

import java.util.ArrayList;
import java.util.List;

/**
 * Reader Macro Function for handling the reading of lists, properly handling the encountering of a '.' character
 * according to list parsing rules of where in the list the '.' can appear and how many items must both precede and
 * follow it.
 */
abstract class ListReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Reads in an returns a properly parsed {@link ListStruct}, handling whitespaces and '.' characters. If a '.'
	 * character is encountered, proper parsing rules will be applied and a dotted list will be returned.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the {@link ListStruct}
	 *
	 * @return the properly parsed {@link ListStruct}
	 */
	static ListStruct readList(final Reader reader) {
		final List<LispStruct> currentTokenList = new ArrayList<>();

		boolean isDottedList = false;

		int codePoint = flushWhitespace(reader);
		while (codePoint != CharacterConstants.RIGHT_PARENTHESIS) {

			if (codePoint == CharacterConstants.FULL_STOP) {
				isDottedList = processDot(reader, currentTokenList);
				if (isDottedList) {
					break;
				}
			}

			reader.unreadChar(codePoint);

			final LispStruct lispStruct = reader.read();
			if (lispStruct != null) {
				currentTokenList.add(lispStruct);
			}

			codePoint = flushWhitespace(reader);
		}

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		return isDottedList ? ListStruct.buildDottedList(currentTokenList) : ListStruct.buildProperList(currentTokenList);
	}

	/**
	 * Processes the '.' character when encountered in the reader, determining if the '.' was a lone '.' character or
	 * part of a symbol. If followed by either a whitespace or a 'terminating' character, it ensures that there is at
	 * least one element that appears before the '.' in the currently parsed list of tokens.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in and process tokens following the '.' character
	 * @param currentTokenList
	 * 		the list of currently read and parsed {@link LispStruct} tokens
	 *
	 * @return true if the list to be created post processing of the '.' character is indeed a dotted list; false
	 * otherwise
	 */
	private static boolean processDot(final Reader reader, final List<LispStruct> currentTokenList) {

		boolean isDotted = false;

		// NOTE: This will throw errors when it reaches an EOF
		final ReadResult readResult = reader.readChar();
		final int nextCodePoint = readResult.getResult();

		if (isWhitespaceOrTerminating(reader, nextCodePoint)) {
			if (currentTokenList.isEmpty()) {
				throw new ReaderErrorException("Nothing appears before . in list.");
			}

			isDotted = true;
			processAfterDot(reader, currentTokenList, nextCodePoint);
		} else {
			reader.unreadChar(nextCodePoint);
		}
		return isDotted;
	}

	/**
	 * Processes the token(s) following '.' in the list. Determines how many items follow the '.' and throws a {@link
	 * ReaderErrorException} if either no items or more than one item are there.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the next token(s) following the '.' in the list
	 * @param currentTokenList
	 * 		the current list of read {@link LispStruct}s in the list
	 * @param codePoint
	 * 		the next character code point following the '.' (at this point, either a whitespace or terminating character)
	 */
	private static void processAfterDot(final Reader reader, final List<LispStruct> currentTokenList, final int codePoint) {
		int firstCodePoint = codePoint;
		if (isWhitespace(reader, codePoint)) {
			firstCodePoint = flushWhitespace(reader);
		}

		LispStruct lispStruct = null;

		while (lispStruct == null) {

			if (firstCodePoint == CharacterConstants.RIGHT_PARENTHESIS) {
				throw new ReaderErrorException("Nothing appears after . in list.");
			}
			reader.unreadChar(codePoint);

			// NOTE: This will throw errors when it reaches an EOF
			lispStruct = reader.read();
			firstCodePoint = flushWhitespace(reader);
		}
		currentTokenList.add(lispStruct);

		while (firstCodePoint != CharacterConstants.RIGHT_PARENTHESIS) {
			reader.unreadChar(firstCodePoint);

			// NOTE: This will throw errors when it reaches an EOF
			lispStruct = reader.read();
			if (lispStruct != null) {
				throw new ReaderErrorException("More than one object follows . in list: " + lispStruct.printStruct());
			}

			firstCodePoint = flushWhitespace(reader);
		}
	}

	/**
	 * Gets the next code point value that is not a 'whitespace' character.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the next character tokens until a non-whitespace character is found
	 *
	 * @return the next code point value that is not a 'whitespace' character
	 */
	private static int flushWhitespace(final Reader reader) {

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = reader.readChar();
		int codePoint = readResult.getResult();
		while (isWhitespace(reader, codePoint)) {
			readResult = reader.readChar();
			codePoint = readResult.getResult();
		}
		return codePoint;
	}
}
