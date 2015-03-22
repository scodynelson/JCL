/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Reader Macro Function for handling the reading of lists, properly handling the encountering of a '.' character
 * according to list parsing rules of where in the list the '.' can appear and how many items must both precede and
 * follow it.
 */
@Component
final class ListReaderMacroFunction implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5022365174547071098L;

	/**
	 * {@link Autowired} {@link Printer} used for printing elements and structures to the output stream.
	 */
	@Autowired
	private Printer printer;

	/**
	 * Reads in an returns a properly parsed {@link ListStruct}, handling whitespaces and '.' characters. If a '.'
	 * character is encountered, proper parsing rules will be applied and a dotted list will be returned.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the {@link ListStruct}
	 *
	 * @return the properly parsed {@link ListStruct}
	 */
	ListStruct readList(final Reader reader) {
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

			final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
			if (token != null) {
				currentTokenList.add(token);
			}

			codePoint = flushWhitespace(reader);
		}

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return NullStruct.INSTANCE;
		}

		if (currentTokenList.isEmpty()) {
			return NullStruct.INSTANCE;
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
	private boolean processDot(final Reader reader, final List<LispStruct> currentTokenList) {

		boolean isDotted = false;

		// NOTE: This will throw errors when it reaches an EOF
		final ReadPeekResult readResult = reader.readChar(true, NullStruct.INSTANCE, false);
		final int nextCodePoint = readResult.getResult();

		if (ReaderMacroFunctionUtil.isWhitespaceOrTerminating(nextCodePoint)) {
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
	private void processAfterDot(final Reader reader, final List<LispStruct> currentTokenList, final int codePoint) {
		int firstCodePoint = codePoint;
		if (ReaderMacroFunctionUtil.isWhitespace(codePoint)) {
			firstCodePoint = flushWhitespace(reader);
		}

		LispStruct token = null;

		while (token == null) {

			if (firstCodePoint == CharacterConstants.RIGHT_PARENTHESIS) {
				throw new ReaderErrorException("Nothing appears after . in list.");
			}
			reader.unreadChar(codePoint);

			// NOTE: This will throw errors when it reaches an EOF
			token = reader.read(true, NullStruct.INSTANCE, true);
			firstCodePoint = flushWhitespace(reader);
		}
		currentTokenList.add(token);

		while (firstCodePoint != CharacterConstants.RIGHT_PARENTHESIS) {
			reader.unreadChar(firstCodePoint);

			// NOTE: This will throw errors when it reaches an EOF
			token = reader.read(true, NullStruct.INSTANCE, true);
			if (token != null) {
				final String printedToken = printer.print(token);
				throw new ReaderErrorException("More than one object follows . in list: " + printedToken);
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
		ReadPeekResult readResult = reader.readChar(true, NullStruct.INSTANCE, false);
		int codePoint = readResult.getResult();
		while (ReaderMacroFunctionUtil.isWhitespace(codePoint)) {
			readResult = reader.readChar(true, NullStruct.INSTANCE, false);
			codePoint = readResult.getResult();
		}
		return codePoint;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(printer)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ListReaderMacroFunction rhs = (ListReaderMacroFunction) obj;
		return new EqualsBuilder().append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
