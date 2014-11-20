package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.variables.Variable;

import java.util.ArrayList;
import java.util.List;

abstract class ListReaderMacroFunction extends ReaderMacroFunction {

	protected static ListStruct process(final Reader reader) {
		final List<LispStruct> theList = new ArrayList<>();

		boolean isDottedList = false;

		int codePoint = flushWhitespace(reader);
		while (codePoint != CharacterConstants.RIGHT_PARENTHESIS) {

			if (codePoint == CharacterConstants.FULL_STOP) {
				isDottedList = processDot(reader, theList);
				if (isDottedList) {
					break;
				}
			}

			reader.unreadChar(codePoint);

			final LispStruct lispStruct = reader.read();
			if (lispStruct != null) {
				theList.add(lispStruct);
			}

			codePoint = flushWhitespace(reader);
		}

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		return isDottedList ? ListStruct.buildDottedList(theList) : ListStruct.buildProperList(theList);
	}

	private static boolean processDot(final Reader reader, final List<LispStruct> theList) {

		boolean isDotted = false;

		final int nextCodePoint = getNextCodePoint(reader);

		if (isWhitespaceOrTerminating(reader, nextCodePoint)) {
			if (theList.isEmpty()) {
				throw new ReaderErrorException("Nothing appears before . in list.");
			}

			isDotted = true;
			processAfterDot(reader, theList, nextCodePoint);
		} else {
			reader.unreadChar(nextCodePoint);
		}
		return isDotted;
	}

	private static void processAfterDot(final Reader reader, final List<LispStruct> theList, final int codePoint) {
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
		theList.add(lispStruct);

		while (firstCodePoint != CharacterConstants.RIGHT_PARENTHESIS) {
			reader.unreadChar(firstCodePoint);

			// NOTE: This will throw errors when it reaches an EOF
			lispStruct = reader.read();
			if (lispStruct != null) {
				throw new ReaderErrorException("More than one object follows . in list.");
			}

			firstCodePoint = flushWhitespace(reader);
		}
	}

	private static int flushWhitespace(final Reader reader) {

		int codePoint = getNextCodePoint(reader);
		while (isWhitespace(reader, codePoint)) {
			codePoint = getNextCodePoint(reader);
		}
		return codePoint;
	}
}
