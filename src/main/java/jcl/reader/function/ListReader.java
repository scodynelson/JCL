package jcl.reader.function;

import jcl.LispStruct;
import jcl.reader.impl.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.Variable;
import jcl.syntax.CharacterConstants;

import java.util.ArrayList;
import java.util.List;

import static jcl.reader.function.FunctionReaderUtils.getNextCodePoint;
import static jcl.reader.function.FunctionReaderUtils.isWhitespace;
import static jcl.reader.function.FunctionReaderUtils.isWhitespaceOrTerminating;

public class ListReader {

	private final Reader reader;

	public ListReader(final Reader reader) {
		this.reader = reader;
	}

	public ListStruct readList() {

		final List<LispStruct> theList = new ArrayList<>();

		boolean isDotted = false;

		int codePoint = flushWhitespace();
		while (codePoint != CharacterConstants.RIGHT_PARENTHESIS) {

			if (codePoint == CharacterConstants.FULL_STOP) {
				final int nextCodePoint = getNextCodePoint(reader);

				if (isWhitespaceOrTerminating(reader, nextCodePoint)) {
					if (theList.isEmpty()) {
						if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
							return null;
						} else {
							throw new ReaderErrorException("Nothing appears before . in list.");
						}
					}

					isDotted = true;
					processAfterDot(theList, nextCodePoint);
					break;
				} else {
					reader.unreadChar(nextCodePoint);
				}
			}
			reader.unreadChar(codePoint);

			final LispStruct lispStruct = reader.read();
			if (lispStruct != null) {
				theList.add(lispStruct);
			}

			codePoint = flushWhitespace();
		}

		return isDotted ? ListStruct.buildDottedList(theList) : ListStruct.buildProperList(theList);
	}

	private void processAfterDot(final List<LispStruct> theList, final int codePoint) {
		int firstCodePoint = codePoint;
		if (isWhitespace(reader, codePoint)) {
			firstCodePoint = flushWhitespace();
		}

		LispStruct lispStruct = null;

		while (lispStruct == null) {

			if (firstCodePoint == CharacterConstants.RIGHT_PARENTHESIS) {
				throw new ReaderErrorException("Nothing appears after . in list.");
			}
			reader.unreadChar(codePoint);

			// NOTE: This will throw errors when it reaches an EOF
			lispStruct = reader.read();
			firstCodePoint = flushWhitespace();
		}
		theList.add(lispStruct);

		while (firstCodePoint != CharacterConstants.RIGHT_PARENTHESIS) {
			reader.unreadChar(firstCodePoint);

			// NOTE: This will throw errors when it reaches an EOF
			lispStruct = reader.read();
			if (lispStruct != null) {
				throw new ReaderErrorException("More than one object follows . in list.");
			}

			firstCodePoint = flushWhitespace();
		}
	}

	private int flushWhitespace() {

		int codePoint = getNextCodePoint(reader);
		while (isWhitespace(reader, codePoint)) {
			codePoint = getNextCodePoint(reader);
		}
		return codePoint;
	}
}