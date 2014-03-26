package jcl.readtables.reader.impl.macrofunctions;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.syntax.CharacterConstants;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;
import jcl.variables.ReadSuppressVariable;

import java.util.ArrayList;
import java.util.List;

public class ListMacroFunctionReader extends BaseMacroFunctionReader {

	public ListMacroFunctionReader(final StateReader stateReader) {
		super(stateReader);
	}

	public ListStruct readList() {

		final List<LispStruct> theList = new ArrayList<>();

		int codePoint = flushWhitespace();

		while (codePoint != CharacterConstants.RIGHT_PARENTHESIS) {

			if (codePoint == CharacterConstants.FULL_STOP) {

				int nextCodePoint = stateReader.readChar().getResult();

				if (isSyntaxType(stateReader, nextCodePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING)) {
					if (theList.isEmpty()) {
						if (ReadSuppressVariable.INSTANCE.getValue()) {
							return null;
						} else {
							throw new ReaderErrorException("Nothing appears before . in list.");
						}
					}

					if (isSyntaxType(stateReader, nextCodePoint, SyntaxType.WHITESPACE)) {
						nextCodePoint = flushWhitespace();
					}

					final LispStruct lispStruct = readAfterDot(nextCodePoint);
					theList.add(lispStruct);
					return ListStruct.buildDottedList(theList);
				} else {
					stateReader.unreadChar(nextCodePoint);
				}
			}
			stateReader.unreadChar(codePoint);

			final LispStruct lispStruct = stateReader.read();
			if (lispStruct != null) {
				theList.add(lispStruct);
			}

			codePoint = flushWhitespace();
		}

		return ListStruct.buildProperList(theList);
	}

	private LispStruct readAfterDot(final int firstCodePoint) {

		LispStruct lispStruct;

		int codePoint = firstCodePoint;
		while (true) {

			if (codePoint == CharacterConstants.RIGHT_PARENTHESIS) {
				throw new ReaderErrorException("Nothing appears after . in list.");
			}
			stateReader.unreadChar(codePoint);

			lispStruct = stateReader.read();
			if (lispStruct != null) {
				break;
			}

			codePoint = flushWhitespace();
		}

		int nextCodePoint = flushWhitespace();
		while (nextCodePoint != CharacterConstants.RIGHT_PARENTHESIS) {
			stateReader.unreadChar(nextCodePoint);

			lispStruct = stateReader.read();
			if (lispStruct != null) {
				throw new ReaderErrorException("More than one object follows . in list.");
			}

			nextCodePoint = flushWhitespace();
		}

		return lispStruct;
	}

	private int flushWhitespace() {

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = stateReader.readChar();
		int codePoint = readResult.getResult();

		if (isSyntaxType(stateReader, codePoint, SyntaxType.WHITESPACE)) {
			readResult = stateReader.readChar();
			codePoint = readResult.getResult();
		}

		return codePoint;
	}
}
