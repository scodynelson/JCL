package jcl.readtables.reader.impl.macrofunctions;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.readtables.reader.LispReader;
import jcl.syntax.CharacterConstants;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;
import jcl.variables.ReadSuppressVariable;

import java.util.ArrayList;
import java.util.List;

public class ListMacroFunctionReader {

	private final LispReader reader;

	public ListMacroFunctionReader(final LispReader reader) {
		this.reader = reader;
	}

	public ListStruct readList() {

		final List<LispStruct> theList = new ArrayList<>();

		int codePoint = flushWhitespace();

		while (codePoint != CharacterConstants.RIGHT_PARENTHESIS) {

			if (codePoint == CharacterConstants.FULL_STOP) {

				int nextCodePoint = reader.readChar().getResult();

				if (MacroFunctionReaderUtils.isSyntaxType(reader, nextCodePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING)) {
					if (theList.isEmpty()) {
						if (ReadSuppressVariable.INSTANCE.getValue()) {
							return null;
						} else {
							throw new ReaderErrorException("Nothing appears before . in list.");
						}
					}

					if (MacroFunctionReaderUtils.isSyntaxType(reader, nextCodePoint, SyntaxType.WHITESPACE)) {
						nextCodePoint = flushWhitespace();
					}

					final LispStruct lispStruct = readAfterDot(nextCodePoint);
					theList.add(lispStruct);
					return ListStruct.buildDottedList(theList);
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

		return ListStruct.buildProperList(theList);
	}

	private LispStruct readAfterDot(final int firstCodePoint) {

		LispStruct lispStruct;

		int codePoint = firstCodePoint;
		while (true) {

			if (codePoint == CharacterConstants.RIGHT_PARENTHESIS) {
				throw new ReaderErrorException("Nothing appears after . in list.");
			}
			reader.unreadChar(codePoint);

			lispStruct = reader.read();
			if (lispStruct != null) {
				break;
			}

			codePoint = flushWhitespace();
		}

		int nextCodePoint = flushWhitespace();
		while (nextCodePoint != CharacterConstants.RIGHT_PARENTHESIS) {
			reader.unreadChar(nextCodePoint);

			lispStruct = reader.read();
			if (lispStruct != null) {
				throw new ReaderErrorException("More than one object follows . in list.");
			}

			nextCodePoint = flushWhitespace();
		}

		return lispStruct;
	}

	private int flushWhitespace() {

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = reader.readChar();
		int codePoint = readResult.getResult();

		if (MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE)) {
			readResult = reader.readChar();
			codePoint = readResult.getResult();
		}

		return codePoint;
	}
}
