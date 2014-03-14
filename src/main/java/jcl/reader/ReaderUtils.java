package jcl.reader;

import jcl.syntax.AttributeType;
import jcl.syntax.SyntaxType;
import jcl.structs.ReadtableStruct;

public final class ReaderUtils {

	public static final int EOF = -1;

	private ReaderUtils() {
	}

	public static boolean isEndOfFileCharacter(final Integer character) {
		return (character == null) || (character == EOF);
	}

	public static boolean isAttributeType(final StateReader stateReader, final int codePoint, final AttributeType... attributeTypes) {

		final ReadtableStruct readtable = stateReader.getReadtable();

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (readtable.getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}

	public static boolean isSyntaxType(final StateReader stateReader, final int codePoint, final SyntaxType... syntaxTypes) {

		final ReadtableStruct readtable = stateReader.getReadtable();

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (readtable.getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}
}
