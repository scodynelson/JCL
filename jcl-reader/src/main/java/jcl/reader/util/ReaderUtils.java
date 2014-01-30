package jcl.reader.util;

import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.SyntaxType;
import jcl.structs.readtables.ReadtableStruct;

public final class ReaderUtils {

	public static final int EOF = -1;

	private ReaderUtils() {
	}

	public static boolean isEndOfFileCharacter(final Integer character) {
		return (character == null) || (character == EOF);
	}

	public static boolean isAttributeType(final ReadtableStruct readtable, final int codePoint, final AttributeType... attributeTypes) {

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (readtable.getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}

	public static boolean isSyntaxType(final ReadtableStruct readtable, final int codePoint, final SyntaxType... syntaxTypes) {

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (readtable.getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}
}
