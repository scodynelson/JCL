package jcl.readtables.reader.impl.macrofunctions;

import jcl.readtables.ReadtableStruct;
import jcl.readtables.reader.LispReader;
import jcl.syntax.AttributeType;
import jcl.syntax.SyntaxType;

class MacroFunctionReaderUtils {

	static boolean isAttributeType(final LispReader reader, final int codePoint, final AttributeType... attributeTypes) {

		final ReadtableStruct readtable = reader.getReadtable();

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (readtable.getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}

	static boolean isSyntaxType(final LispReader reader, final int codePoint, final SyntaxType... syntaxTypes) {

		final ReadtableStruct readtable = reader.getReadtable();

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (readtable.getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}
}
