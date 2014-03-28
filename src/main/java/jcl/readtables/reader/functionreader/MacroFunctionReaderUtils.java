package jcl.readtables.reader.functionreader;

import jcl.readtables.reader.Reader;
import jcl.syntax.AttributeType;
import jcl.syntax.SyntaxType;

class MacroFunctionReaderUtils {

	static boolean isAttributeType(final Reader reader, final int codePoint, final AttributeType... attributeTypes) {

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (reader.getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}

	static boolean isSyntaxType(final Reader reader, final int codePoint, final SyntaxType... syntaxTypes) {

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (reader.getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}
}
