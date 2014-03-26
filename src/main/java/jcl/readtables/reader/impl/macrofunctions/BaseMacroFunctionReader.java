package jcl.readtables.reader.impl.macrofunctions;

import jcl.LispStruct;
import jcl.readtables.ReadtableStruct;
import jcl.readtables.reader.LispReader;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.syntax.AttributeType;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;

public class BaseMacroFunctionReader implements LispReader {

	protected final StateReader stateReader;

	public BaseMacroFunctionReader(final StateReader stateReader) {
		this.stateReader = stateReader;
	}

	@Override
	public LispStruct read() {
		return stateReader.read();
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return stateReader.read(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadResult readChar() {
		return stateReader.readChar();
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return stateReader.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public void unreadChar(final int codePoint) {
		stateReader.unreadChar(codePoint);
	}

	//***************//
	//** UTILITIES **//
	//***************//

	protected static boolean isAttributeType(final StateReader stateReader, final int codePoint, final AttributeType... attributeTypes) {

		final ReadtableStruct readtable = stateReader.getReadtable();

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (readtable.getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}

	protected static boolean isSyntaxType(final StateReader stateReader, final int codePoint, final SyntaxType... syntaxTypes) {

		final ReadtableStruct readtable = stateReader.getReadtable();

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (readtable.getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}
}
