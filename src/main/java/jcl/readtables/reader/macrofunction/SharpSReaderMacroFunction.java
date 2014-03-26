package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.classes.StructureObjectStruct;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.syntax.CharacterConstants;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the '#s' Lisp reader macro.
 */
public class SharpSReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final StateReader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_S) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_S);

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			reader.read();
			return null;
		}

//		final int readChar = reader.readChar();
//		if (readChar != CharacterConstants.LEFT_PARENTHESIS) {
//			throw new ReaderErrorException("Non-list following #S");
//		}

//		final ListStruct listToken = ReaderUtils.readList(reader);
//		if (listToken == null) {
//			throw new ReaderErrorException("Non-list following #S");
//		}

//		final List<LispStruct> lispTokens = listToken.getAsJavaList();
//		if (CollectionUtils.isEmpty(lispTokens)) {
//			throw new ReaderErrorException("Structure type was not supplied");
//		}

//		final LispStruct structureType = lispTokens.get(0);
//		if (!(structureType instanceof SymbolStruct)) {
//			throw new ReaderErrorException("Structure type is not a symbol: " + structureType);
//		}

		// TODO: Find class object from structureType value
//		if (!(classObj instanceof StructureClass)) {
//			throw new ReaderErrorException(structureType + " is not a defined structure type.");
//		}

		// TODO: Get default constructor for the structure
//		if (defCon == null) {
//			throw new ReaderErrorException("The " + structureType + " structure does not have a default constructor.");
//		}

		// TODO: Call constructor to create Structure object

		return new StructureObjectStruct();
	}
}
