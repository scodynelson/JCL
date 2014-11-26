/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.classes.StructureObjectStruct;

import java.math.BigInteger;

/**
 * Implements the '#s' Lisp reader macro.
 */
public final class SharpSReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpSReaderMacroFunction INSTANCE = new SharpSReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpSReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_S) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_S);

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
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