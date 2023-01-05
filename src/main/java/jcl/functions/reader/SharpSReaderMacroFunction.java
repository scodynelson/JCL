/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.lang.FunctionStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadCharResult;
import jcl.lang.SymbolStruct;
import jcl.lang.classes.StructureClassStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.Reader;
import jcl.util.CodePointConstants;

/**
 * Implements the '#s' Lisp reader macro.
 */
public final class SharpSReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpSReaderMacroFunction() {
		super("SHARP-S");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_S) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_S);

		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
			return NILStruct.INSTANCE;
		}

		final ReadCharResult readResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE);
		final int nextCodePoint = readResult.getResult();
		if (nextCodePoint != CodePointConstants.LEFT_PARENTHESIS) {
			throw new ReaderErrorException("Non-list following #S");
		}

		final ListStruct listToken = ListReaderMacroFunction.readList(inputStreamStruct);
		if (listToken == null) {
			throw new ReaderErrorException("Non-list following #S");
		}

		if (listToken.length().zerop().toJavaPBoolean()) {
			throw new ReaderErrorException("Structure type was not supplied");
		}
		final Iterator<LispStruct> iterator = listToken.iterator();

		final LispStruct structureType = iterator.next();
		if (!(structureType instanceof final SymbolStruct structureSymbol)) {
			throw new ReaderErrorException("Structure type is not a symbol: " + structureType);
		}

		final StructureClassStruct structureClass = StructureClassStruct.getStructureClass(structureSymbol, false);
		if (structureClass == null) {
			final String message = structureType + " is not a defined structure type for symbol: " + structureSymbol;
			throw new ReaderErrorException(message);
		}

		final SymbolStruct defaultConstructorSymbol = structureClass.getDefaultConstructorSymbol();
		if (defaultConstructorSymbol == null) {
			throw new ReaderErrorException("The " + structureType + " structure does not have a default constructor.");
		}

		final FunctionStruct defaultConstructor = defaultConstructorSymbol.symbolFunction();
		if (defaultConstructor == null) {
			throw new ReaderErrorException("The " + structureType + " structure default constructor is undefined.");
		}

		final List<LispStruct> arguments = new ArrayList<>();
		iterator.forEachRemaining(arguments::add);

		LispStruct[] argumentsArray = new LispStruct[arguments.size()];
		argumentsArray = arguments.toArray(argumentsArray);
		return defaultConstructor.apply(argumentsArray);
	}
}
