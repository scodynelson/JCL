/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.CharacterStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.FunctionStructImpl;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.readtable.ReaderMacroFunction;

/**
 * Abstract implementation definition for all Reader defined macro functions that read character macros based off of a
 * provided {@link Integer} code point.
 */
public abstract class ReaderMacroFunctionImpl extends FunctionStructImpl implements ReaderMacroFunction {

	protected ReaderMacroFunctionImpl() {
		// TODO
		super("Some Documentation");
	}

	private static final SymbolStruct DUMMY_SYMBOL = LispStructFactory.toSymbol("dummySymbol");

	@Override
	public SymbolStruct getFunctionSymbol() {
		// TODO: we can do this better
		return DUMMY_SYMBOL;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {

		final InputStreamStruct stream = (InputStreamStruct) lispStructs[0];

		final CharacterStruct macroCharacter = (CharacterStruct) lispStructs[1];
		final int codePoint = macroCharacter.getCodePoint();

		final Optional<BigInteger> numberArgument;
		if (isDispatch()) {
			final IntegerStruct macroNumberArgument = (IntegerStruct) lispStructs[2];
			// TODO: optimize??
			final BigInteger bigInteger = macroNumberArgument.bigIntegerValue();
			numberArgument = Optional.of(bigInteger);
		} else {
			numberArgument = Optional.empty();
		}

		final ReaderInputStreamStruct readerInputStreamStruct = new ReaderInputStreamStruct(stream);
		return readMacro(readerInputStreamStruct, codePoint, numberArgument);
	}
}
