/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.classes.StructureClassStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.reader.Reader;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#s' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpSReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	/**
	 * {@link Autowired} {@link ListReaderMacroFunction} used for reading {@link ListStruct}s.
	 */
	private final ListReaderMacroFunction listReaderMacroFunction;

	@Autowired
	public SharpSReaderMacroFunction(final Reader reader, final ListReaderMacroFunction listReaderMacroFunction) {
		super("SHARP-S");
		this.reader = reader;
		this.listReaderMacroFunction = listReaderMacroFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_S, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_S, this);
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_S) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_S);

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
			return NILStruct.INSTANCE;
		}

		final ReadPeekResult readResult = reader.readChar(inputStreamStruct, true, NILStruct.INSTANCE, false);
		final int nextCodePoint = readResult.getResult();
		if (nextCodePoint != CodePointConstants.LEFT_PARENTHESIS) {
			throw new ReaderErrorException("Non-list following #S");
		}

		final ListStruct listToken = listReaderMacroFunction.readList(inputStreamStruct);
		if (listToken == null) {
			throw new ReaderErrorException("Non-list following #S");
		}

		if (listToken.length() == 0) {
			throw new ReaderErrorException("Structure type was not supplied");
		}
		final Iterator<LispStruct> iterator = listToken.iterator();

		final LispStruct structureType = iterator.next();
		if (!(structureType instanceof SymbolStruct)) {
			throw new ReaderErrorException("Structure type is not a symbol: " + structureType);
		}

		final SymbolStruct structureSymbol = (SymbolStruct) structureType;
		final StructureClassStruct structureClass = structureSymbol.getStructureClass();
		if (structureClass == null) {
			throw new ReaderErrorException(structureType + " is not a defined structure type for symbol: " + structureSymbol);
		}

		final SymbolStruct defaultConstructorSymbol = structureClass.getDefaultConstructorSymbol();
		if (defaultConstructorSymbol == null) {
			throw new ReaderErrorException("The " + structureType + " structure does not have a default constructor.");
		}

		final FunctionStruct defaultConstructor = defaultConstructorSymbol.getFunction();
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
