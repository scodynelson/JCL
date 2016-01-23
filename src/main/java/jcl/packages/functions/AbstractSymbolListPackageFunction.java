/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.types.ListType;
import jcl.types.SymbolType;

/**
 * Abstract {@link FunctionStruct} implementation for package functions that take symbol objects. This {@link
 * FunctionStruct} also has an optional package parameter value.
 */
abstract class AbstractSymbolListPackageFunction extends AbstractOptionalPackageFunction {

	/**
	 * Protected constructor passing the provided {@code documentation} string to the super constructor.
	 *
	 * @param documentation
	 * 		the documentation string
	 */
	protected AbstractSymbolListPackageFunction(final String documentation) {
		super(documentation);
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} list of symbol objects for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} list of symbol objects
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SYMBOLS").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Application method for the package function that gets the symbol objects and applies the {@link Function} from
	 * the {@link #symbolListFunction()} function.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return {@link TStruct#INSTANCE}
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Symbols", ListType.INSTANCE, SymbolType.INSTANCE);

		final PackageStruct aPackage = getPackage(lispStructs);

		final SymbolStruct[] realSymbolArray;
		if (lispStruct instanceof ListStruct) {
			final List<LispStruct> symbols = ((ListStruct) lispStruct).getAsJavaList();
			final List<SymbolStruct> realSymbols = new ArrayList<>(symbols.size());
			for (final LispStruct theSymbol : symbols) {
				validator.validateTypes(theSymbol, functionName(), "Symbol", SymbolType.INSTANCE);
				realSymbols.add((SymbolStruct) theSymbol);
			}
			realSymbolArray = realSymbols.toArray(new SymbolStruct[realSymbols.size()]);
		} else if (lispStruct instanceof SymbolStruct) {
			realSymbolArray = new SymbolStruct[1];
			realSymbolArray[0] = (SymbolStruct) lispStruct;
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		symbolListFunction().accept(aPackage, realSymbolArray);

		return TStruct.INSTANCE;
	}

	/**
	 * Abstract method to return a {@link BiConsumer} function that consumes a {@link PackageStruct} and an array of
	 * {@link SymbolStruct}s.
	 *
	 * @return a {@link BiConsumer} function that consumes a {@link PackageStruct} and an array of {@link SymbolStruct}s
	 */
	protected abstract BiConsumer<PackageStruct, SymbolStruct[]> symbolListFunction();
}
