/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.util.ClassUtils;

/**
 * Abstract {@link FunctionStruct} implementation for package functions that take symbol objects. This {@link
 * FunctionStruct} also has an optional package parameter value.
 */
abstract class AbstractSymbolListPackageFunction extends CommonLispBuiltInFunctionStruct {

	protected AbstractSymbolListPackageFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("SYMBOLS")
		                .optionalParameter("PACKAGE").withInitialValue(PackageVariables.PACKAGE.getVariableValue())
		);
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
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument("SYMBOLS");
		final PackageStruct aPackage = arguments.getOptionalArgument("PACKAGE", PackageStruct.class);

		final SymbolStruct[] realSymbolArray;
		if (lispStruct instanceof ListStruct) {
			final ListStruct symbols = (ListStruct) lispStruct;
			final List<SymbolStruct> realSymbols = new ArrayList<>();
			for (final LispStruct theSymbol : symbols) {
				realSymbols.add(ClassUtils.convert(SymbolStruct.class, theSymbol));
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
