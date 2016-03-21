/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code find-all-symbols}.
 */
@Component
public final class FindAllSymbolsFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "FIND-ALL-SYMBOLS";
	private static final String SYMBOL_NAME_ARGUMENT = "SYMBOL-NAME";

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public FindAllSymbolsFunction() {
		super("Searches every registered package for symbols that have a name that is the same as string.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_NAME_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code find-all-symbols} package function that returns all the {@link SymbolStruct}s
	 * that match the provided string-designator symbol name from every registered {@link PackageStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link SymbolStruct}s that match the provided string-designator symbol name from every registered
	 * {@link PackageStruct}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(SYMBOL_NAME_ARGUMENT);
		final String name = validator.validateStringDesignator(lispStruct, functionName, "Symbol Name");

		final List<SymbolStruct> allSymbols = PackageStruct.findAllSymbols(name);
		return ListStruct.buildProperList(allSymbols);
	}
}
