/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code find-all-symbols}.
 */
@Component
public final class FindAllSymbolsFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4242697651341404961L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public FindAllSymbolsFunction() {
		super("Searches every registered package for symbols that have a name that is the same as string.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} string-designator object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} string-designator object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "SYMBOL-NAME").buildList();
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
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final String name = validator.validateStringDesignator(lispStruct, functionName(), "Symbol Name");

		final List<SymbolStruct> allSymbols = PackageStruct.findAllSymbols(name);
		return ListStruct.buildProperList(allSymbols);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code find-all-symbols} as a string.
	 *
	 * @return the function name {@code find-all-symbols} as a string
	 */
	@Override
	protected String functionName() {
		return "FIND-ALL-SYMBOLS";
	}
}
