/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code find-all-symbols}.
 */
@Component
public final class FindAllSymbolsFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4242697651341404961L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public FindAllSymbolsFunction() {
		super("Searches every registered package for symbols that have a name that is the same as string.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "NAME").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final String name = getStringFromStringDesignator(lispStruct, "Symbol Name");

		final List<SymbolStruct<?>> allSymbols = PackageStruct.findAllSymbols(name);
		final LispStruct[] allSymbolsArray = allSymbols.toArray(new LispStruct[allSymbols.size()]);
		return ListStruct.buildProperList(allSymbolsArray);
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
