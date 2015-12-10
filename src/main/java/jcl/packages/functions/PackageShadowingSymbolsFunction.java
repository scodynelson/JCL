/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-shadowing-symbols}.
 */
@Component
public final class PackageShadowingSymbolsFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 1263271762013032850L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageShadowingSymbolsFunction() {
		super("Returns a list of symbols that have been declared as shadowing symbols in package by shadow or shadowing-import.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final PackageStruct aPackage = findPackage(lispStruct);

		final Collection<SymbolStruct<?>> shadowingSymbols = aPackage.getShadowingSymbols().values();
		return ListStruct.buildProperList(new ArrayList<>(shadowingSymbols));
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code package-shadowing-symbols} as a string.
	 *
	 * @return the function name {@code package-shadowing-symbols} as a string
	 */
	@Override
	protected String functionName() {
		return "PACKAGE-SHADOWING-SYMBOLS";
	}
}
