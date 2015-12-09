/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.types.ListType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class UnexportFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6515511500131565347L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	@Autowired
	private FindPackageFunction findPackageFunction;

	/**
	 * Public constructor passing the documentation string.
	 */
	public UnexportFunction() {
		super("Reverts external symbols in package to internal status.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "SYMBOLS").buildList();
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return new OptionalParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE")
				.initForm(PackageVariables.PACKAGE.getValue())
				.suppliedPBinding()
				.buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Symbols", ListType.INSTANCE);

		final List<LispStruct> symbols = ((ListStruct) lispStruct).getAsJavaList();
		final List<SymbolStruct<?>> realSymbols = new ArrayList<>(symbols.size());
		for (final LispStruct symbol : symbols) {
			validator.validateTypes(symbol, functionName(), "Symbol", SymbolType.INSTANCE);
			realSymbols.add((SymbolStruct<?>) symbol);
		}

		final PackageStruct aPackage = getPackage(lispStructs);
		final SymbolStruct<?>[] symbolArray = realSymbols.toArray(new SymbolStruct<?>[realSymbols.size()]);
		aPackage.unexport(symbolArray);

		return TStruct.INSTANCE;
	}

	private PackageStruct getPackage(final LispStruct... lispStructs) {
		final PackageStruct aPackage;
		if (lispStructs.length >= 2) {
			final LispStruct packageDesignator = lispStructs[1];
			aPackage = findPackageFunction.findPackage(packageDesignator);
		} else {
			aPackage = PackageVariables.PACKAGE.getValue();
		}
		return aPackage;
	}

	@Override
	protected String functionName() {
		return "UNEXPORT";
	}
}
