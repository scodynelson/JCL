/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.types.ListType;
import jcl.types.SymbolType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code export}.
 */
@Component
public final class ExportFunction extends AbstractOptionalPackageFunction {

	private static final long serialVersionUID = -3271748062551723057L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public ExportFunction() {
		super("Makes one or more symbols that are accessible in package (whether directly or by inheritance) be external symbols of that package.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "SYMBOLS").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Symbols", ListType.INSTANCE);

		final List<LispStruct> symbols = ((ListStruct) lispStruct).getAsJavaList();
		final List<SymbolStruct<?>> realSymbolsList = new ArrayList<>(symbols.size());
		for (final LispStruct symbol : symbols) {
			validator.validateTypes(symbol, functionName(), "Symbol", SymbolType.INSTANCE);
			realSymbolsList.add((SymbolStruct<?>) symbol);
		}

		final PackageStruct aPackage = getPackage(lispStructs);
		final SymbolStruct<?>[] realSymbolArray = realSymbolsList.toArray(new SymbolStruct<?>[realSymbolsList.size()]);
		aPackage.export(realSymbolArray);

		return TStruct.INSTANCE;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code export} as a string.
	 *
	 * @return the function name {@code export} as a string
	 */
	@Override
	protected String functionName() {
		return "EXPORT";
	}
}
