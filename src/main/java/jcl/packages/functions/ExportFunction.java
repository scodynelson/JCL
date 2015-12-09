/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ExportFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -3271748062551723057L;

	@Autowired
	private Printer printer;

	@Autowired
	private FindPackageFunction findPackageFunction;

	public ExportFunction() {
		super("Makes one or more symbols that are accessible in package (whether directly or by inheritance) be external symbols of that package.");
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

		final LispStruct symbols = lispStructs[0];
		if (!(symbols instanceof ListStruct)) {
			final String printedObject = printer.print(symbols);
			throw new TypeErrorException("Symbols argument not of type List: " + printedObject);
		}

		final List<LispStruct> symbolsList = ((ListStruct) symbols).getAsJavaList();
		final List<SymbolStruct<?>> realSymbolsList = new ArrayList<>(symbolsList.size());
		for (final LispStruct symbol : symbolsList) {
			if (!(symbol instanceof SymbolStruct)) {
				final String printedObject = printer.print(symbol);
				throw new TypeErrorException("Symbol argument not of type Symbol: " + printedObject);
			}
			realSymbolsList.add((SymbolStruct<?>) symbol);
		}

		final PackageStruct aPackage;
		if (lispStructs.length >= 2) {
			final LispStruct packageDesignator = lispStructs[1];
			aPackage = findPackageFunction.findPackage(packageDesignator);
		} else {
			aPackage = PackageVariables.PACKAGE.getValue();
		}
		aPackage.export(realSymbolsList);

		return TStruct.INSTANCE;
	}

	@Override
	protected String functionName() {
		return "EXPORT";
	}
}
