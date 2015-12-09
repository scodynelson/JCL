/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ExportFunction extends FunctionStruct {

	public static final SymbolStruct<?> EXPORT_FUNCTION = GlobalPackageStruct.COMMON_LISP.intern("EXPORT").getSymbol();

	private static final long serialVersionUID = -3271748062551723057L;

	@Autowired
	private Printer printer;

	private ExportFunction() {
		super("Makes one or more symbols that are accessible in package (whether directly or by inheritance) be external symbols of that package.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		EXPORT_FUNCTION.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(EXPORT_FUNCTION);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> symbolsArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("SYMBOLS").getSymbol();
		final RequiredParameter symbolsArgRequiredBinding = new RequiredParameter(symbolsArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(symbolsArgRequiredBinding);

		final SymbolStruct<?> packageArgSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("PACKAGE-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter packageArgSuppliedPBinding = new SuppliedPParameter(packageArgSuppliedP);

		final SymbolStruct<?> packageArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PACKAGE").getSymbol();
		final OptionalParameter packageArgOptionalBinding = new OptionalParameter(packageArgSymbol, NullStruct.INSTANCE, packageArgSuppliedPBinding);
		final List<OptionalParameter> optionalParameters = Collections.singletonList(packageArgOptionalBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                       .optionalBindings(optionalParameters)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

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

		final PackageStruct packageStruct;
		if (lispStructs.length == 1) {
			packageStruct = PackageVariables.PACKAGE.getValue();
		} else {
			final LispStruct packageDesignator = lispStructs[1];
			if (packageDesignator instanceof PackageStruct) {
				packageStruct = (PackageStruct) packageDesignator;
			} else if (packageDesignator instanceof StringStruct) {
				final String packageName = ((StringStruct) packageDesignator).getAsJavaString();
				packageStruct = PackageStruct.findPackage(packageName);
			} else {
				final String printedObject = printer.print(packageDesignator);
				throw new TypeErrorException("Package argument not of type Package or String: " + printedObject);
			}
		}
		packageStruct.export(realSymbolsList);

		return TStruct.INSTANCE;
	}
}
