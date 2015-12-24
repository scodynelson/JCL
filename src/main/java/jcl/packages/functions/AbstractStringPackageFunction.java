/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;
import java.util.function.BiFunction;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.struct.ValuesStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.StringType;

abstract class AbstractStringPackageFunction extends AbstractOptionalPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5852499101326991853L;

	protected AbstractStringPackageFunction(final String documentation) {
		super(documentation);
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "STRING").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "String", StringType.INSTANCE);
		final StringStruct aString = (StringStruct) lispStruct;

		final PackageStruct aPackage = getPackage(lispStructs);
		final PackageSymbolStruct packageSymbol = packageFunction().apply(aPackage, aString.getAsJavaString());
		if (packageSymbol == null) {
			return new ValuesStruct(NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		final SymbolStruct symbol = packageSymbol.getSymbol();
		final KeywordStruct packageSymbolType = packageSymbol.getPackageSymbolType();
		return new ValuesStruct(symbol, packageSymbolType);
	}

	protected abstract BiFunction<PackageStruct, String, PackageSymbolStruct> packageFunction();
}
