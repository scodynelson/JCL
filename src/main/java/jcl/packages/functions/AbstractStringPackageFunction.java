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
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.StringType;

/**
 * Abstract {@link FunctionStruct} implementation for package functions that operate on string-designators representing
 * symbol names and return {@link ValuesStruct}s based on that symbol's existence. This {@link FunctionStruct} also has
 * an optional package parameter value.
 */
abstract class AbstractStringPackageFunction extends AbstractOptionalPackageFunction {

	/**
	 * Protected constructor passing the provided {@code documentation} string to the super constructor.
	 *
	 * @param documentation
	 * 		the documentation string
	 */
	protected AbstractStringPackageFunction(final String documentation) {
		super(documentation);
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} string object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} string object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STRING").buildList();
	}

	/**
	 * Application method for the package function that gets the optional {@link PackageStruct} to perform the {@link
	 * #packageFunction()} operation on and performs it, returning the appropriate {@link ValuesStruct} populated
	 * object. If the result of the {@link #packageFunction()} operation is {@code null}, {@link NILStruct#INSTANCE} is
	 * returned for both the symbol and the package type.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return a {@link ValuesStruct} object containing the found {@link SymbolStruct} and it's {@link KeywordStruct}
	 * package type, or {@link NILStruct} if the result of the {@link #packageFunction()} operation is {@code null}
	 */
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

	/**
	 * Abstract method to return a {@link BiFunction} function that consumes a {@link PackageStruct} and a {@link
	 * String} and returns a {@link PackageSymbolStruct} result.
	 *
	 * @return a {@link BiFunction} function that consumes a {@link PackageStruct} and a {@link String} and returns a
	 * {@link PackageSymbolStruct} result
	 */
	protected abstract BiFunction<PackageStruct, String, PackageSymbolStruct> packageFunction();
}
