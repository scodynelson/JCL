/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiFunction;

import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStructImpl;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.statics.PackageVariables;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;

/**
 * Abstract {@link FunctionStruct} implementation for package functions that operate on string-designators representing
 * symbol names and return {@link ValuesStruct}s based on that symbol's existence. This {@link FunctionStruct} also has
 * an optional package parameter value.
 */
abstract class AbstractStringPackageFunction extends CommonLispBuiltInFunctionStruct {

	protected AbstractStringPackageFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("STRING")
		                .optionalParameter("PACKAGE").withInitialValue(PackageVariables.PACKAGE.getVariableValue())
		);
	}

	/**
	 * Application method for the package function that gets the optional {@link PackageStructImpl} to perform the {@link
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
	public LispStruct apply(final Arguments arguments) {
		final StringStruct aString = arguments.getRequiredArgument("STRING", StringStruct.class);
		final PackageStructImpl aPackage = arguments.getOptionalArgument("PACKAGE", PackageStructImpl.class);
		final PackageSymbolStruct packageSymbol = packageFunction().apply(aPackage, aString.getAsJavaString());
		if (packageSymbol == null) {
			return ValuesStruct.valueOf(NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		final SymbolStruct symbol = packageSymbol.getSymbol();
		final KeywordStruct packageSymbolType = packageSymbol.getPackageSymbolType();
		return ValuesStruct.valueOf(symbol, packageSymbolType);
	}

	/**
	 * Abstract method to return a {@link BiFunction} function that consumes a {@link PackageStructImpl} and a {@link
	 * String} and returns a {@link PackageSymbolStruct} result.
	 *
	 * @return a {@link BiFunction} function that consumes a {@link PackageStructImpl} and a {@link String} and returns a
	 * {@link PackageSymbolStruct} result
	 */
	protected abstract BiFunction<PackageStructImpl, String, PackageSymbolStruct> packageFunction();
}
