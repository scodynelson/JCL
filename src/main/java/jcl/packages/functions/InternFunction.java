/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.struct.ValuesStruct;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.packages.PackageVariables;
import jcl.symbols.KeywordStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.StringType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class InternFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4889133141323870879L;

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
	public InternFunction() {
		super("Enters a symbol named string into package.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "STRING").buildList();
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
		validator.validateTypes(lispStruct, functionName(), "String", StringType.INSTANCE);
		final StringStruct aString = (StringStruct) lispStruct;

		final PackageStruct aPackage = getPackage(lispStructs);

		final PackageSymbolStruct packageSymbol = aPackage.intern(aString.getAsJavaString());
		if (packageSymbol == null) {
			return new ValuesStruct(NullStruct.INSTANCE, NullStruct.INSTANCE);
		}

		final SymbolStruct<?> symbol = packageSymbol.getSymbol();
		final KeywordStruct packageSymbolType = packageSymbol.getPackageSymbolType();
		return new ValuesStruct(symbol, packageSymbolType);
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
		return "INTERN";
	}
}
