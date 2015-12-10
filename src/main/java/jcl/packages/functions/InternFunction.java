/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.struct.ValuesStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.StringType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code intern}.
 */
@Component
public final class InternFunction extends AbstractOptionalPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4889133141323870879L;

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
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "String", StringType.INSTANCE);
		final StringStruct aString = (StringStruct) lispStruct;

		final PackageStruct aPackage = getPackage(lispStructs);

		final PackageSymbolStruct packageSymbol = aPackage.intern(aString.getAsJavaString());

		final SymbolStruct<?> symbol = packageSymbol.getSymbol();
		final KeywordStruct packageSymbolType = packageSymbol.getPackageSymbolType();
		return new ValuesStruct(symbol, packageSymbolType);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code intern} as a string.
	 *
	 * @return the function name {@code intern} as a string
	 */
	@Override
	protected String functionName() {
		return "INTERN";
	}
}
