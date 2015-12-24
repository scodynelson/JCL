/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.types.SymbolType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code unintern}.
 */
@Component
public final class UninternFunction extends AbstractOptionalPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -7726668790858900177L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public UninternFunction() {
		super("Removes symbol from package.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} symbol object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} symbol object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "SYMBOL").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code unintern} package function that uninterns the provided {@link SymbolStruct} from
	 * an optionally provided {@link PackageStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return {@link TStruct#INSTANCE} if the {@link SymbolStruct} was successfully uninterned; {@link
	 * NILStruct#INSTANCE} otherwise
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Symbol", SymbolType.INSTANCE);

		final SymbolStruct symbol = (SymbolStruct) lispStruct;
		final PackageStruct aPackage = getPackage(lispStructs);

		final boolean wasUninterned = aPackage.unintern(symbol);
		return wasUninterned ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code unintern} as a string.
	 *
	 * @return the function name {@code unintern} as a string
	 */
	@Override
	protected String functionName() {
		return "UNINTERN";
	}
}
