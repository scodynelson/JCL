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
import jcl.symbols.TStruct;
import jcl.types.CharacterType;
import jcl.types.ListType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code shadow}.
 */
@Component
public final class ShadowFunction extends AbstractOptionalPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -307962001320583822L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public ShadowFunction() {
		super("Assures that symbols with names given by symbol-names are present in the package.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} symbol names list or string-designator object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} symbol names list or string-designator object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "SYMBOL-NAMES").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Symbol names to shadow", ListType.INSTANCE, StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE);

		final PackageStruct aPackage = getPackage(lispStructs);

		final String[] symbolNameArray;
		if (lispStruct instanceof ListStruct) {
			final List<LispStruct> symbolNames = ((ListStruct) lispStruct).getAsJavaList();
			final List<String> realSymbolNames = new ArrayList<>(symbolNames.size());
			for (final LispStruct theSymbolName : symbolNames) {
				final String realSymbolName = getStringFromStringDesignator(theSymbolName, "Symbol Name");
				realSymbolNames.add(realSymbolName);
			}
			symbolNameArray = realSymbolNames.toArray(new String[realSymbolNames.size()]);
		} else {
			symbolNameArray = new String[1];
			symbolNameArray[0] = getStringFromStringDesignator(lispStruct, "Symbol Name");
		}

		aPackage.shadow(symbolNameArray);

		return TStruct.INSTANCE;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code shadow} as a string.
	 *
	 * @return the function name {@code shadow} as a string
	 */
	@Override
	protected String functionName() {
		return "SHADOW";
	}
}
