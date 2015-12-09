/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.types.CharacterType;
import jcl.types.ListType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import org.springframework.stereotype.Component;

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

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "SYMBOL-NAMES").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Symbol names to shadow", ListType.INSTANCE);

		final List<LispStruct> symbolNames = ((ListStruct) lispStruct).getAsJavaList();
		final List<String> realSymbolNames = new ArrayList<>(symbolNames.size());
		for (final LispStruct symbolName : symbolNames) {
			final String realSymbolName = getStringValue(symbolName);
			realSymbolNames.add(realSymbolName);
		}

		final PackageStruct aPackage = getPackage(lispStructs);
		final String[] symbolNameArray = realSymbolNames.toArray(new String[realSymbolNames.size()]);
		aPackage.shadow(symbolNameArray);

		return TStruct.INSTANCE;
	}

	private String getStringValue(final LispStruct stringDesignator) {
		validator.validateTypes(stringDesignator, functionName(), "String-Designator", StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE);

		if (stringDesignator instanceof StringStruct) {
			return ((StringStruct) stringDesignator).getAsJavaString();
		} else if (stringDesignator instanceof SymbolStruct) {
			return ((SymbolStruct) stringDesignator).getName();
		} else if (stringDesignator instanceof CharacterStruct) {
			return ((CharacterStruct) stringDesignator).getCharacter().toString();
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}

	@Override
	protected String functionName() {
		return "SHADOW";
	}
}
