/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CharacterFunction extends FunctionStruct {

	private static final long serialVersionUID = -1229967753542859679L;

	@Autowired
	private TypeValidator validator;

	private CharacterFunction() {
		super("Returns the character denoted by the character designator.");
		initLambdaListBindings();
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct<?> symbol = aPackage.intern("CHARACTER").getSymbol();
		aPackage.export(symbol);
		return symbol;
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final SymbolStruct<?> character = GlobalPackageStruct.COMMON_LISP.intern("CHARACTER").getSymbol();
		final RequiredParameter requiredParameter = new RequiredParameter(character);
		return Collections.singletonList(requiredParameter);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, "CHARACTER", "Character",
				CharacterType.INSTANCE, StringType.INSTANCE, SymbolType.INSTANCE);

		if (lispStruct instanceof CharacterStruct) {
			return lispStruct;
		} else if (lispStruct instanceof StringStruct) {
			final StringStruct stringStruct = (StringStruct) lispStruct;
			return getCharacterFromString(stringStruct.getAsJavaString(), "String");
		} else if (lispStruct instanceof SymbolStruct) {
			final SymbolStruct<?> symbolStruct = (SymbolStruct) lispStruct;
			return getCharacterFromString(symbolStruct.getName(), "Symbol name");
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}

	private static CharacterStruct getCharacterFromString(final String aString, final String errorPrefix) {
		if (aString.length() != 1) {
			throw new SimpleErrorException(errorPrefix + " is not of length one: " + aString);
		}
		return new CharacterStruct(aString.charAt(0));
	}
}
