/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.types.CharacterType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

public abstract class AbstractCharacterEqualityFunction extends FunctionStruct {

	private static final long serialVersionUID = 3117929060088318079L;

	@Autowired
	private TypeValidator validator;

	protected AbstractCharacterEqualityFunction(final String documentation) {
		super(documentation);
		initLambdaListBindings();
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct<?> symbol = aPackage.intern(functionName()).getSymbol();
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
	protected RestParameter getRestBinding() {
		final SymbolStruct<?> characters = GlobalPackageStruct.COMMON_LISP.intern("CHARACTERS").getSymbol();
		return new RestParameter(characters);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final CharacterStruct[] characters = getCharacters(lispStructs);
		return characterEqualityFunction().apply(characters) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	private CharacterStruct[] getCharacters(final LispStruct... lispStructs) {

		final CharacterStruct[] characters = new CharacterStruct[lispStructs.length];
		for (int i = 0; i < lispStructs.length; i++) {
			final LispStruct lispStruct = lispStructs[i];
			validator.validateTypes(lispStruct, functionName(), "Character", CharacterType.INSTANCE);
			characters[i] = (CharacterStruct) lispStruct;
		}
		return characters;
	}

	protected abstract String functionName();

	protected abstract Function<CharacterStruct[], Boolean> characterEqualityFunction();
}
