/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.List;
import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.types.CharacterType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractCharacterEqualityFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = 3117929060088318079L;

	@Autowired
	private TypeValidator validator;

	protected AbstractCharacterEqualityFunction(final String documentation) {
		super(documentation);
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
	}

	@Override
	protected RestParameter getRestBinding() {
		return new RestParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CHARACTERS").build();
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

	protected abstract Function<CharacterStruct[], Boolean> characterEqualityFunction();
}
