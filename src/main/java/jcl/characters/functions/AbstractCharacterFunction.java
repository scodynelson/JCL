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
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractCharacterFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -7030656974789702740L;

	@Autowired
	private TypeValidator validator;

	protected AbstractCharacterFunction(final String documentation) {
		super(documentation);
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final CharacterStruct character = getCharacter(lispStructs);
		return characterFunction().apply(character);
	}

	private CharacterStruct getCharacter(final LispStruct... lispStructs) {

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Character", CharacterType.INSTANCE);

		return (CharacterStruct) lispStruct;
	}

	protected abstract Function<CharacterStruct, LispStruct> characterFunction();
}
