/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.List;
import java.util.function.Predicate;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStructs;
import jcl.types.CharacterType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractCharacterPredicateFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = 8935124915148949205L;

	@Autowired
	private TypeValidator validator;

	protected AbstractCharacterPredicateFunction(final String documentation) {
		super(documentation);
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Character", CharacterType.INSTANCE);

		final CharacterStruct character = (CharacterStruct) lispStruct;
		return BooleanStructs.toLispBoolean(predicate().test(character));
	}

	protected abstract Predicate<CharacterStruct> predicate();
}
