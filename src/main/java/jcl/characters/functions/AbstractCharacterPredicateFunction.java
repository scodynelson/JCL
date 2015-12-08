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

abstract class AbstractCharacterPredicateFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 8935124915148949205L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Protected constructor passing the provided {@code documentation} string to the super constructor.
	 *
	 * @param documentation
	 * 		the documentation string
	 */
	protected AbstractCharacterPredicateFunction(final String documentation) {
		super(documentation);
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} character object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} character object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Character", CharacterType.INSTANCE);

		final CharacterStruct character = (CharacterStruct) lispStruct;
		return BooleanStructs.toLispBoolean(predicate().test(character));
	}

	/**
	 * Abstract method to return a {@link Predicate} that consumes a {@link CharacterStruct}.
	 *
	 * @return returns a {@link Predicate} that consumes a {@link CharacterStruct}
	 */
	protected abstract Predicate<CharacterStruct> predicate();
}
