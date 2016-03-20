/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.List;
import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.packages.GlobalPackageStruct;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates on a {@link CharacterStruct}.
 */
abstract class AbstractCharacterFunction extends CommonLispBuiltInFunctionStruct {

	protected AbstractCharacterFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("CHARACTER")
		);
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} character object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} character object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Application method for the character function that gets the {@link CharacterStruct} parameter object and applies
	 * the result of the abstract {@link #characterFunction()} method with the parameter as the {@link Function}
	 * parameter.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of the {@link #characterFunction()} applied to the {@link CharacterStruct} parameter value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct character = arguments.getRequiredArgument("CHARACTER", CharacterStruct.class);
		return characterFunction().apply(character);
	}

	/**
	 * Abstract method to return a {@link Function} that consumes a {@link CharacterStruct} and returns a {@link
	 * LispStruct} as a result.
	 *
	 * @return returns a {@link Function} that consumes a {@link CharacterStruct} and returns a {@link LispStruct} as a
	 * result
	 */
	protected abstract Function<CharacterStruct, LispStruct> characterFunction();
}
