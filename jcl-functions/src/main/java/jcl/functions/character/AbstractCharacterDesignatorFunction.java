/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;
import java.util.function.Supplier;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operate on character designators, namely
 * {@link CharacterStruct}s, {@link StringStruct}s, and {@link SymbolStructImpl}s.
 */
abstract class AbstractCharacterDesignatorFunction extends CommonLispBuiltInFunctionStruct {

	protected AbstractCharacterDesignatorFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("CHARACTER-DESIGNATOR")
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the character function that gets the character-designator parameter object (character,
	 * string, or symbol) and applies the {@link Function} from the {@link #characterFunction()} function.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of the {@link #characterFunction()} applied to the character-designator parameter value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		// TODO: CharacterDesignator type??
		final LispStruct characterDesignator = arguments.getRequiredArgument("CHARACTER-DESIGNATOR");
//		validator.validateTypes(lispStruct, "CHARACTER", "Character",
//		                        CharacterType.INSTANCE, StringType.INSTANCE, SymbolType.INSTANCE);


		return characterFunction().apply(characterDesignator).get();
	}

	/**
	 * Abstract method to return a {@link Function} that consumes a {@link LispStruct} and returns a {@link
	 * CharacterStruct} as a result.
	 *
	 * @return returns a {@link Function} that consumes a {@link LispStruct} and returns a {@link CharacterStruct} as a
	 * result
	 */
	protected abstract Function<LispStruct, Supplier<CharacterStruct>> characterFunction();
}
