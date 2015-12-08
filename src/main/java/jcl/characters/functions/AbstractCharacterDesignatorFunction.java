/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.List;
import java.util.function.Function;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

abstract class AbstractCharacterDesignatorFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -748646141505142347L;

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
	protected AbstractCharacterDesignatorFunction(final String documentation) {
		super(documentation);
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} character-designator object (character, string, or symbol) for this
	 * function.
	 *
	 * @return a list of a single {@link RequiredParameter} character-designator object (character, string, or symbol)
	 * object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER-DESIGNATOR").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, "CHARACTER", "Character",
				CharacterType.INSTANCE, StringType.INSTANCE, SymbolType.INSTANCE);

		if (lispStruct instanceof CharacterStruct) {
			return lispStruct;
		} else if (lispStruct instanceof StringStruct) {
			return stringFunction().apply((StringStruct) lispStruct);
		} else if (lispStruct instanceof SymbolStruct) {
			return symbolFunction().apply((SymbolStruct) lispStruct);
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}

	/**
	 * Abstract method to return a {@link Function} that consumes a {@link StringStruct} and returns a {@link
	 * LispStruct} as a result.
	 *
	 * @return returns a {@link Function} that consumes a {@link StringStruct} and returns a {@link LispStruct} as a
	 * result
	 */
	protected abstract Function<StringStruct, LispStruct> stringFunction();

	/**
	 * Abstract method to return a {@link Function} that consumes a {@link SymbolStruct} and returns a {@link
	 * LispStruct} as a result.
	 *
	 * @return returns a {@link Function} that consumes a {@link SymbolStruct} and returns a {@link LispStruct} as a
	 * result
	 */
	protected abstract Function<SymbolStruct<?>, LispStruct> symbolFunction();
}
