/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import jcl.LispType;
import jcl.functions.AbstractPredicateCommonLispFunction;
import jcl.types.CharacterType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code characterp}.
 */
@Component
public final class CharacterPFunction extends AbstractPredicateCommonLispFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharacterPFunction() {
		super("Returns true if object is of type character; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code characterp} as a string.
	 *
	 * @return the function name {@code characterp} as a string
	 */
	@Override
	protected String functionName() {
		return "CHARACTERP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterType#INSTANCE} as the type instance to check against.
	 *
	 * @return the {@link CharacterType#INSTANCE} singleton value
	 */
	@Override
	protected LispType testType() {
		return CharacterType.INSTANCE;
	}
}
