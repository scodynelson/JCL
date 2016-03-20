/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;
import java.util.function.Supplier;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code character}.
 */
@Component
public final class CharacterFunction extends AbstractCharacterDesignatorFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharacterFunction() {
		super("Returns the character denoted by the character designator.",
		      "CHARACTER"
		);
	}

	/**
	 * {@inheritDoc}
	 * Creates a {@link Function} applying {@link LispStruct#asCharacter()} against a {@link LispStruct} parameter.
	 *
	 * @return a {@link Function} applying {@link LispStruct#asCharacter()} against a {@link LispStruct}
	 */
	@Override
	protected Function<LispStruct, Supplier<CharacterStruct>> characterFunction() {
		return LispStruct::asCharacter;
	}
}
