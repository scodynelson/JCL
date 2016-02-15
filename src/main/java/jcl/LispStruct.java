/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl;

import java.util.function.Supplier;

import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.pathnames.PathnameStruct;
import jcl.types.TType;

/**
 * The {@link LispStruct} is the representation for all Lisp types.
 */
public interface LispStruct {

	/**
	 * This method returns the type of the struct.
	 *
	 * @return the type of the struct
	 */
	default LispType getType() {
		return TType.INSTANCE;
	}

	/**
	 * By default, throws a {@link TypeErrorException} indicating a problem conversion to a character. This will be
	 * overridden in implementations that support conversion to characters.
	 *
	 * @return a converted character, or a throw {@link TypeErrorException}
	 */
	default Supplier<CharacterStruct> toCharacter() {
		throw new TypeErrorException("Type cannot be converted to Character.");
	}

	/**
	 * By default, throws a {@link TypeErrorException} indicating a problem conversion to a character by its full
	 * character name. This will be overridden in implementations that support conversion to characters.
	 *
	 * @return a converted character, or a throw {@link TypeErrorException}
	 */
	default Supplier<CharacterStruct> toNamedCharacter() {
		throw new TypeErrorException("Type cannot be converted to Character.");
	}

	/**
	 * By default, throws a {@link TypeErrorException} indicating a problem conversion to a pathname. This will be
	 * overridden in implementations that support conversion to pathnames.
	 *
	 * @return a converted pathname, or a throw {@link TypeErrorException}
	 */
	default Supplier<PathnameStruct> toPathname() {
		throw new TypeErrorException("Type cannot be converted to Pathname.");
	}
}
