/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class UpperCasePFunction extends AbstractCharacterPredicateFunction {

	private static final long serialVersionUID = -6043849777082339800L;

	private UpperCasePFunction() {
		super("Returns true if character is an uppercase character; otherwise, returns false.");
	}

	@Override
	protected String functionName() {
		return "UPPER-CASE-P";
	}

	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isUpperCase;
	}
}
