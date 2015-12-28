/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import jcl.LispType;
import jcl.functions.AbstractPredicateCommonLispFunction;
import jcl.types.CharacterType;
import org.springframework.stereotype.Component;

@Component
public final class CharacterP extends AbstractPredicateCommonLispFunction {

	private static final long serialVersionUID = -3260845734052708742L;

	public CharacterP() {
		super("Returns true if object is of type character; otherwise, returns false.");
	}

	@Override
	protected String functionName() {
		return "CHARACTERP";
	}

	@Override
	protected LispType testType() {
		return CharacterType.INSTANCE;
	}
}
