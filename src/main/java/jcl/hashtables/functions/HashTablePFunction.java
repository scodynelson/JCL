/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import jcl.LispType;
import jcl.functions.AbstractPredicateCommonLispFunction;
import jcl.types.HashTableType;
import org.springframework.stereotype.Component;

@Component
public final class HashTablePFunction extends AbstractPredicateCommonLispFunction {

	private static final long serialVersionUID = -9145781039029906911L;

	public HashTablePFunction() {
		super("Returns true if object is of type hash-table; otherwise, returns false.");
	}

	@Override
	protected String functionName() {
		return "HASH-TABLE-P";
	}

	@Override
	protected LispType testType() {
		return HashTableType.INSTANCE;
	}
}
