/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.functions.AbstractExtensionsFunctionStruct;
import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class MaxMemory extends AbstractExtensionsFunctionStruct {

	private static final long serialVersionUID = -8202963218833948816L;

	public MaxMemory() {
		super("Returns the current max runtime memory usage.");
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final long maxMemory = Runtime.getRuntime().maxMemory();
		return new IntegerStruct(BigInteger.valueOf(maxMemory));
	}

	@Override
	protected String functionName() {
		return "MAX-MEMORY";
	}
}
