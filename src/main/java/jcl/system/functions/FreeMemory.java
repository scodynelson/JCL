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
public final class FreeMemory extends AbstractExtensionsFunctionStruct {

	public FreeMemory() {
		super("Returns the current free runtime memory usage.");
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final long freeMemory = Runtime.getRuntime().freeMemory();
		return new IntegerStruct(BigInteger.valueOf(freeMemory));
	}

	@Override
	protected String functionName() {
		return "FREE-MEMORY";
	}
}
