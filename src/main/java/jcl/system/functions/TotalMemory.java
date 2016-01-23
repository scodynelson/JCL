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
public final class TotalMemory extends AbstractExtensionsFunctionStruct {

	public TotalMemory() {
		super("Returns the current total runtime memory usage.");
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final long totalMemory = Runtime.getRuntime().totalMemory();
		return new IntegerStruct(BigInteger.valueOf(totalMemory));
	}

	@Override
	protected String functionName() {
		return "TOTAL-MEMORY";
	}
}
