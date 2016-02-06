/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import org.springframework.stereotype.Component;

@Component
public final class ReadCharNoHangFunction extends ReadCharFunction {

	@Override
	protected String functionName() {
		return "READ-CHAR-NO-HANG";
	}
}
