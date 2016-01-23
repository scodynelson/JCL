/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispType;
import jcl.functions.AbstractPredicateCommonLispFunction;
import jcl.types.StreamType;
import org.springframework.stereotype.Component;

@Component
public final class StreamPFunction extends AbstractPredicateCommonLispFunction {

	public StreamPFunction() {
		super("Returns true if object is of type stream; otherwise, returns false.");
	}

	@Override
	protected String functionName() {
		return "STREAMP";
	}

	@Override
	protected LispType testType() {
		return StreamType.INSTANCE;
	}
}
