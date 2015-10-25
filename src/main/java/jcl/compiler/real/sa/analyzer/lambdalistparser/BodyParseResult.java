/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.BodyParameter;

final class BodyParseResult extends ParseResult {

	private final BodyParameter bodyBinding;

	BodyParseResult(final LispStruct currentElement, final BodyParameter bodyBinding) {
		super(currentElement);
		this.bodyBinding = bodyBinding;
	}

	BodyParameter getBodyBinding() {
		return bodyBinding;
	}
}
