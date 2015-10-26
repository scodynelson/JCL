/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;

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
