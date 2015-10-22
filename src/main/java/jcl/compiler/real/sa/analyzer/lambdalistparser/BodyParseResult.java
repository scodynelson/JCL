/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.BodyBinding;

final class BodyParseResult extends ParseResult {

	private final BodyBinding bodyBinding;

	BodyParseResult(final LispStruct currentElement, final BodyBinding bodyBinding) {
		super(currentElement);
		this.bodyBinding = bodyBinding;
	}

	BodyBinding getBodyBinding() {
		return bodyBinding;
	}
}
