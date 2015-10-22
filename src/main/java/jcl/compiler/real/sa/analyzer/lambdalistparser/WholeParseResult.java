/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.compiler.real.environment.binding.lambdalist.WholeBinding;

final class WholeParseResult extends ParseResult {

	private final WholeBinding wholeBinding;

	WholeParseResult(final WholeBinding wholeBinding) {
		super(null);
		this.wholeBinding = wholeBinding;
	}

	WholeBinding getWholeBinding() {
		return wholeBinding;
	}
}
