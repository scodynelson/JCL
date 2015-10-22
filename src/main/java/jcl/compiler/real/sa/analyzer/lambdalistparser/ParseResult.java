/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;

class ParseResult {

	private final LispStruct currentElement;

	ParseResult(final LispStruct currentElement) {
		this.currentElement = currentElement;
	}

	LispStruct getCurrentElement() {
		return currentElement;
	}
}
