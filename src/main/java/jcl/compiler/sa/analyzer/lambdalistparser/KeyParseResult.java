/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;

final class KeyParseResult extends ParseResult {

	private final List<KeyParameter> keyBindings;

	KeyParseResult(final LispStruct currentElement, final List<KeyParameter> keyBindings) {
		super(currentElement);
		this.keyBindings = keyBindings;
	}

	List<KeyParameter> getKeyBindings() {
		return keyBindings;
	}
}
