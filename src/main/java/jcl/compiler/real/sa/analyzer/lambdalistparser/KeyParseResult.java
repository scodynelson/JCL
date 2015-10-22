/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;

final class KeyParseResult extends ParseResult {

	private final List<KeyBinding> keyBindings;

	KeyParseResult(final LispStruct currentElement, final List<KeyBinding> keyBindings) {
		super(currentElement);
		this.keyBindings = keyBindings;
	}

	List<KeyBinding> getKeyBindings() {
		return keyBindings;
	}
}
