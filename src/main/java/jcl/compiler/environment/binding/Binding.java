/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding;

import jcl.lang.SymbolStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class Binding {

	private final SymbolStruct var;

	@Override
	public String toString() {
		return var.toString();
	}
}
