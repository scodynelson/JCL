/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.compiler.real.environment.LabelsEnvironment;
import jcl.compiler.real.struct.specialoperator.LabelsStruct.LabelsVar;
import jcl.symbols.SymbolStruct;

public class LabelsStruct extends InnerFunctionStruct<LabelsEnvironment, LabelsVar> {

	private static final long serialVersionUID = -2347494500321073144L;

	public LabelsStruct(final List<LabelsVar> vars, final PrognStruct forms, final LabelsEnvironment lexicalEnvironment) {
		super(vars, forms, lexicalEnvironment);
	}

	public static class LabelsVar extends InnerFunctionStruct.InnerFunctionVar {

		private static final long serialVersionUID = 2989214415282349607L;

		public LabelsVar(final SymbolStruct<?> var, final CompilerFunctionStruct initForm, final boolean isSpecial) {
			super(var, initForm, isSpecial);
		}
	}
}
