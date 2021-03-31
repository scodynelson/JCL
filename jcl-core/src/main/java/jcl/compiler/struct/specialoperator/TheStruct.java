/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import lombok.Getter;

@Getter
public class TheStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct valueType;
	private final LispStruct form;

	public TheStruct(final LispStruct valueType, final LispStruct form) {
		super("the");
		this.valueType = valueType;
		this.form = form;
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link TheStruct} objects. The {@link TheStruct#form} value is generated directly.
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		// TODO: do we want to add the logic here to verify the type information of the generated form???

		form.generate(generatorState);
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                       final int environmentArgStore) {
		// Do Nothing
	}
}
