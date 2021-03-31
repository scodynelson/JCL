/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.Deque;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.lang.LispStruct;
import lombok.Getter;

@Getter
public class LocallyStruct extends CompilerSpecialOperatorStruct {

	private final List<SpecialDeclarationStruct> specials;
	private final PrognStruct forms;
	private final Environment locallyEnvironment;

	public LocallyStruct(final List<SpecialDeclarationStruct> specials, final List<LispStruct> forms,
	                     final Environment locallyEnvironment) {
		super("locally");
		this.specials = specials;
		this.forms = new PrognStruct(forms);
		this.locallyEnvironment = locallyEnvironment;
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LocallyStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Temporarily pushing the {@link LocallyStruct#locallyEnvironment} onto the {@link
	 * GeneratorState#environmentDeque} while generating the code for the {@link LocallyStruct#forms} values</li>
	 * </ol>
	 * As an example, it will transform {@code (locally 1)} into the following Java code:
	 * <pre>
	 * {@code
	 *      BigInteger var2 = new BigInteger("1");
	 *      new IntegerStruct(var2);
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final Deque<Environment> environmentDeque = generatorState.getEnvironmentDeque();

		environmentDeque.addFirst(locallyEnvironment);
		forms.generate(generatorState);
		environmentDeque.removeFirst();
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                       final int environmentArgStore) {
		// Do Nothing.
	}
}
