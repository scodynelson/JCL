/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.util.Deque;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LocallyEnvironment;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.struct.specialoperator.LocallyStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'locally' special operator code generation.
 */
@Component
class LocallyCodeGenerator implements CodeGenerator<LocallyStruct> {

	/**
	 * {@link PrognCodeGenerator} used for generating the {@link LocallyStruct#forms}.
	 */
	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

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
	 * @param input
	 * 		the {@link LocallyStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final LocallyStruct input, final GeneratorState generatorState) {

		final PrognStruct forms = input.getForms();
		final LocallyEnvironment locallyEnvironment = input.getLocallyEnvironment();

		final Deque<Environment> environmentDeque = generatorState.getEnvironmentDeque();

		environmentDeque.addFirst(locallyEnvironment);
		prognCodeGenerator.generate(forms, generatorState);
		environmentDeque.removeFirst();
	}
}
