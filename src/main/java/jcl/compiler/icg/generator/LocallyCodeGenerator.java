/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.Deque;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.struct.specialoperator.LocallyStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'locally' special operator code generation.
 */
@Component
final class LocallyCodeGenerator implements CodeGenerator<LocallyStruct> {

	/**
	 * {@link PrognCodeGenerator} used for generating the {@link LocallyStruct#forms}.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

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
	 *      new IntIntegerStruct(var2);
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link LocallyStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<LocallyStruct> event) {
		final LocallyStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final PrognStruct forms = input.getForms();
		final Environment locallyEnvironment = input.getLocallyEnvironment();

		final Deque<Environment> environmentDeque = generatorState.getEnvironmentDeque();

		environmentDeque.addFirst(locallyEnvironment);
		codeGenerator.generate(forms, generatorState);
		environmentDeque.removeFirst();
	}
}
