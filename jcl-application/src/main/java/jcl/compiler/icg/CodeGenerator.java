package jcl.compiler.icg;

import jcl.lang.LispStruct;

/**
 * Defines the code generation interface with a single method {@link #generate(LispStruct, GeneratorState)} used for
 * generating the input type of {@link LispStruct} specified by the type {@link I}.
 *
 * @param <I>
 * 		the type of the input {@link LispStruct} to generate code for
 */
@FunctionalInterface
public interface CodeGenerator<I extends LispStruct> {

	/**
	 * Generation method that will generate the necessary bytecode for the provided {@link I} input value using the
	 * current {@link GeneratorState} to ensure the current state of the generation process is maintained.
	 *
	 * @param input
	 * 		the {@link I} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	void onGeneratorEvent(GeneratorEvent<I> event);
}
