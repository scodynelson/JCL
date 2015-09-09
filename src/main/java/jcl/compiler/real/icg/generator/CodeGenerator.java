package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;

@FunctionalInterface
public interface CodeGenerator<I extends LispStruct> {

	void generate(I input, final GeneratorState generatorState);
}
