package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;

public interface CodeGenerator<I extends LispStruct> {

	void generate(I input, IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder);
}
