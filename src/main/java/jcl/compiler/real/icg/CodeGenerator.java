package jcl.compiler.real.icg;

import jcl.LispStruct;

public interface CodeGenerator<I extends LispStruct> {

	void generate(I input, IntermediateCodeGenerator codeGenerator);
}
