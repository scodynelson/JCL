package jcl.compiler.real.icg;

import jcl.compiler.real.element.Element;

public interface CodeGenerator<I extends Element> {

	void generate(I input, IntermediateCodeGenerator codeGenerator);
}
