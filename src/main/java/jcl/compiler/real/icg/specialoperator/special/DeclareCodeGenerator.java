package jcl.compiler.real.icg.specialoperator.special;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;

public class DeclareCodeGenerator implements CodeGenerator<ConsElement> {

	public static final DeclareCodeGenerator INSTANCE = new DeclareCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
	}
}
