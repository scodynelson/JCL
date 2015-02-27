package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;

public class EvalWhenCodeGenerator implements CodeGenerator<ConsElement> {

	public static final EvalWhenCodeGenerator INSTANCE = new EvalWhenCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		//TODO unimplemented 'eval-when'
	}
}
