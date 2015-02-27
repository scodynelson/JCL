package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;

public class TheCodeGenerator implements CodeGenerator<ConsElement> {

	public static final TheCodeGenerator INSTANCE = new TheCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		//TODO unimplemented 'the'
	}
}
