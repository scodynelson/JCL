package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;

public class FletCodeGenerator implements CodeGenerator<ConsElement> {

	public static final FletCodeGenerator INSTANCE = new FletCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		PrognCodeGenerator.INSTANCE.generate(input, codeGenerator);
	}
}
