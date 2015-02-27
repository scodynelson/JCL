package jcl.compiler.real.icg.specialoperator.special;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;

public class MacroLambdaCodeGenerator implements CodeGenerator<ConsElement> {

	public static final MacroLambdaCodeGenerator INSTANCE = new MacroLambdaCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.MacroLambda = true;
		LambdaCodeGenerator.INSTANCE.generate(input, codeGenerator);
	}
}
