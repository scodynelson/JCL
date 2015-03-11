package jcl.compiler.real.icg.specialoperator.special;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;

public class MacroLambdaCodeGenerator implements CodeGenerator<ListStruct> {

	public static final MacroLambdaCodeGenerator INSTANCE = new MacroLambdaCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		classBuilder.setMacroLambda(true);
		LambdaCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
	}
}
