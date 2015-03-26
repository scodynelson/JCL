package jcl.compiler.real.icg.generator.specialoperator.old.special;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.lists.ListStruct;

//@Component
public class MacroLambdaCodeGenerator implements CodeGenerator<ListStruct> {

	//	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {
		classBuilder.setMacroLambda(true);
//		lambdaCodeGenerator.generate(input, codeGenerator, classBuilder); TODO
	}
}
