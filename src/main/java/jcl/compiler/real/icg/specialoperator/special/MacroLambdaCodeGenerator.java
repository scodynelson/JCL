package jcl.compiler.real.icg.specialoperator.special;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroLambdaCodeGenerator implements CodeGenerator<ListStruct> {

	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		classBuilder.setMacroLambda(true);
		lambdaCodeGenerator.generate(input, codeGenerator, classBuilder);
	}
}
