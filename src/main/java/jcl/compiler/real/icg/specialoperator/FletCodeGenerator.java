package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;

public class FletCodeGenerator implements CodeGenerator<ListStruct> {

	public static final FletCodeGenerator INSTANCE = new FletCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		PrognCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
	}
}
