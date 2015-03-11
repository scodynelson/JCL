package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;

public class MultipleValueProg1CodeGenerator implements CodeGenerator<ListStruct> {

	public static final MultipleValueProg1CodeGenerator INSTANCE = new MultipleValueProg1CodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		//TODO unimplemented 'prog1'
	}
}
