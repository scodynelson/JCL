package jcl.compiler.real.icg.specialoperator.special;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;

public class DeclareCodeGenerator implements CodeGenerator<ListStruct> {

	public static final DeclareCodeGenerator INSTANCE = new DeclareCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
	}
}
