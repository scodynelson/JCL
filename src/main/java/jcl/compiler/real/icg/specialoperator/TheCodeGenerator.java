package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;

public class TheCodeGenerator implements CodeGenerator<ListStruct> {

	public static final TheCodeGenerator INSTANCE = new TheCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		//TODO unimplemented 'the'
	}
}
