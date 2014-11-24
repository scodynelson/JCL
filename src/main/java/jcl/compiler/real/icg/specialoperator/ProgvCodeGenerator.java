package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.lists.ListStruct;

public class ProgvCodeGenerator implements CodeGenerator<ListStruct> {

	public static final ProgvCodeGenerator INSTANCE = new ProgvCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		//TODO unimplemented 'progv'
	}
}
