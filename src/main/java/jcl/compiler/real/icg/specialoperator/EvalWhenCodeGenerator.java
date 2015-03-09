package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.lists.ListStruct;

public class EvalWhenCodeGenerator implements CodeGenerator<ListStruct> {

	public static final EvalWhenCodeGenerator INSTANCE = new EvalWhenCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		//TODO unimplemented 'eval-when'
	}
}
