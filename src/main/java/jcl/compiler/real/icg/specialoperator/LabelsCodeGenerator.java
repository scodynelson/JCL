package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;

public class LabelsCodeGenerator implements CodeGenerator<ListStruct> {

	public static final LabelsCodeGenerator INSTANCE = new LabelsCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		PrognCodeGenerator.INSTANCE.generate(input, codeGenerator);
	}
}
