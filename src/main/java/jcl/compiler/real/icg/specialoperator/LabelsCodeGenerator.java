package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.specialoperator.PrognCodeGenerator;
import jcl.structs.lists.ListStruct;

public class LabelsCodeGenerator {

	public static void genCodeLabels(final IntermediateCodeGenerator icg, final ListStruct list) {
		PrognCodeGenerator.genCodeProgn(icg, list);
	}
}
