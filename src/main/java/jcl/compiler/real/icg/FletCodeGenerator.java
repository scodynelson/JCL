package jcl.compiler.real.icg;

import jcl.compiler.real.icg.specialoperator.PrognCodeGenerator;
import jcl.structs.lists.ListStruct;

public class FletCodeGenerator {

	public static void genCodeFlet(final IntermediateCodeGenerator icg, final ListStruct list) {
		PrognCodeGenerator.genCodeProgn(icg, list);
	}
}
