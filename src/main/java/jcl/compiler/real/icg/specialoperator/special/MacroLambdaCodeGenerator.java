package jcl.compiler.real.icg.specialoperator.special;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;

public class MacroLambdaCodeGenerator {

	public static void genCodeMacroLambda(final IntermediateCodeGenerator icg, final ListStruct list) {
		icg.MacroLambda = true;
		LambdaCodeGenerator.genCodeLambda(icg, list);
	}
}
