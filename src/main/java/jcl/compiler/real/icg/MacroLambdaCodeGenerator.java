package jcl.compiler.real.icg;

import jcl.structs.lists.ListStruct;

public class MacroLambdaCodeGenerator {

	public static void genCodeMacroLambda(final IntermediateCodeGenerator icg, final ListStruct list) {
		icg.MacroLambda = true;
		LambdaCodeGenerator.genCodeLambda(icg, list);
	}
}
