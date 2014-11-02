package jcl.compiler.real.icg;

import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;

public class MacroletCodeGenerator {

	public static void genCodeMacrolet(final IntermediateCodeGenerator icg, ListStruct list) {
		// Get rid of the MACROLET symbol
		list = list.getRest();

        /* Call icgMainLoop() for each expression in the PROGN call,
		 * and remove all but the last expression's value from the stack  */
		while (!list.equals(NullStruct.INSTANCE)) {
			icg.icgMainLoop(list.getFirst());
			list = list.getRest();
			if (!list.equals(NullStruct.INSTANCE)) {
				icg.emitter.emitPop();
			}
		}
	}
}
