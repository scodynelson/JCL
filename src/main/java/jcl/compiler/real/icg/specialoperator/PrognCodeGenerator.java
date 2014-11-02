package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;

public class PrognCodeGenerator {

	public static void genCodeProgn(final IntermediateCodeGenerator icg, ListStruct list) {
		// Get rid of the PROGN symbol
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
