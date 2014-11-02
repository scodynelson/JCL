package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;

public class LoadTimeValueCodeGenerator {

	public static void genCodeLoadTimeValue(final IntermediateCodeGenerator icg, final ListStruct list) {
		// This list looks like (load-time-value some-field-name)
		// all we have to do is get the value of the field
		icg.emitter.emitGetstatic(icg.classNames.peek(), list.getRest().getFirst().toString(), "Ljava/lang/Object;");
	}
}
