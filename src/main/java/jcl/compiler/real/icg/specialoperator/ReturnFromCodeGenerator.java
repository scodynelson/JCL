package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SymbolStruct;

public class ReturnFromCodeGenerator {

	public static void genCodeReturnFrom(final IntermediateCodeGenerator icg, ListStruct list) {
		// Get rid of the RETURN-FROM symbol
		list = list.getRest();
		final SymbolStruct<?> sym = (SymbolStruct) list.getFirst();
		list = list.getRest();

		icg.emitter.emitNew("lisp/system/compiler/exceptions/ReturnFromException");
		// +1 -> exception
		icg.emitter.emitDup();
		// +2 -> exception, exception
		icg.genCodeSpecialVariable(sym);
		// +3 -> exception, exception, name
		icg.icgMainLoop(list.getFirst());
		icg.emitter.emitInvokespecial("lisp/system/compiler/exceptions/ReturnFromException", "<init>", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", false);
		icg.emitter.emitAthrow();
	}
}
