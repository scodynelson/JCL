package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SymbolStruct;

public class GoCodeGenerator {

	public static void genCodeGo(final IntermediateCodeGenerator icg, ListStruct list) {
		/* Get the symbol out of the list. */
		list = list.getRest();
		final SymbolStruct<?> sym = (SymbolStruct) list.getFirst();

        /*
		// first, try to see if this is a go in the same tagbody (the most common)
        TagbodyLabel tbl = findTagbodyBySymbol(tagbodyStack.peek(), sym);
        if (tbl != null) {
        emitter.emitGoto(tbl.label);
        } else {
         */
		/* Throw a GoException. */
		icg.emitter.emitNew("lisp/system/compiler/exceptions/GoException");
		icg.emitter.emitDup();
		//genCodeSpecialSymbol(sym);
		icg.emitter.emitLdc("" + TagbodyCodeGenerator.findTagbodyInStack(icg.tagbodyStack, sym).index);   // me
		//emitter.emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Llisp/common/type/Symbol;)V"); //me
		icg.emitter.emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Ljava/lang/Object;)", "V", false);
		icg.emitter.emitAthrow();
		//}
	}
}
