package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.symbols.SymbolStruct;
import jcl.system.EnhancedLinkedList;

public class GoCodeGenerator implements CodeGenerator<ConsElement> {

	public static final GoCodeGenerator INSTANCE = new GoCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		/* Get the symbol out of the list. */
		EnhancedLinkedList<SimpleElement> restOfList = input.getElements().getAllButFirst();
		final SymbolStruct<?> sym = (SymbolStruct) restOfList.getFirst();

        /*
		// first, try to see if this is a go in the same tagbody (the most common)
        TagbodyLabel tbl = findTagbodyBySymbol(tagbodyStack.peek(), sym);
        if (tbl != null) {
        emitter.emitGoto(tbl.label);
        } else {
         */
		/* Throw a GoException. */
		codeGenerator.emitter.emitNew("lisp/system/compiler/exceptions/GoException");
		codeGenerator.emitter.emitDup();
		//genCodeSpecialSymbol(sym);
		codeGenerator.emitter.emitLdc("" + TagbodyCodeGenerator.findTagbodyInStack(codeGenerator.tagbodyStack, sym).index);   // me
		//emitter.emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Llisp/common/type/Symbol;)V"); //me
		codeGenerator.emitter.emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Ljava/lang/Object;)", "V", false);
		codeGenerator.emitter.emitAthrow();
		//}
	}
}
