package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class GoCodeGenerator implements CodeGenerator<ListStruct> {

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {
		/* Get the symbol out of the list. */
		final ListStruct restOfList = input.getRest();
		final SymbolStruct<?> sym = (SymbolStruct) restOfList.getFirst();

        /*
		// first, try to see if this is a go in the same tagbody (the most common)
        TagbodyLabel tbl = findTagbodyBySymbol(tagbodyStack.peek(), sym);
        if (tbl != null) {
        emitter.emitGoto(tbl.label);
        } else {
         */
		/* Throw a GoException. */
		classBuilder.getEmitter().emitNew("lisp/system/compiler/exceptions/GoException");
		classBuilder.getEmitter().emitDup();
		//genCodeSpecialSymbol(sym);
		classBuilder.getEmitter().emitLdc(String.valueOf(TagbodyCodeGenerator.findTagbodyInStack(classBuilder.getTagbodyStack(), sym).getIndex()));   // me
		//emitter.emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Llisp/common/type/Symbol;)V"); //me
		classBuilder.getEmitter().emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Ljava/lang/Object;)", "V", false);
		classBuilder.getEmitter().emitAthrow();
		//}
	}
}
