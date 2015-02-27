package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.system.EnhancedLinkedList;

public class ReturnFromCodeGenerator implements CodeGenerator<ConsElement> {

	public static final ReturnFromCodeGenerator INSTANCE = new ReturnFromCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		// Get rid of the RETURN-FROM symbol
		EnhancedLinkedList<SimpleElement> restOfList = input.getElements().getAllButFirst();
		final SymbolElement sym = (SymbolElement) restOfList.getFirst();
		restOfList = restOfList.getAllButFirst();

		codeGenerator.emitter.emitNew("lisp/system/compiler/exceptions/ReturnFromException");
		// +1 -> exception
		codeGenerator.emitter.emitDup();
		// +2 -> exception, exception
		codeGenerator.genCodeSpecialVariable(sym);
		// +3 -> exception, exception, name
		codeGenerator.icgMainLoop(restOfList.getFirst());
		codeGenerator.emitter.emitInvokespecial("lisp/system/compiler/exceptions/ReturnFromException", "<init>", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", false);
		codeGenerator.emitter.emitAthrow();
	}
}
