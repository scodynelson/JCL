package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SymbolStruct;

public class ReturnFromCodeGenerator implements CodeGenerator<ListStruct> {

	public static final ReturnFromCodeGenerator INSTANCE = new ReturnFromCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		// Get rid of the RETURN-FROM symbol
		ListStruct restOfList = input.getRest();
		final SymbolStruct<?> sym = (SymbolStruct) restOfList.getFirst();
		restOfList = restOfList.getRest();

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
