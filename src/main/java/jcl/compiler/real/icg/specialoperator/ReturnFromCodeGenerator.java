package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.SpecialVariableCodeGenerator;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;

public class ReturnFromCodeGenerator implements CodeGenerator<ListStruct> {

	public static final ReturnFromCodeGenerator INSTANCE = new ReturnFromCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		// Get rid of the RETURN-FROM symbol
		ListStruct restOfList = input.getRest();
		final SymbolStruct<?> sym = (SymbolStruct) restOfList.getFirst();
		restOfList = restOfList.getRest();

		classBuilder.getEmitter().emitNew("lisp/system/compiler/exceptions/ReturnFromException");
		// +1 -> exception
		classBuilder.getEmitter().emitDup();
		// +2 -> exception, exception
		SpecialVariableCodeGenerator.INSTANCE.generate(sym, codeGenerator, classBuilder);
		// +3 -> exception, exception, name
		codeGenerator.icgMainLoop(restOfList.getFirst(), classBuilder);
		classBuilder.getEmitter().emitInvokespecial("lisp/system/compiler/exceptions/ReturnFromException", "<init>", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", false);
		classBuilder.getEmitter().emitAthrow();
	}
}
