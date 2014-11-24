package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.SymbolFunctionCodeGenerator;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;

public class FunctionCodeGenerator implements CodeGenerator<ListStruct> {

	public static final FunctionCodeGenerator INSTANCE = new FunctionCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		ListStruct restOfList = input.getRest();
		final Object fn = restOfList.getFirst();
		if (fn instanceof SymbolStruct) {
			SymbolFunctionCodeGenerator.INSTANCE.generate((SymbolStruct) fn, codeGenerator);
		} else if (fn instanceof ListStruct) {
			final ListStruct fnList = (ListStruct) fn;
//            if (fnList.getCar() == SpecialOperator.LAMBDA) {
//                genCodeLambda(fnList);
//            } else {
			// this is a setf function (setf foo)
			// this is a call to return the setf function in the specified symbol
			// It's ok if there is no function right now. This is just code to
			// get it when needed
			// Step 1: get the symbol
			// Step 2: return the function stashed in the symbol or NIL if not there
			// The SETF expander will ensure that there will be a FUNCALL #'(setf foo) with args
			final SymbolStruct<?> setfSymbol = (SymbolStruct) ((ListStruct) fn).getRest().getFirst();
			codeGenerator.genCodeSpecialVariable(setfSymbol); // now we have the symbol on the stack
			// number the invoke
			final Label label = new Label();
			codeGenerator.emitter.visitMethodLabel(label);
			// extract the setf function if there is one
			codeGenerator.emitter.emitCheckcast("lisp/system/SymbolImpl");
			codeGenerator.emitter.emitInvokevirtual("lisp/system/SymbolImpl", "getSetfFunction", "()", "Llisp/common/type/Function;", false);
			codeGenerator.emitter.emitDup();      // need to test to see it's there
			final Label yesSetfFunction = new Label();
			codeGenerator.emitter.emitIfnonnull(yesSetfFunction); // there is no setf function, return NIL
			codeGenerator.emitter.emitPop();      // balance the stack
			codeGenerator.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
			codeGenerator.emitter.visitMethodLabel(yesSetfFunction);
//            }
		} else {
			codeGenerator.icgMainLoop(fn);
		}
	}
}
