package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.SymbolFunctionCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SymbolStruct;
import org.objectweb.asm.Label;

public class FunctionCodeGenerator {

	public static void genCodeFunction(final IntermediateCodeGenerator icg, ListStruct list) {
		list = list.getRest();
		final Object fn = list.getFirst();
		if (fn instanceof SymbolStruct) {
			SymbolFunctionCodeGenerator.genCodeSymbolFunction(icg, (SymbolStruct) fn);
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
			icg.genCodeSpecialVariable(setfSymbol); // now we have the symbol on the stack
			// number the invoke
			final Label label = new Label();
			icg.emitter.visitMethodLabel(label);
			// extract the setf function if there is one
			icg.emitter.emitCheckcast("lisp/system/SymbolImpl");
			icg.emitter.emitInvokevirtual("lisp/system/SymbolImpl", "getSetfFunction", "()", "Llisp/common/type/Function;", false);
			icg.emitter.emitDup();      // need to test to see it's there
			final Label yesSetfFunction = new Label();
			icg.emitter.emitIfnonnull(yesSetfFunction); // there is no setf function, return NIL
			icg.emitter.emitPop();      // balance the stack
			icg.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
			icg.emitter.visitMethodLabel(yesSetfFunction);
//            }
		} else {
			icg.icgMainLoop(fn);
		}
	}
}
