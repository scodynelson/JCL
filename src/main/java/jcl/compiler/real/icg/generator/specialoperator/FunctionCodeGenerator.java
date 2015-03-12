package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.SpecialVariableCodeGenerator;
import jcl.compiler.real.icg.generator.SymbolFunctionCodeGenerator;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FunctionCodeGenerator implements CodeGenerator<ListStruct> {

	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Autowired
	private SymbolFunctionCodeGenerator symbolFunctionCodeGenerator;

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		final ListStruct restOfList = input.getRest();
		final Object fn = restOfList.getFirst();
		if (fn instanceof SymbolStruct) {
			symbolFunctionCodeGenerator.generate((SymbolStruct<?>) fn, codeGenerator, classBuilder);
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
			final SymbolStruct<?> setfSymbol = (SymbolStruct<?>) ((ListStruct) fn).getRest().getFirst();
			specialVariableCodeGenerator.generate(setfSymbol, codeGenerator, classBuilder); // now we have the symbol on the stack
			// number the invoke
			final Label label = new Label();
			classBuilder.getEmitter().visitMethodLabel(label);
			// extract the setf function if there is one
			classBuilder.getEmitter().emitCheckcast("lisp/system/SymbolImpl");
			classBuilder.getEmitter().emitInvokevirtual("lisp/system/SymbolImpl", "getSetfFunction", "()", "Llisp/common/type/Function;", false);
			classBuilder.getEmitter().emitDup();      // need to test to see it's there
			final Label yesSetfFunction = new Label();
			classBuilder.getEmitter().emitIfnonnull(yesSetfFunction); // there is no setf function, return NIL
			classBuilder.getEmitter().emitPop();      // balance the stack
			classBuilder.getEmitter().emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
			classBuilder.getEmitter().visitMethodLabel(yesSetfFunction);
//            }
		} else {
			codeGenerator.icgMainLoop(fn, classBuilder);
		}
	}
}