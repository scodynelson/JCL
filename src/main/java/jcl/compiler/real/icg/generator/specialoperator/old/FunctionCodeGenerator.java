package jcl.compiler.real.icg.generator.specialoperator.old;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.specialoperator.old.simple.SpecialVariableCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.old.simple.SymbolFunctionCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.old.special.LambdaCodeGenerator;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

//@Component
public class FunctionCodeGenerator implements CodeGenerator<ListStruct> {

	//	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	//	@Autowired
	private SymbolFunctionCodeGenerator symbolFunctionCodeGenerator;

	//	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	//	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final ListStruct restOfList = input.getRest();
		final LispStruct fn = restOfList.getFirst();
		if (fn instanceof SymbolStruct) {
			symbolFunctionCodeGenerator.generate((SymbolStruct<?>) fn, classBuilder);
		} else if (fn instanceof ListStruct) {
			final ListStruct fnList = (ListStruct) fn;
			if (fnList.getFirst() == SpecialOperatorStruct.LAMBDA) {
//	            lambdaCodeGenerator.generate(fnList, classBuilder);
				lambdaCodeGenerator.generate(new LambdaStruct(null, null, null, null), classBuilder);
			} else {
				// this is a setf function (setf foo)
				// this is a call to return the setf function in the specified symbol
				// It's ok if there is no function right now. This is just code to
				// get it when needed
				// Step 1: get the symbol
				// Step 2: return the function stashed in the symbol or NIL if not there
				// The SETF expander will ensure that there will be a FUNCALL #'(setf foo) with args
				final SymbolStruct<?> setfSymbol = (SymbolStruct<?>) ((ListStruct) fn).getRest().getFirst();
				specialVariableCodeGenerator.generate(setfSymbol, classBuilder); // now we have the symbol on the stack
				// number the invoke
				final Label label = new Label();
				mv.visitLabel(label);
				// extract the setf function if there is one
				mv.visitTypeInsn(Opcodes.CHECKCAST, "lisp/system/SymbolImpl");
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/system/SymbolImpl", "getSetfFunction", "()Llisp/common/type/Function;", false);
				mv.visitInsn(Opcodes.DUP);      // need to test to see it's there
				final Label yesSetfFunction = new Label();
				mv.visitJumpInsn(Opcodes.IFNONNULL, yesSetfFunction); // there is no setf function, return NIL
				mv.visitInsn(Opcodes.POP);      // balance the stack
				mv.visitFieldInsn(Opcodes.GETSTATIC, "lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
				mv.visitLabel(yesSetfFunction);
			}
		} else {
			formGenerator.generate(fn, classBuilder);
		}
	}
}
