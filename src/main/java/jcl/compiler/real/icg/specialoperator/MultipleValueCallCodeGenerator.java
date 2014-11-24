package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;

import java.util.UUID;

public class MultipleValueCallCodeGenerator implements CodeGenerator<ListStruct> {

	public static final MultipleValueCallCodeGenerator INSTANCE = new MultipleValueCallCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {

		// evaluate the lambda, leaving an instance on the stack
		ListStruct restOfList = input.getRest();
		final Object fn = restOfList.getFirst();
		codeGenerator.icgMainLoop(fn);

		// now process each of the arguments, leaving them on the stack
		restOfList = restOfList.getRest();

		// now stuff into a list. It's a bit tricky sense functions can return
		// objects or arrays of objects. Have to check at runtime
		final boolean firstPass = true;
		// make a private field to hold the resulting list
		final SymbolStruct<?> mvcFieldName = new SymbolStruct<>("MVC_Field_" + UUID.randomUUID());
		codeGenerator.emitter.newField(Opcodes.ACC_PRIVATE, mvcFieldName.toString(), "Llisp/common/type/ListStruct;", null, null);
		// initialize it to NIL
		codeGenerator.emitter.emitAload(0);
		codeGenerator.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		codeGenerator.emitter.emitPutfield(codeGenerator.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			// this puts a value or value[] on the stack
			codeGenerator.icgMainLoop(restOfList.getFirst(), true);
			codeGenerator.emitter.emitDup();
			// which is it?
			codeGenerator.emitter.emitInstanceof("[Ljava/lang/Object;");
			// one if by sea... (it is)
			final Label isntArray = new Label();
			final Label allDone = new Label();

			codeGenerator.emitter.emitIfeq(isntArray);  // jumps if it's NOT an array
			// so, make the array into a lisp list
			codeGenerator.emitter.emitCheckcast("[Ljava/lang/Object;");
			codeGenerator.emitter.emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			codeGenerator.emitter.emitGoto(allDone);
			// ....
			codeGenerator.emitter.visitMethodLabel(isntArray);
			// so, make the object into a lisp list
			codeGenerator.emitter.emitCheckcast("java/lang/Object");
			codeGenerator.emitter.emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "(Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			// now we have to splice it to the previous
			codeGenerator.emitter.visitMethodLabel(allDone);

			// now, just nconc the prior list to the current one
			// Don't forget to restore the result into the local field.
			//    stack now holds the current list
			// get the nconc function
			codeGenerator.emitter.emitGetstatic("lisp/extensions/type/CommonLispFunctions", "StdFunctions", "Llisp/extensions/type/CommonLispFunctions;");
			codeGenerator.emitter.emitGetfield("lisp/extensions/type/CommonLispFunctions", "NConc", "Llisp/common/function/NConc;");
			codeGenerator.emitter.emitDup();

			final Label hackLabel = new Label(); // used to get out of the current loop from a (values)
			codeGenerator.emitter.emitInstanceof("lisp/extensions/type/Function2");
			codeGenerator.emitter.emitIfeq(hackLabel);
			codeGenerator.emitter.visitMethodLabel(hackLabel);
			codeGenerator.emitter.emitCheckcast("lisp/extensions/type/Function2");

			// get them in the right order [ ...fn, second =>
			codeGenerator.emitter.emitSwap();
			// get the held value
			codeGenerator.emitter.emitAload(0);
			codeGenerator.emitter.emitGetfield(codeGenerator.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");
			// now looks like ...fn, second, first
			// get them in the right order
			codeGenerator.emitter.emitSwap();
			// now looks like ...fn, first, second
			// now call nconc
			codeGenerator.emitter.emitInvokeinterface("lisp/extensions/type/Function2", "funcall",
					"(Ljava/lang/Object;Ljava/lang/Object;)", "Ljava/lang/Object;", true);
			// now we have the two list spliced, now stow away
			codeGenerator.emitter.emitCheckcast("lisp/common/type/ListStruct");
			codeGenerator.emitter.emitAload(0);
			codeGenerator.emitter.emitSwap();
			codeGenerator.emitter.emitPutfield(codeGenerator.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

			restOfList = restOfList.getRest();
		}

		// get the held value
		codeGenerator.emitter.emitAload(0);
		codeGenerator.emitter.emitGetfield(codeGenerator.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

		// now that we have the final list on the stack,
		//   we have to NIL-out the field to prevent sticky garbage
		codeGenerator.emitter.emitAload(0);
		codeGenerator.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		codeGenerator.emitter.emitPutfield(codeGenerator.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

		// now apply the function to the evaluated args
		codeGenerator.emitter.emitInvokeinterface("lisp/common/type/Function", "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", true);
	}
}
