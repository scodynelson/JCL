package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;

public class MultipleValueCallCodeGenerator {

	public static void genCodeMultipleValueCall(final IntermediateCodeGenerator icg, ListStruct list) {

		// evaluate the lambda, leaving an instance on the stack
		list = list.getRest();
		final Object fn = list.getFirst();
		icg.icgMainLoop(fn);

		// now process each of the arguments, leaving them on the stack
		list = list.getRest();

		// now stuff into a list. It's a bit tricky sense functions can return
		// objects or arrays of objects. Have to check at runtime
		final boolean firstPass = true;
		// make a private field to hold the resulting list
		final SymbolStruct<?> mvcFieldName = (SymbolStruct) GensymFunction.funcall("MVC_Field_" + System.currentTimeMillis());
		icg.emitter.newField(Opcodes.ACC_PRIVATE, mvcFieldName.toString(), "Llisp/common/type/ListStruct;", null, null);
		// initialize it to NIL
		icg.emitter.emitAload(0);
		icg.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		icg.emitter.emitPutfield(icg.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");
		while (!list.equals(NullStruct.INSTANCE)) {
			// this puts a value or value[] on the stack
			icg.icgMainLoop(list.getFirst(), true);
			icg.emitter.emitDup();
			// which is it?
			icg.emitter.emitInstanceof("[Ljava/lang/Object;");
			// one if by sea... (it is)
			final Label isntArray = new Label();
			final Label allDone = new Label();

			icg.emitter.emitIfeq(isntArray);  // jumps if it's NOT an array
			// so, make the array into a lisp list
			icg.emitter.emitCheckcast("[Ljava/lang/Object;");
			icg.emitter.emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			icg.emitter.emitGoto(allDone);
			// ....
			icg.emitter.visitMethodLabel(isntArray);
			// so, make the object into a lisp list
			icg.emitter.emitCheckcast("java/lang/Object");
			icg.emitter.emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "(Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			// now we have to splice it to the previous
			icg.emitter.visitMethodLabel(allDone);

			// now, just nconc the prior list to the current one
			// Don't forget to restore the result into the local field.
			//    stack now holds the current list
			// get the nconc function
			icg.emitter.emitGetstatic("lisp/extensions/type/CommonLispFunctions", "StdFunctions", "Llisp/extensions/type/CommonLispFunctions;");
			icg.emitter.emitGetfield("lisp/extensions/type/CommonLispFunctions", "NConc", "Llisp/common/function/NConc;");
			icg.emitter.emitDup();

			final Label hackLabel = new Label(); // used to get out of the current loop from a (values)
			icg.emitter.emitInstanceof("lisp/extensions/type/Function2");
			icg.emitter.emitIfeq(hackLabel);
			icg.emitter.visitMethodLabel(hackLabel);
			icg.emitter.emitCheckcast("lisp/extensions/type/Function2");

			// get them in the right order [ ...fn, second =>
			icg.emitter.emitSwap();
			// get the held value
			icg.emitter.emitAload(0);
			icg.emitter.emitGetfield(icg.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");
			// now looks like ...fn, second, first
			// get them in the right order
			icg.emitter.emitSwap();
			// now looks like ...fn, first, second
			// now call nconc
			icg.emitter.emitInvokeinterface("lisp/extensions/type/Function2", "funcall",
					"(Ljava/lang/Object;Ljava/lang/Object;)", "Ljava/lang/Object;", true);
			// now we have the two list spliced, now stow away
			icg.emitter.emitCheckcast("lisp/common/type/ListStruct");
			icg.emitter.emitAload(0);
			icg.emitter.emitSwap();
			icg.emitter.emitPutfield(icg.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

			list = list.getRest();
		}

		// get the held value
		icg.emitter.emitAload(0);
		icg.emitter.emitGetfield(icg.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

		// now that we have the final list on the stack,
		//   we have to NIL-out the field to prevent sticky garbage
		icg.emitter.emitAload(0);
		icg.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		icg.emitter.emitPutfield(icg.classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

		// now apply the function to the evaluated args
		icg.emitter.emitInvokeinterface("lisp/common/type/Function", "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", true);
	}
}
