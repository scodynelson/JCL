package jcl.compiler.real.icg.generator.specialoperator.old;

import java.util.UUID;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueCallCodeGenerator implements CodeGenerator<ListStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {

		// evaluate the lambda, leaving an instance on the stack
		ListStruct restOfList = input.getRest();
		final LispStruct fn = restOfList.getFirst();
		formGenerator.generate(fn, classBuilder);

		// now process each of the arguments, leaving them on the stack
		restOfList = restOfList.getRest();

		// now stuff into a list. It's a bit tricky sense functions can return
		// objects or arrays of objects. Have to check at runtime
		final boolean firstPass = true;
		// make a private field to hold the resulting list
		final SymbolStruct<?> mvcFieldName = new SymbolStruct<>("MVC_Field_" + UUID.randomUUID());
		classBuilder.getEmitter().newField(Opcodes.ACC_PRIVATE, mvcFieldName.toString(), "Llisp/common/type/ListStruct;", null, null);
		// initialize it to NIL
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		classBuilder.getEmitter().emitPutfield(classBuilder.getClassNames().peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			// this puts a value or value[] on the stack
			final boolean currentMV = classBuilder.isAllowMultipleValues();
			try {
				classBuilder.setAllowMultipleValues(true);
				formGenerator.generate(restOfList.getFirst(), classBuilder);
			} finally {
				classBuilder.setAllowMultipleValues(currentMV);
			}

			classBuilder.getEmitter().emitDup();
			// which is it?
			classBuilder.getEmitter().emitInstanceof("[Ljava/lang/Object;");
			// one if by sea... (it is)
			final Label isntArray = new Label();
			final Label allDone = new Label();

			classBuilder.getEmitter().emitIfeq(isntArray);  // jumps if it's NOT an array
			// so, make the array into a lisp list
			classBuilder.getEmitter().emitCheckcast("[Ljava/lang/Object;");
			classBuilder.getEmitter().emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			classBuilder.getEmitter().emitGoto(allDone);
			// ....
			classBuilder.getEmitter().visitMethodLabel(isntArray);
			// so, make the object into a lisp list
			classBuilder.getEmitter().emitCheckcast("java/lang/Object");
			classBuilder.getEmitter().emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "(Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			// now we have to splice it to the previous
			classBuilder.getEmitter().visitMethodLabel(allDone);

			// now, just nconc the prior list to the current one
			// Don't forget to restore the result into the local field.
			//    stack now holds the current list
			// get the nconc function
			classBuilder.getEmitter().emitGetstatic("lisp/extensions/type/CommonLispFunctions", "StdFunctions", "Llisp/extensions/type/CommonLispFunctions;");
			classBuilder.getEmitter().emitGetfield("lisp/extensions/type/CommonLispFunctions", "NConc", "Llisp/common/function/NConc;");
			classBuilder.getEmitter().emitDup();

			final Label hackLabel = new Label(); // used to get out of the current loop from a (values)
			classBuilder.getEmitter().emitInstanceof("lisp/extensions/type/Function2");
			classBuilder.getEmitter().emitIfeq(hackLabel);
			classBuilder.getEmitter().visitMethodLabel(hackLabel);
			classBuilder.getEmitter().emitCheckcast("lisp/extensions/type/Function2");

			// get them in the right order [ ...fn, second =>
			classBuilder.getEmitter().emitSwap();
			// get the held value
			classBuilder.getEmitter().emitAload(0);
			classBuilder.getEmitter().emitGetfield(classBuilder.getClassNames().peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");
			// now looks like ...fn, second, first
			// get them in the right order
			classBuilder.getEmitter().emitSwap();
			// now looks like ...fn, first, second
			// now call nconc
			classBuilder.getEmitter().emitInvokeinterface("lisp/extensions/type/Function2", "funcall",
					"(Ljava/lang/Object;Ljava/lang/Object;)", "Ljava/lang/Object;", true);
			// now we have the two list spliced, now stow away
			classBuilder.getEmitter().emitCheckcast("lisp/common/type/ListStruct");
			classBuilder.getEmitter().emitAload(0);
			classBuilder.getEmitter().emitSwap();
			classBuilder.getEmitter().emitPutfield(classBuilder.getClassNames().peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

			restOfList = restOfList.getRest();
		}

		// get the held value
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitGetfield(classBuilder.getClassNames().peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

		// now that we have the final list on the stack,
		//   we have to NIL-out the field to prevent sticky garbage
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		classBuilder.getEmitter().emitPutfield(classBuilder.getClassNames().peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

		// now apply the function to the evaluated args
		classBuilder.getEmitter().emitInvokeinterface("lisp/common/type/Function", "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", true);
	}
}
