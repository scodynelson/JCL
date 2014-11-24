package jcl.compiler.real.icg.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.real.environment.Binding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Opcodes;

import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.Vector;

public class LambdaCodeGenerator implements CodeGenerator<ListStruct> {

	//TODO when checking bindings, in handling the init-forms, start with the just previous
	// bindings in the lambda list. Differs from how LET handles it

	public static final LambdaCodeGenerator INSTANCE = new LambdaCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		genCodeLambdaInContext(codeGenerator, input, false);
	}

	private static void genCodeLambdaInContext(final IntermediateCodeGenerator icg, final ListStruct list, final boolean inStaticContext) {

		//--------
		// get the class name out of the list
		ListStruct decl = (ListStruct) list.getRest().getFirst();
		// (declare (mumble...) (more-mumble...))
		decl = decl.getRest();
		// ((mumble...) (more-mumble...))

		ListStruct javaSymbolName = NullStruct.INSTANCE;
		ListStruct lispSymbolName = NullStruct.INSTANCE;
		ListStruct documentation = NullStruct.INSTANCE;

		final List<LispStruct> declJavaList = decl.getAsJavaList();
		for (final LispStruct lispStruct : declJavaList) {
			final ListStruct element = (ListStruct) lispStruct;

			final LispStruct first = element.getFirst();
			if (Declaration.JAVA_CLASS_NAME.equals(first)) {
				javaSymbolName = element;
			}
			if (Declaration.LISP_NAME.equals(first)) {
				lispSymbolName = element;
			}
			if (Declaration.DOCUMENTATION.equals(first)) {
				documentation = element;
			}
		}

		final String className = javaSymbolName.getRest().getFirst().toString().replace('.', '/');
		icg.classNames.push(className);

		// now lispify it
		if (lispSymbolName.equals(NullStruct.INSTANCE)) {
			lispSymbolName = javaSymbolName;
		}
		final SymbolStruct<?> lispName = (SymbolStruct) lispSymbolName.getRest().getFirst();
		//

		// compile the new function class
		final Vector<String> interfaces = new Vector<>();
		interfaces.add("lisp/common/type/CompiledFunction");
		if (icg.MacroLambda) {
			interfaces.add("lisp/common/type/MacroFunction");
			icg.MacroLambda = false;
		}
		final List<Binding> bindingSetBody = ((Environment) list.getFirst()).getBindings();

		final int numParams = bindingSetBody.size();
		if (numParams <= 11) {
			interfaces.add("lisp/extensions/type/Function" + numParams);
		}

		final int numRequiredParams = icg.countRequiredParams(bindingSetBody);

		// compile the new function class
		icg.emitter.newClass(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, className, null,
				"lisp/common/function/FunctionBaseClass",
				interfaces.toArray(new String[1]));

		final SymbolStruct<?> docUID = new SymbolStruct<>("docUID_" + UUID.randomUUID());

		// if this is from a compile-file, add the source file name
		final String fileName = icg.sourceFile.getRest().getFirst().toString();
		icg.emitter.visitClassSource(fileName, null);

		String docString = "";

		// add the documentation for the class
		if (!documentation.equals(NullStruct.INSTANCE)) {
			docString = documentation.getRest().getFirst().toString();
		}

		icg.emitter.newAnnotation("Llisp/system/documentation/DocStringAnn;", true);
		icg.emitter.visitAnnotationValue("docUID", docUID.toString());
		icg.emitter.visitAnnotationValue("docString", docString);
		icg.emitter.visitAnnotationValue("generated", System.currentTimeMillis());
		icg.emitter.visitAnnotationValue("javaName", className);
		icg.emitter.endAnnotation();

		icg.emitter.newAnnotation("Llisp/extensions/type/SourceFileAnnotation;", true);
		icg.emitter.visitAnnotationValue("dateTime", new Date().toString());
		if (icg.sourceFile.equals(NullStruct.INSTANCE)) {
			icg.emitter.visitAnnotationValue("sourceFile", "#<in-memory>");
		} else {
			icg.emitter.visitAnnotationValue("sourceFile", fileName);
		}
		icg.emitter.endAnnotation();

		//Add the UID to the class so we can find our DOM later.
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL,
				"DOCUMENTATION_UID", "Ljava/lang/String;", null, docUID.toString());

		// add the static initialization
		// Need to separate the static init and any static methods and fields in here
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL,
				"SYMBOL", "Llisp/common/type/Symbol;", null, null);

		// constructor();
		icg.doConstructor(list, className);

		// Handle all of the binding information
		try {
			icg.bindingEnvironment = icg.bindingStack.push((Environment) list.getFirst());

			// now create the check arguments method that's used when safety > 1
			//-----------> checkArguments <--------------------
//            doCheckArguments(numRequiredParams);

			//---------> funcall <-----------
			// funcall method
			String funcallParams = "";
			for (final Binding aBindingSetBody1 : bindingSetBody) {
				funcallParams += "Ljava/lang/Object;";
			}
			icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "funcall", '(' + funcallParams + ')', "Ljava/lang/Object;", null, null);

			// allocate and fill a closure if there is one defined
			icg.doClosureSetup(icg.bindingEnvironment);

			// set up the free radicals - 1960's!!
			icg.doFreeVariableSetup();

			// Beginning gen code for the body
			final List<LispStruct> copyListJavaList = list.getAsJavaList();
			final ListStruct copyList = ListStruct.buildProperList(copyListJavaList);
			ListStruct funcallList = copyList.getRest().getRest();

			while (!NullStruct.INSTANCE.equals(funcallList)) {
				icg.icgMainLoop(funcallList.getFirst());
				funcallList = funcallList.getRest();
				if (!NullStruct.INSTANCE.equals(funcallList)) {
					icg.emitter.emitPop();
				}
			}
			// pop the closure if there was a new one
			icg.undoClosureSetup(icg.bindingEnvironment);

			// now we'return done..
			icg.emitter.emitAreturn();
			icg.emitter.endMethod();

			//------> apply <----------

			// Set up the apply(lisp.common.type.ListStruct) method
			icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", null, null);
			// Generate the JVM code to turn the list into an array. Then call the apply(Object[]) method
			// Local 1 is the list argument
			icg.emitter.emitAload(0); // this

			// Now unfurl the arg list onto the stack and call the funcall method
			//...
			// Roll out the number of params defined for the fn - numParams
			for (final Binding aBindingSetBody : bindingSetBody) {
				icg.emitter.emitAload(1); // get the current value of arg list
				// get the car of the list
				icg.emitter.emitInvokeinterface("lisp/common/type/ListStruct", "getCar", "()", "Ljava/lang/Object;", true);
				// get the cdr
				icg.emitter.emitAload(1); // get the current value of arg list
				icg.emitter.emitInvokeinterface("lisp/common/type/ListStruct", "rest", "()", "Llisp/common/type/ListStruct;", true);
				icg.emitter.emitAstore(1);
			}
//*****
			icg.emitter.emitInvokevirtual(className, "funcall", '(' + funcallParams + ')', "Ljava/lang/Object;", false);
			icg.emitter.emitAreturn();
			icg.emitter.endMethod();

			// put the static components into the class definition
			// putting it here gives us the ability to put the load-time-value forms
			// in the class after we know what they are
			// 1. gather the LTV property from the lambda environment
			// 2. if non-NIL,
			// 2a. for each field name, add field, gen code for lambda,
			//     add code to init the field
			// 2b. again
			doStaticInit(icg, className, lispName);

			icg.emitter.endClass();
		} finally {
			icg.bindingStack.pop();
			icg.bindingEnvironment = icg.bindingStack.peek();
		}

		// ** finished compiling the new lambda class **
		// Now make an instance and leave it on the stack
		if (!icg.emitter.isClassStackEmpty()) {
			// push new function object onto stack
			icg.emitter.emitNew(className);
			icg.emitter.emitDup();
			// call constructor
			if (inStaticContext) {
				// here we have to init the instance but there's no outer context
				// so we put a null on the stack since the load-time-value runs in the
				// global environment
				icg.emitter.emitAconst_null();
			} else {
				// get whatever is on top of the closure stack
				// get this
				icg.emitter.emitAload(0);
				// get the closure stack
				icg.emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			}
			icg.emitter.emitInvokespecial(className, "<init>", "(Llisp/extensions/type/Closure;)", "V", false);
		}

		// pop off the current class name, we're done with it
		icg.classNames.pop();
	}

	private static void doStaticInit(final IntermediateCodeGenerator codeGenerator, final String className, final SymbolStruct<?> lispName) {
		// static init
		codeGenerator.emitter.newMethod(Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC, "<clinit>", "()", "V", null, null);
		// init the SYMBOL field with the LISP name symbol
		if (lispName.getSymbolPackage() != null) {
			codeGenerator.genCodeSpecialVariable(lispName);
		} else {
			//make the symbol
			codeGenerator.emitter.emitLdc(lispName.toString());
			// make it into a Lisp string
			codeGenerator.emitter.emitInvokestatic("lisp/common/type/String$Factory", "newInstance", "(Ljava/lang/CharSequence;)", "Llisp/common/type/String;", false);
			// now create the symbol
			codeGenerator.emitter.emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Llisp/common/type/String;)", "Llisp/common/type/Symbol;", false);
		}
		codeGenerator.emitter.emitPutstatic(className, "SYMBOL", "Llisp/common/type/Symbol;");

		// Creating and initializing any necessary load-time-values
		final Environment env = codeGenerator.bindingEnvironment;

		// see if we have to add any static fields for load-time-value
		final List<LoadTimeValue> ltvList = env.getLoadTimeValues();
		// ltvList is a plist of the field names and lambda forms
		for (final LoadTimeValue loadTimeValue : ltvList) {
			final String fldName = loadTimeValue.getName().getName();
			// add the field
			codeGenerator.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL,
					fldName, "Ljava/lang/Object;", null, null);
			// now get down to the function
			// gen code for the function

			genCodeLambdaInContext(codeGenerator, (ListStruct) loadTimeValue.getValue(), true);
			// now there's an instance of the function on the stack, call it
			codeGenerator.emitter.emitInvokeinterface("lisp/extensions/type/Function0", "funcall", "()", "Ljava/lang/Object;", true);
			// now put the value into the static field
			codeGenerator.emitter.emitPutstatic(className, fldName, "Ljava/lang/Object;");
		}
		// all done
		codeGenerator.emitter.emitReturn();
		codeGenerator.emitter.endMethod();
	}
}
