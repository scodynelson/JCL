package jcl.compiler.real.icg.generator.specialoperator.special;

import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.allocation.LocalAllocation;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.ClosureCodeGenerator;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.SpecialVariableCodeGenerator;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaCodeGenerator implements CodeGenerator<LambdaStruct> {

	//TODO when checking bindings, in handling the init-forms, start with the just previous
	// bindings in the lambda list. Differs from how LET handles it

	@Autowired
	private ClosureCodeGenerator closureCodeGenerator;

	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final LambdaStruct input, final JavaClassBuilder classBuilder) {
		genCodeLambdaInContext(input, false, classBuilder);
	}

	private void genCodeLambdaInContext(final LambdaStruct lambdaStruct, final boolean inStaticContext, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();

		ListStruct javaSymbolName = NullStruct.INSTANCE;
		ListStruct lispSymbolName = NullStruct.INSTANCE;
		ListStruct documentation = NullStruct.INSTANCE;

		final String className = javaSymbolName.getRest().getFirst().toString().replace('.', '/');
		classBuilder.getClassNames().push(className);

		// now lispify it
		if (lispSymbolName.equals(NullStruct.INSTANCE)) {
			lispSymbolName = javaSymbolName;
		}
		final SymbolStruct<?> lispName = (SymbolStruct<?>) lispSymbolName.getRest().getFirst();
		//

		// compile the new function class
		final List<EnvironmentParameterBinding> bindingSetBody = lambdaStruct.getLambdaEnvironment().getLexicalBindings();

		final int numRequiredParams = countRequiredParams(bindingSetBody);

		// compile the new function class
		classBuilder.getEmitter().newClass(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, className, null, "lisp/common/function/FunctionBaseClass", null);

		// if this is from a compile-file, add the source file name
		final String fileName = classBuilder.getSourceFile().getRest().getFirst().toString();
		classBuilder.getEmitter().visitClassSource(fileName, null);

		String docString = lambdaStruct.getDocString().getAsJavaString();

		// add the documentation for the class
		if (!documentation.equals(NullStruct.INSTANCE)) {
			docString = documentation.getRest().getFirst().toString();
		}

		classBuilder.getEmitter().newAnnotation("Llisp/extensions/type/SourceFileAnnotation;", true);
		classBuilder.getEmitter().visitAnnotationValue("dateTime", new Date().toString());
		if (classBuilder.getSourceFile().equals(NullStruct.INSTANCE)) {
			classBuilder.getEmitter().visitAnnotationValue("sourceFile", "#<in-memory>");
		} else {
			classBuilder.getEmitter().visitAnnotationValue("sourceFile", fileName);
		}
		classBuilder.getEmitter().endAnnotation();

		// add the static initialization
		// Need to separate the static init and any static methods and fields in here
		classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL, "SYMBOL", "Llisp/common/type/Symbol;", null, null);

		// constructor();
		doConstructor(lambdaStruct, className, classBuilder);

		// Handle all of the binding information
		try {
			classBuilder.getBindingStack().push(lambdaStruct.getLambdaEnvironment());

			// now create the check arguments method that's used when safety > 1
			//-----------> checkArguments <--------------------
//            doCheckArguments(numRequiredParams);

			//---------> funcall <-----------
			// funcall method
			String funcallParams = "";
			for (final Binding<?> aBindingSetBody1 : bindingSetBody) {
				funcallParams += "Ljava/lang/Object;";
			}
			classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "funcall", '(' + funcallParams + ')', "Ljava/lang/Object;", null, null);

			// allocate and fill a closure if there is one defined
			closureCodeGenerator.generate(classBuilder.getBindingEnvironment(), classBuilder);

			// set up the free radicals - 1960's!!
			doFreeVariableSetup(classBuilder);

			// Beginning gen code for the body
			ListStruct funcallList = null;

			while (!NullStruct.INSTANCE.equals(funcallList)) {
				formGenerator.generate(funcallList.getFirst(), classBuilder);
				funcallList = funcallList.getRest();
				if (!NullStruct.INSTANCE.equals(funcallList)) {
					classBuilder.getEmitter().emitPop();
				}
			}
			// pop the closure if there was a new one
			undoClosureSetup(classBuilder.getBindingEnvironment(), classBuilder);

			// now we'return done..
			classBuilder.getEmitter().emitAreturn();
			classBuilder.getEmitter().endMethod();

			//------> apply <----------

			// Set up the apply(lisp.common.type.ListStruct) method
			classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", null, null);
			// Generate the JVM code to turn the list into an array. Then call the apply(Object[]) method
			// Local 1 is the list argument
			classBuilder.getEmitter().emitAload(0); // this

			// Now unfurl the arg list onto the stack and call the funcall method
			//...
			// Roll out the number of params defined for the fn - numParams
			for (final Binding<?> aBindingSetBody : bindingSetBody) {
				classBuilder.getEmitter().emitAload(1); // get the current value of arg list
				// get the car of the list
				classBuilder.getEmitter().emitInvokeinterface("lisp/common/type/ListStruct", "getCar", "()", "Ljava/lang/Object;", true);
				// get the cdr
				classBuilder.getEmitter().emitAload(1); // get the current value of arg list
				classBuilder.getEmitter().emitInvokeinterface("lisp/common/type/ListStruct", "rest", "()", "Llisp/common/type/ListStruct;", true);
				classBuilder.getEmitter().emitAstore(1);
			}
//*****
			classBuilder.getEmitter().emitInvokevirtual(className, "funcall", '(' + funcallParams + ')', "Ljava/lang/Object;", false);
			classBuilder.getEmitter().emitAreturn();
			classBuilder.getEmitter().endMethod();

			// put the static components into the class definition
			// putting it here gives us the ability to put the load-time-value forms
			// in the class after we know what they are
			// 1. gather the LTV property from the lambda environment
			// 2. if non-NIL,
			// 2a. for each field name, add field, gen code for lambda,
			//     add code to init the field
			// 2b. again
			doStaticInit(className, lispName, classBuilder);

			classBuilder.getEmitter().endClass();
		} finally {
			classBuilder.getBindingStack().pop();
		}

		// ** finished compiling the new lambda class **
		// Now make an instance and leave it on the stack
		if (!classBuilder.getEmitter().isClassStackEmpty()) {
			// push new function object onto stack
			classBuilder.getEmitter().emitNew(className);
			classBuilder.getEmitter().emitDup();
			// call constructor
			if (inStaticContext) {
				// here we have to init the instance but there's no outer context
				// so we put a null on the stack since the load-time-value runs in the
				// global environment
				classBuilder.getEmitter().emitAconst_null();
			} else {
				// get whatever is on top of the closure stack
				// get this
				classBuilder.getEmitter().emitAload(0);
				// get the closure stack
				classBuilder.getEmitter().emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			}
			classBuilder.getEmitter().emitInvokespecial(className, "<init>", "(Llisp/extensions/type/Closure;)", "V", false);
		}

		// pop off the current class name, we're done with it
		classBuilder.getClassNames().pop();
	}

	private void doStaticInit(final String className, final SymbolStruct<?> lispName, final JavaClassBuilder classBuilder) {
		// static init
		classBuilder.getEmitter().newMethod(Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC, "<clinit>", "()", "V", null, null);
		// init the SYMBOL field with the LISP name symbol
		if (lispName.getSymbolPackage() != null) {
			specialVariableCodeGenerator.generate(lispName, classBuilder);
		} else {
			//make the symbol
			classBuilder.getEmitter().emitLdc(lispName.toString());
			// make it into a Lisp string
			classBuilder.getEmitter().emitInvokestatic("lisp/common/type/String$Factory", "newInstance", "(Ljava/lang/CharSequence;)", "Llisp/common/type/String;", false);
			// now create the symbol
			classBuilder.getEmitter().emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Llisp/common/type/String;)", "Llisp/common/type/Symbol;", false);
		}
		classBuilder.getEmitter().emitPutstatic(className, "SYMBOL", "Llisp/common/type/Symbol;");

		// Creating and initializing any necessary load-time-values
		final LambdaEnvironment env = (LambdaEnvironment) classBuilder.getBindingEnvironment();

		// see if we have to add any static fields for load-time-value
		final List<LoadTimeValue> ltvList = env.getLoadTimeValues();
		// ltvList is a plist of the field names and lambda forms
		for (final LoadTimeValue loadTimeValue : ltvList) {
			final UUID uniqueLTVId = loadTimeValue.getUniqueLTVId();
			final String fldName = "LOAD_TIME_VALUE" + uniqueLTVId;
			// add the field
			classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL,
					fldName, "Ljava/lang/Object;", null, null);
			// now get down to the function
			// gen code for the function

//			genCodeLambdaInContext(codeGenerator, loadTimeValue.getValue(), true, classBuilder);
			// now there's an instance of the function on the stack, call it
			classBuilder.getEmitter().emitInvokeinterface("lisp/extensions/type/Function0", "funcall", "()", "Ljava/lang/Object;", true);
			// now put the value into the static field
			classBuilder.getEmitter().emitPutstatic(className, fldName, "Ljava/lang/Object;");
		}
		// all done
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();
	}

	private static int countRequiredParams(final List<EnvironmentParameterBinding> bindingSetBody) {
		int countRequired = 0;
		// go through the list counting the usage :required entries
		for (final Binding<?> binding : bindingSetBody) {
			if (binding instanceof RequiredBinding) {
				countRequired++;
			} else {
				break;
			}
		}
		return countRequired;
	}

	private static void doConstructor(final LambdaStruct lambdaStruct, final String className, final JavaClassBuilder classBuilder) {
		// The basic constructor used when compiling a top-level lambda
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitAconst_null();
		classBuilder.getEmitter().emitInvokespecial(className, "<init>", "(Llisp/extensions/type/Closure;)", "V", false);  // this(null);
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();

		// This method is called when the compiler thinks there's a closure around
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "<init>", "(Llisp/extensions/type/Closure;)", "V", null, null);
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitInvokespecial("lisp/common/function/FunctionBaseClass", "<init>", "()", "V", false);  // super();
		//store the closure if one is passed in
		final Label isNull = new Label();
		classBuilder.getEmitter().emitAload(1);
		classBuilder.getEmitter().emitIfnull(isNull);
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitAload(1);
//        emitter.emitInvokevirtual("lisp/common/function/FunctionBaseClass", "addClosure", "(Llisp/extensions/type/Closure;)Llisp/extensions/type/Closure;");
		classBuilder.getEmitter().emitInvokevirtual(className, "addClosure", "(Llisp/extensions/type/Closure;)", "Llisp/extensions/type/Closure;", false);
		classBuilder.getEmitter().emitPop();
		classBuilder.getEmitter().visitMethodLabel(isNull);
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();
	}

	private static void undoClosureSetup(final Environment environment, final JavaClassBuilder classBuilder) {
		final Closure closureSetBody = environment.getClosure();
		final int numParams = closureSetBody.getBindings().size() - 1; // remove :closure and (:depth . n) from contention
		if (numParams > 0) {
			// keep a copy of the 'this' reference
			classBuilder.getEmitter().emitAload(0);
			// blow up the current closure
			classBuilder.getEmitter().emitInvokespecial("lisp/common/function/FunctionBaseClass", "popClosure", "()", "Llisp/extensions/type/Closure;", false);
			classBuilder.getEmitter().emitPop();
		}
	}

	/**
	 * For Symbol-table, for each entry where the scope is :dynamic and the binding is :free,
	 * gen code to retrieve the global variable and store it locally for easy reference. Once
	 * it is stored, other code referencing that symbol can just pick up the symbol from a local
	 * variable.
	 * The other aspect of dealing with special variables is that they have to be bound in the
	 * environment and unbound at the end. This necessitates a try-finally block. The same code is
	 * used in the LET form.
	 *
	 * @param classBuilder
	 * 		classBuilder
	 */
	private void doFreeVariableSetup(final JavaClassBuilder classBuilder) {
		//-- get the symbol-table
		final SymbolTable symbolTable = classBuilder.getBindingEnvironment().getSymbolTable();
		// Now iterate over the entries, looking for ones to allocate
		final List<SymbolLocalBinding> dynamicLocalBindings = symbolTable.getDynamicLocalBindings();
		final List<SymbolEnvironmentBinding> dynamicEnvironmentBindings = symbolTable.getDynamicEnvironmentBindings();

		for (final SymbolEnvironmentBinding symbolEnvironmentBinding : dynamicEnvironmentBindings) {
			final Environment environment = symbolEnvironmentBinding.getBinding();
			final SymbolStruct<?> sym = symbolEnvironmentBinding.getSymbolStruct();
			final Optional<SymbolLocalBinding> dynamicLocalBinding = environment.getSymbolTable().getDynamicLocalBinding(sym);
			if (dynamicLocalBinding.isPresent()) {
				dynamicLocalBindings.add(dynamicLocalBinding.get());
			}
		}

		for (final SymbolLocalBinding binding : dynamicLocalBindings) {
			// (symbol :allocation ... :binding ... :scope ... :type ...)
			// (:allocation ... :binding ... :scope ... :type ...)
			// for free and dynamic
			// get the local variable slot
			final LocalAllocation alloc = binding.getAllocation();
			// (:local . n)
			final int slot = alloc.getPosition();
			// now gen some code (whew)
			// gen code to either intern a symbol or call make-symbol if uninterned
			final SymbolStruct<?> symbol = binding.getSymbolStruct();
			if (symbol.getSymbolPackage() == null) {
				final String name = symbol.getName();
				// have to gen a make-symbol
				classBuilder.getEmitter().emitLdc(name);
				classBuilder.getEmitter().emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Ljava/lang/String;)", "Llisp/common/type/Symbol;", false);
			} else {
				specialVariableCodeGenerator.generate(symbol, classBuilder);
			}
			// store the symbol in the indicated local variable
			classBuilder.getEmitter().emitAstore(slot);
		}
	}

	private static Closure findNearestClosure(final Environment bindingEnv) {
		// get the current closure
		final Closure closure = bindingEnv.getClosure();
		if (closure != null) {
			// return the closure. It's an error if there's no depth value
			return closure;
		} else {
			final Environment parent = bindingEnv.getParent();
			// (:Parent ...)
			if (parent.equals(Environment.NULL)) {
				return null;
			} else {
				// go up to the top if necessary
				return findNearestClosure(parent);
			}
		}
	}

	private static void doCheckArguments(final int numParams, final JavaClassBuilder classBuilder) {
		// Create a basic checkArguments method to do some checking.
		// This will be more complex as the compiler gets smarter
		//--------> checkArguments <-------------
		final int checkArgsTestCtr = 0;
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "checkArguments", "(Llisp/common/type/ListStruct;)", "Llisp/common/type/Boolean;", null, null);
		// the most basic check is the number of arguments
		classBuilder.getEmitter().emitAload(1); // get the list argument
		classBuilder.getEmitter().emitInvokeinterface("java/util/Collection", "size", "()", "I", true);
		classBuilder.getEmitter().emitLdc(numParams);
		final Label label = new Label();
		classBuilder.getEmitter().emitIf_icmpeq(label);
		//throw an exception
		classBuilder.getEmitter().emitNew("lisp/common/exceptions/FunctionException");
		classBuilder.getEmitter().emitDup();
		// 1st arg to fn Excp
		classBuilder.getEmitter().emitLdc("Wrong number of arguments to function. Should be " + numParams);
		// 2nd arg to fn Excp
		classBuilder.getEmitter().emitNew("lisp/common/exceptions/WrongNumberOfArgsException");
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitInvokespecial("lisp/common/exceptions/WrongNumberOfArgsException", "<init>", "()", "V", false);
		classBuilder.getEmitter().emitInvokespecial("lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;Ljava/lang/Throwable;)", "V", false);
		classBuilder.getEmitter().emitAthrow();
		//done with that test
		classBuilder.getEmitter().visitMethodLabel(label);
		// done
		classBuilder.getEmitter().emitGetstatic("lisp/common/type/T", "T", "Llisp/common/type/Symbol;");
		classBuilder.getEmitter().emitAreturn();
		classBuilder.getEmitter().endMethod();
	}
}
