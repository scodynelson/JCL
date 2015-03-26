package jcl.compiler.real.icg.generator.specialoperator.old.special;

import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.Stack;

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
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.specialoperator.old.simple.ClosureCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.old.simple.SpecialVariableCodeGenerator;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

//@Component
public class LambdaCodeGenerator implements CodeGenerator<LambdaStruct> {

	//TODO when checking bindings, in handling the init-forms, start with the just previous
	// bindings in the lambda list. Differs from how LET handles it

	//	@Autowired
	private ClosureCodeGenerator closureCodeGenerator;

	//	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	//	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final LambdaStruct input, final JavaClassBuilder classBuilder) {
		genCodeLambdaInContext(input, false, classBuilder);
	}

	private void genCodeLambdaInContext(final LambdaStruct lambdaStruct, final boolean inStaticContext, final JavaClassBuilder classBuilder) {

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

		final ClassDef currentClass = new ClassDef(className, "");
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		// compile the new function class
		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, className, null, "lisp/common/function/FunctionBaseClass", null);

		// if this is from a compile-file, add the source file name
		final String fileName = classBuilder.getSourceFile().getRest().getFirst().toString();
		cw.visitSource(fileName, null);

		String docString = lambdaStruct.getDocString().getAsJavaString();

		// add the documentation for the class
		if (!documentation.equals(NullStruct.INSTANCE)) {
			docString = documentation.getRest().getFirst().toString();
		}

		{
			final AnnotationVisitor av = cw.visitAnnotation("Llisp/extensions/type/SourceFileAnnotation;", true);
			currentClass.setAnnotationVisitor(av);
			av.visit("dateTime", new Date().toString());
			if (classBuilder.getSourceFile().equals(NullStruct.INSTANCE)) {
				av.visit("sourceFile", "#<in-memory>");
			} else {
				av.visit("sourceFile", fileName);
			}
			av.visitEnd();
		}

		// add the static initialization
		// Need to separate the static init and any static methods and fields in here
		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL, "SYMBOL", "Llisp/common/type/Symbol;", null, null);
			currentClass.setFieldVisitor(fv);

			fv.visitEnd();
		}

		// constructor();
		doConstructor(lambdaStruct, className, classBuilder);

		// Handle all of the binding information
		try {
			classBuilder.getBindingStack().push(lambdaStruct.getLambdaEnvironment());

			// now create the check arguments method that's used when safety > 1
			//-----------> checkArguments <--------------------
//            doCheckArguments(numRequiredParams);

			//---------> funcall <-----------
			String funcallParams = "";
			for (final Binding<?> aBindingSetBody1 : bindingSetBody) {
				funcallParams += "Ljava/lang/Object;";
			}
			// funcall method
			{
				final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "funcall", '(' + funcallParams + ')' + "Ljava/lang/Object;", null, null);
				currentClass.setMethodVisitor(mv);
				mv.visitCode();

				// allocate and fill a closure if there is one defined
				closureCodeGenerator.generate(classBuilder.getBindingEnvironment(), classBuilder);

				// set up the free radicals - 1960's!!
				doFreeVariableSetup(classBuilder, mv);

				// Beginning gen code for the body
				ListStruct funcallList = null;

				while (!NullStruct.INSTANCE.equals(funcallList)) {
					formGenerator.generate(funcallList.getFirst(), classBuilder);
					funcallList = funcallList.getRest();
					if (!NullStruct.INSTANCE.equals(funcallList)) {
						mv.visitInsn(Opcodes.POP);
					}
				}
				// pop the closure if there was a new one
				undoClosureSetup(classBuilder.getBindingEnvironment(), mv);

				// now we'return done..
				mv.visitInsn(Opcodes.ARETURN);

				mv.visitMaxs(-1, -1);
				mv.visitEnd();
			}

			//------> apply <----------

			// Set up the apply(lisp.common.type.ListStruct) method
			{
				final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "apply", "(Llisp/common/type/ListStruct;)Ljava/lang/Object;", null, null);
				currentClass.setMethodVisitor(mv);
				mv.visitCode();

				// Generate the JVM code to turn the list into an array. Then call the apply(Object[]) method
				// Local 1 is the list argument
				mv.visitVarInsn(Opcodes.ALOAD, 0); // this

				// Now unfurl the arg list onto the stack and call the funcall method
				//...
				// Roll out the number of params defined for the fn - numParams
				for (final Binding<?> aBindingSetBody : bindingSetBody) {
					mv.visitVarInsn(Opcodes.ALOAD, 1); // get the current value of arg list
					// get the car of the list
					mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/common/type/ListStruct", "getCar", "()Ljava/lang/Object;", true);
					// get the cdr
					mv.visitVarInsn(Opcodes.ALOAD, 1); // get the current value of arg list
					mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/common/type/ListStruct", "rest", "()Llisp/common/type/ListStruct;", true);
					mv.visitVarInsn(Opcodes.ASTORE, 1);
				}
//*****
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className, "funcall", '(' + funcallParams + ')' + "Ljava/lang/Object;", false);

				mv.visitInsn(Opcodes.ARETURN);

				mv.visitMaxs(-1, -1);
				mv.visitEnd();
			}

			// put the static components into the class definition
			// putting it here gives us the ability to put the load-time-value forms
			// in the class after we know what they are
			// 1. gather the LTV property from the lambda environment
			// 2. if non-NIL,
			// 2a. for each field name, add field, gen code for lambda,
			//     add code to init the field
			// 2b. again
			doStaticInit(className, lispName, classBuilder);

			cw.visitEnd();

			classStack.pop();
			if (!classStack.isEmpty()) {
				final ClassDef previousClassDef = classStack.peek();
				classBuilder.setCurrentClass(previousClassDef);
			}
		} finally {
			classBuilder.getBindingStack().pop();
		}

		{
			// ** finished compiling the new lambda class **
			// Now make an instance and leave it on the stack
			if (!classStack.isEmpty()) {
				final MethodVisitor mv = currentClass.getMethodVisitor();

				// push new function object onto stack
				mv.visitTypeInsn(Opcodes.NEW, className);
				mv.visitInsn(Opcodes.DUP);
				// call constructor
				if (inStaticContext) {
					// here we have to init the instance but there's no outer context
					// so we put a null on the stack since the load-time-value runs in the
					// global environment
					mv.visitInsn(Opcodes.ACONST_NULL);
				} else {
					// get whatever is on top of the closure stack
					// get this
					mv.visitVarInsn(Opcodes.ALOAD, 0);
					// get the closure stack
					mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/common/function/FunctionBaseClass", "getClosure", "()Llisp/extensions/type/Closure;", false);
				}
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, className, "<init>", "(Llisp/extensions/type/Closure;)V", false);
			}
		}

		// pop off the current class name, we're done with it
		classBuilder.getClassNames().pop();
	}

	private void doStaticInit(final String className, final SymbolStruct<?> lispName, final JavaClassBuilder classBuilder) {
		// static init
		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC, "<clinit>", "()V", null, null);
		currentClass.setMethodVisitor(mv);
		mv.visitCode();

		// init the SYMBOL field with the LISP name symbol
		if (lispName.getSymbolPackage() != null) {
			specialVariableCodeGenerator.generate(lispName, classBuilder);
		} else {
			//make the symbol
			mv.visitLdcInsn(lispName.toString());
			// make it into a Lisp string
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/common/type/String$Factory", "newInstance", "(Ljava/lang/CharSequence;)Llisp/common/type/String;", false);
			// now create the symbol
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/common/type/Symbol$Factory", "newInstance", "(Llisp/common/type/String;)Llisp/common/type/Symbol;", false);
		}
		mv.visitFieldInsn(Opcodes.PUTSTATIC, className, "SYMBOL", "Llisp/common/type/Symbol;");

		// Creating and initializing any necessary load-time-values
		final LambdaEnvironment env = (LambdaEnvironment) classBuilder.getBindingEnvironment();

		// see if we have to add any static fields for load-time-value
		final List<LoadTimeValue> ltvList = env.getLoadTimeValues();
		// ltvList is a plist of the field names and lambda forms
		for (final LoadTimeValue loadTimeValue : ltvList) {
			final String uniqueLTVId = loadTimeValue.getUniqueLTVId();
			final String fldName = "LOAD_TIME_VALUE" + uniqueLTVId;
			// add the field
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL, fldName, "Ljava/lang/Object;", null, null);
			currentClass.setFieldVisitor(fv);
			// now get down to the function
			// gen code for the function

//			genCodeLambdaInContext(loadTimeValue.getValue(), true, classBuilder);
			// now there's an instance of the function on the stack, call it
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/extensions/type/Function0", "funcall", "()Ljava/lang/Object;", true);
			// now put the value into the static field
			mv.visitFieldInsn(Opcodes.PUTSTATIC, className, fldName, "Ljava/lang/Object;");
			fv.visitEnd();
		}
		// all done
		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();
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
		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();

		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitInsn(Opcodes.DUP);
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, className, "<init>", "(Llisp/extensions/type/Closure;)V", false);  // this(null);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}

		// This method is called when the compiler thinks there's a closure around
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "(Llisp/extensions/type/Closure;)V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/common/function/FunctionBaseClass", "<init>", "()V", false);  // super();
			//store the closure if one is passed in
			final Label isNull = new Label();
			mv.visitVarInsn(Opcodes.ALOAD, 1);
			mv.visitJumpInsn(Opcodes.IFNULL, isNull);
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitVarInsn(Opcodes.ALOAD, 1);
//        emitter.emitInvokevirtual("lisp/common/function/FunctionBaseClass", "addClosure", "(Llisp/extensions/type/Closure;)Llisp/extensions/type/Closure;");
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className, "addClosure", "(Llisp/extensions/type/Closure;)Llisp/extensions/type/Closure;", false);
			mv.visitInsn(Opcodes.POP);
			mv.visitLabel(isNull);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
	}

	private static void undoClosureSetup(final Environment environment, final MethodVisitor mv) {
		final Closure closureSetBody = environment.getClosure();
		final int numParams = closureSetBody.getBindings().size() - 1; // remove :closure and (:depth . n) from contention
		if (numParams > 0) {
			// keep a copy of the 'this' reference
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			// blow up the current closure
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/common/function/FunctionBaseClass", "popClosure", "()Llisp/extensions/type/Closure;", false);
			mv.visitInsn(Opcodes.POP);
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
	 * @param mv
	 * 		methodVisitor
	 */
	private void doFreeVariableSetup(final JavaClassBuilder classBuilder, final MethodVisitor mv) {
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
				mv.visitLdcInsn(name);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/common/type/Symbol$Factory", "newInstance", "(Ljava/lang/String;)Llisp/common/type/Symbol;", false);
			} else {
				specialVariableCodeGenerator.generate(symbol, classBuilder);
			}
			// store the symbol in the indicated local variable
			mv.visitVarInsn(Opcodes.ASTORE, slot);
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
}
