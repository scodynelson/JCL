package jcl.compiler.real.icg;

import java.util.List;
import java.util.Optional;

import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.allocation.LocalAllocation;
import jcl.compiler.real.environment.allocation.PositionAllocation;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.environment.binding.ClosureBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.apache.commons.collections4.CollectionUtils;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IntermediateCodeGenerator {

	private static final Logger LOGGER = LoggerFactory.getLogger(IntermediateCodeGenerator.class);

	public Object apply(final ListStruct argList) {
		return funcall(argList.getFirst());
	}

	public Object funcall(final Object lispFunc) {
		final JavaClassBuilder classBuilder = new JavaClassBuilder();
		icgMainLoop(lispFunc, classBuilder);
//        assert(closureDepth == 0) : "Unbalanced closure depth: " + closureDepth;
		return classBuilder.getEmitter().getClasses();
	}

	public void icgMainLoop(final Object obj, final boolean allowMultipleValues, final JavaClassBuilder classBuilder) {
		final boolean currentMV = classBuilder.isAllowMultipleValues();
		try {
			classBuilder.setAllowMultipleValues(allowMultipleValues);
			icgMainLoop(obj, classBuilder);
		} finally {
			classBuilder.setAllowMultipleValues(currentMV);
		}
	}

	public void icgMainLoop(final Object obj, final JavaClassBuilder classBuilder) {

		if (obj.equals(NullStruct.INSTANCE)) {
			NILCodeGenerator.INSTANCE.generate((NullStruct) obj, this, classBuilder);
		} else if (obj instanceof CharacterStruct) {
			CharacterCodeGenerator.INSTANCE.generate((CharacterStruct) obj, this, classBuilder);
		} else if (obj instanceof IntegerStruct) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerStruct) obj, this, classBuilder);
		} else if (obj instanceof FloatStruct) {
			FloatCodeGenerator.INSTANCE.generate((FloatStruct) obj, this, classBuilder);
		} else if (obj instanceof RatioStruct) {
			RatioCodeGenerator.INSTANCE.generate((RatioStruct) obj, this, classBuilder);
//		} else if (obj instanceof ComplexStruct) {
//			ComplexCodeGenerator.INSTANCE.generate((ComplexStruct) obj, this, classBuilder);
		} else if (obj instanceof SymbolStruct) {
			SymbolCodeGenerator.INSTANCE.generate((SymbolStruct) obj, this, classBuilder);
		} else if (obj instanceof ConsStruct) {
			ListCodeGenerator.INSTANCE.generate((ConsStruct) obj, this, classBuilder);
		} else {
			LOGGER.error("ICG: Found thing I can't generate code for: {}, class: {}", obj, obj.getClass().getName());
		}
	}

	/*
	 *********************************************************
	 * Generators
	 *********************************************************
	 */

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

	public static int genLocalSlot(final SymbolStruct<?> sym, final Environment binding) {
		// get the :bindings list
		// ((x :allocation ...) (y :allocation ...) ...)
		final Binding<?> symBinding = binding.getLexicalBinding(sym).get();
		// (:allocation ... :scope ... )
		// get the allocated slot for the symbol and put it on the stack
		return ((PositionAllocation) symBinding.getAllocation()).getPosition();
	}

	public void genCodeSpecialVariable(final SymbolStruct<?> sym, final JavaClassBuilder classBuilder) {
		if (sym.equals(NILStruct.INSTANCE)) {
			classBuilder.getEmitter().emitGetstatic("jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
		} else if (sym.equals(TStruct.INSTANCE)) {
			classBuilder.getEmitter().emitGetstatic("jcl/symbols/TStruct", "INSTANCE", "Ljcl/symbols/TStruct;");
		} else {
			// push current package
			emitSymbolPackage(sym, classBuilder);
			classBuilder.getEmitter().emitLdc(sym.getName());
			// invoke package.intern() - we may not have seen it before
			classBuilder.getEmitter().emitInvokeinterface("lisp/common/type/Package", "intern", "(Ljava/lang/String;)", "[Llisp/common/type/Symbol;", true);
			classBuilder.getEmitter().emitLdc(0);
			classBuilder.getEmitter().emitAaload();
		}
	}

	/**
	 * Emitter method for Java function EMIT-SYMBOL-PACKAGE and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param sym
	 * 		lisp.common.type.Sybmbol sym
	 */
	private Object emitSymbolPackage(final SymbolStruct<?> sym, final JavaClassBuilder classBuilder) {
		// There are optimizations for the standard packages
		if (sym.getSymbolPackage() != null) {
			final PackageStruct homePkgName = sym.getSymbolPackage();
			if (homePkgName.equals(GlobalPackageStruct.COMMON_LISP)) {
				classBuilder.getEmitter().emitGetstatic("lisp/common/type/Package", "CommonLisp", "Llisp/common/type/Package;");
			} else if (homePkgName.equals(GlobalPackageStruct.COMMON_LISP_USER)) {
				classBuilder.getEmitter().emitGetstatic("lisp/common/type/Package", "CommonLispUser", "Llisp/common/type/Package;");
			} else if (homePkgName.equals(GlobalPackageStruct.KEYWORD)) {
				classBuilder.getEmitter().emitGetstatic("lisp/common/type/Package", "Keyword", "Llisp/common/type/Package;");
			} else if (homePkgName.equals(GlobalPackageStruct.SYSTEM)) {
				classBuilder.getEmitter().emitGetstatic("lisp/common/type/Package", "System", "Llisp/common/type/Package;");
			} else {
				emitPackage(homePkgName.getName(), classBuilder);
			}
		} else {
			// no package
		}
		return NILStruct.INSTANCE;
	}

	/**
	 * Emitter method for Java function EMIT-PACKAGE and class for Lisp function
	 * for new Lisp compiler.
	 *
	 * @param name
	 * 		lisp.common.type.Package name
	 */
	private static Object emitPackage(final String name, final JavaClassBuilder classBuilder) {
//        Label label = new Label();
//        visitMethodLabel(label);
//        emitLine(++LineNumber, label);
		classBuilder.getEmitter().emitLdc(name);
		//String owner, String name, String descr
		classBuilder.getEmitter().emitInvokestatic("lisp/system/PackageImpl", "findPackage", "(Ljava/lang/String;)", "Llisp/common/type/Package;", false);
		return NILStruct.INSTANCE;
	}

	public static void doConstructor(final ListStruct list, final String className, final JavaClassBuilder classBuilder) {
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

	public static void undoClosureSetup(final Environment environment, final JavaClassBuilder classBuilder) {
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

	public static void doClosureSetup(final Environment environment, final JavaClassBuilder classBuilder) {
		final Closure closureSetBody = environment.getClosure();
		final int numParams = closureSetBody.getBindings().size(); // remove :closure and (:depth . n) from contention

		if (numParams > 0) {
			// keep a copy of the 'this' reference
			classBuilder.getEmitter().emitAload(0);
			classBuilder.getEmitter().emitDup();
			classBuilder.getEmitter().emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			classBuilder.getEmitter().emitLdc(numParams);
			classBuilder.getEmitter().emitInvokestatic("lisp/extensions/type/Closure$Factory", "newInstance", "(Llisp/extensions/type/Closure;I)", "Llisp/extensions/type/Closure;", false);
			// have a closure object on the stack
			//push it onto the closure stack
			classBuilder.getEmitter().emitInvokespecial("lisp/common/function/FunctionBaseClass", "addClosure", "(Llisp/extensions/type/Closure;)", "Llisp/extensions/type/Closure;", false);
			classBuilder.getEmitter().emitPop();
		}
		// get the :closure information
		final Closure closureStuff = environment.getClosure();
		final List<EnvironmentParameterBinding> bindings = environment.getLexicalBindings();
		// (:closure (:depth . n) (x ....) (y ....) ...)
		// if there is one, allocate the object
		if (CollectionUtils.isNotEmpty(bindings) && (closureStuff != null)) {
			// get the top closure object
			classBuilder.getEmitter().emitAload(0);
			classBuilder.getEmitter().emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			classBuilder.getEmitter().emitDup();
			// run the list of variables
			//TODO handle parameters that are special variables
			for (final Binding<?> binding : bindings) {
				final SymbolStruct<?> variable = binding.getSymbolStruct();
				final Optional<ClosureBinding> closureEntry = closureStuff.getBinding(variable);
				if (closureEntry.isPresent()) {
					// this entry is a closure
					// Since this is a lambda, it's a parameter. So put the value into the closure
					final PositionAllocation allocation = (PositionAllocation) binding.getAllocation();
					final int param = allocation.getPosition();
					// now where does it go
					final int position = closureEntry.get().getPosition();
					classBuilder.getEmitter().emitLdc(position); // index
					classBuilder.getEmitter().emitLdc(0);                   // nesting (current one)
					classBuilder.getEmitter().emitAload(param);      // value from the arg list
					classBuilder.getEmitter().emitInvokeinterface("lisp/extensions/type/Closure", "setBindingAt", "(IILjava/lang/Object;)", "V", true);
					// the closure is left on the stack
					// dup it for the rest loop, except the last time around
					classBuilder.getEmitter().emitDup();
				}
			}
			classBuilder.getEmitter().emitPop2();  // drop the remaining closure reference from the stack
			// for each variable, gen code to get the value and store in the closure
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
	 */
	public void doFreeVariableSetup(final JavaClassBuilder classBuilder) {
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
				genCodeSpecialVariable(symbol, classBuilder);
			}
			// store the symbol in the indicated local variable
			classBuilder.getEmitter().emitAstore(slot);
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

	public static int countRequiredParams(final List<EnvironmentParameterBinding> bindingSetBody) {
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
}
