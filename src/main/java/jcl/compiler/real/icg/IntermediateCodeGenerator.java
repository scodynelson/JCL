package jcl.compiler.real.icg;

import jcl.LispStruct;
import jcl.compiler.old.Emitter;
import jcl.compiler.real.environment.Allocation;
import jcl.compiler.real.environment.Binding;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.ClosureBinding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LocalAllocation;
import jcl.compiler.real.environment.PositionAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.lambdalist.RequiredBinding;
import jcl.compiler.real.icg.specialoperator.TagbodyCodeGenerator;
import jcl.characters.CharacterStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.ComplexStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.apache.commons.collections4.CollectionUtils;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Optional;
import java.util.Stack;

public class IntermediateCodeGenerator {

	private static final Logger LOGGER = LoggerFactory.getLogger(IntermediateCodeGenerator.class);

	public static final SymbolStruct<?> LAMBDA = SpecialOperator.LAMBDA_MARKER;
	public static final SymbolStruct<?> MACRO = SpecialOperator.MACRO_MARKER;

	// this is the current binding environment. It always matches the value
	// on top of the binding stack
	public Environment bindingEnvironment;
	// Whenever a binding environment is encountered, it is pushed on the stack and
	// bindingEnvironment is set to the new environment. When that binding is no
	// longer in force, the stack is popped and the value of bindingEnvironment is
	// set to the new top of stack
	public Stack<Environment> bindingStack;
	// make a stack of current class names
	public Stack<String> classNames;
	public Emitter emitter;
	public boolean allowMultipleValues = false;
	public Stack<Stack<TagbodyCodeGenerator.TagbodyLabel>> tagbodyStack;
	public ListStruct sourceFile = null;
	private int LineNumber = 0;

	/**
	 * the rest Lambda generated will be for a macro
	 */
	public boolean MacroLambda;

	public IntermediateCodeGenerator() {
//        initialize();
	}

	public void initialize() {
		MacroLambda = false;
		emitter = new Emitter();
		bindingEnvironment = Environment.NULL;
		bindingStack = new Stack<>();
		bindingStack.push(Environment.NULL);
		classNames = new Stack<>();
		TagbodyCodeGenerator.tagCounter = 0;
		allowMultipleValues = false;
		tagbodyStack = new Stack<Stack<TagbodyCodeGenerator.TagbodyLabel>>();
	}

	public Object apply(final ListStruct argList) {
		return funcall(argList.getFirst());
	}

	public Object funcall(final Object lispFunc) {
		initialize();
		icgMainLoop(lispFunc);
//        assert(closureDepth == 0) : "Unbalanced closure depth: " + closureDepth;
		return emitter.getClasses();
	}

	public void icgMainLoop(final Object obj, final boolean allowMultipleValues) {
		final boolean currentMV = this.allowMultipleValues;
		try {
			this.allowMultipleValues = allowMultipleValues;
			icgMainLoop(obj);
		} finally {
			this.allowMultipleValues = currentMV;
		}
	}

	public void icgMainLoop(final Object obj) {

		if (obj.equals(NullStruct.INSTANCE)) {
			NILCodeGenerator.INSTANCE.generate((NILStruct) obj, this);
		} else if (obj instanceof CharacterStruct) {
			CharacterCodeGenerator.INSTANCE.generate((CharacterStruct) obj, this);
		} else if (obj instanceof IntegerStruct) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerStruct) obj, this);
		} else if (obj instanceof FloatStruct) {
			FloatCodeGenerator.INSTANCE.generate((FloatStruct) obj, this);
		} else if (obj instanceof RatioStruct) {
			RatioCodeGenerator.INSTANCE.generate((RatioStruct) obj, this);
		} else if (obj instanceof ComplexStruct) {
			ComplexCodeGenerator.INSTANCE.generate((ComplexStruct) obj, this);
		} else if (obj instanceof SymbolStruct) {
			SymbolCodeGenerator.INSTANCE.generate((SymbolStruct) obj, this);
		} else if (obj instanceof ListStruct) {
			ListCodeGenerator.INSTANCE.generate((ListStruct) obj, this);
		} else {
			LOGGER.error("ICG: Found thing I can't generate code for: {}, class: {}", obj, obj.getClass().getName());
		}
	}

	/*
	 *********************************************************
	 * Generators
	 *********************************************************
	 */

	private Closure findNearestClosure(final Environment bindingEnv) {
		// get the current closure
		final Closure closure = bindingEnv.getEnvironmentClosure();
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
		final Binding symBinding = binding.getBinding(sym).get();
		// (:allocation ... :scope ... )
		// get the allocated slot for the symbol and put it on the stack
		return ((PositionAllocation) symBinding.getAllocation()).getPosition();
	}

	public <X extends LispStruct> void genCodeSpecialVariable(final SymbolStruct<X> sym) {
		if (sym.equals(NILStruct.INSTANCE)) {
			emitter.emitGetstatic("jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
		} else if (sym.equals(TStruct.INSTANCE)) {
			emitter.emitGetstatic("jcl/symbols/TStruct", "INSTANCE", "Ljcl/symbols/TStruct;");
		} else {
			// push current package
			emitSymbolPackage(sym);
			emitter.emitLdc(sym.getName());
			// invoke package.intern() - we may not have seen it before
			emitter.emitInvokeinterface("lisp/common/type/Package", "intern", "(Ljava/lang/String;)", "[Llisp/common/type/Symbol;", true);
			emitter.emitLdc(0);
			emitter.emitAaload();
		}
	}

	/**
	 * Emitter method for Java function EMIT-SYMBOL-PACKAGE and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param sym
	 * 		lisp.common.type.Sybmbol sym
	 */
	private Object emitSymbolPackage(final SymbolStruct<?> sym) {
		// There are optimizations for the standard packages
		if (sym.getSymbolPackage() != null) {
			final PackageStruct homePkgName = sym.getSymbolPackage();
			if (homePkgName.equals(GlobalPackageStruct.COMMON_LISP)) {
				emitter.emitGetstatic("lisp/common/type/Package", "CommonLisp", "Llisp/common/type/Package;");
			} else if (homePkgName.equals(GlobalPackageStruct.COMMON_LISP_USER)) {
				emitter.emitGetstatic("lisp/common/type/Package", "CommonLispUser", "Llisp/common/type/Package;");
			} else if (homePkgName.equals(GlobalPackageStruct.KEYWORD)) {
				emitter.emitGetstatic("lisp/common/type/Package", "Keyword", "Llisp/common/type/Package;");
			} else if (homePkgName.equals(GlobalPackageStruct.SYSTEM)) {
				emitter.emitGetstatic("lisp/common/type/Package", "System", "Llisp/common/type/Package;");
			} else {
				emitPackage(homePkgName);
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
	private Object emitPackage(final PackageStruct name) {
//        Label label = new Label();
//        visitMethodLabel(label);
//        emitLine(++LineNumber, label);
		emitter.emitLdc(name.getName());
		//String owner, String name, String descr
		emitter.emitInvokestatic("lisp/system/PackageImpl", "findPackage", "(Ljava/lang/String;)", "Llisp/common/type/Package;", false);
		return NILStruct.INSTANCE;
	}

	public void doConstructor(final ListStruct list, final String className) {
		// The basic constructor used when compiling a top-level lambda
		emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		emitter.emitAload(0);
		emitter.emitDup();
		emitter.emitAconst_null();
		emitter.emitInvokespecial(className, "<init>", "(Llisp/extensions/type/Closure;)", "V", false);  // this(null);
		emitter.emitReturn();
		emitter.endMethod();

		// This method is called when the compiler thinks there's a closure around
		emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "(Llisp/extensions/type/Closure;)", "V", null, null);
		emitter.emitAload(0);
		emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "<init>", "()", "V", false);  // super();
		//store the closure if one is passed in
		final Label isNull = new Label();
		emitter.emitAload(1);
		emitter.emitIfnull(isNull);
		emitter.emitAload(0);
		emitter.emitAload(1);
//        emitter.emitInvokevirtual("lisp/common/function/FunctionBaseClass", "addClosure", "(Llisp/extensions/type/Closure;)Llisp/extensions/type/Closure;");
		emitter.emitInvokevirtual(className, "addClosure", "(Llisp/extensions/type/Closure;)", "Llisp/extensions/type/Closure;", false);
		emitter.emitPop();
		emitter.visitMethodLabel(isNull);
		emitter.emitReturn();
		emitter.endMethod();
	}

	public void undoClosureSetup(final Environment environment) {
		final Closure closureSetBody = environment.getEnvironmentClosure();
		final int numParams = closureSetBody.getBindings().size() - 1; // remove :closure and (:depth . n) from contention
		if (numParams > 0) {
			// keep a copy of the 'this' reference
			emitter.emitAload(0);
			// blow up the current closure
			emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "popClosure", "()", "Llisp/extensions/type/Closure;", false);
			emitter.emitPop();
		}
	}

	public void doClosureSetup(final Environment environment) {
		final Closure closureSetBody = environment.getEnvironmentClosure();
		final int numParams = closureSetBody.getBindings().size(); // remove :closure and (:depth . n) from contention

		if (numParams > 0) {
			// keep a copy of the 'this' reference
			emitter.emitAload(0);
			emitter.emitDup();
			emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			emitter.emitLdc(numParams);
			emitter.emitInvokestatic("lisp/extensions/type/Closure$Factory", "newInstance", "(Llisp/extensions/type/Closure;I)", "Llisp/extensions/type/Closure;", false);
			// have a closure object on the stack
			//push it onto the closure stack
			emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "addClosure", "(Llisp/extensions/type/Closure;)", "Llisp/extensions/type/Closure;", false);
			emitter.emitPop();
		}
		// get the :closure information
		final Closure closureStuff = environment.getEnvironmentClosure();
		final List<Binding> bindings = environment.getBindings();
		// (:closure (:depth . n) (x ....) (y ....) ...)
		// if there is one, allocate the object
		if (CollectionUtils.isNotEmpty(bindings) && (closureStuff != null)) {
			// get the top closure object
			emitter.emitAload(0);
			emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			emitter.emitDup();
			// run the list of variables
			//TODO handle parameters that are special variables
			for (final Binding binding : bindings) {
				final SymbolStruct<?> variable = binding.getSymbolStruct();
				final Optional<ClosureBinding> closureEntry = closureStuff.getBinding(variable);
				if (closureEntry.isPresent()) {
					// this entry is a closure
					// Since this is a lambda, it's a parameter. So put the value into the closure
					final PositionAllocation allocation = (PositionAllocation) binding.getAllocation();
					final int param = allocation.getPosition();
					// now where does it go
					final int position = closureEntry.get().getPosition();
					emitter.emitLdc(position); // index
					emitter.emitLdc(0);                   // nesting (current one)
					emitter.emitAload(param);      // value from the arg list
					emitter.emitInvokeinterface("lisp/extensions/type/Closure", "setBindingAt", "(IILjava/lang/Object;)", "V", true);
					// the closure is left on the stack
					// dup it for the rest loop, except the last time around
					emitter.emitDup();
				}
			}
			emitter.emitPop2();  // drop the remaining closure reference from the stack
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
	public void doFreeVariableSetup() {
		//-- get the symbol-table
		final SymbolTable symbolTable = bindingEnvironment.getSymbolTable();
		// Now iterate over the entries, looking for ones to allocate
		for (final Binding binding : symbolTable.getBindings()) {
			// (symbol :allocation ... :binding ... :scope ... :type ...)
			// (:allocation ... :binding ... :scope ... :type ...)
			// for free and dynamic
			final SymbolBinding symbolBinding = (SymbolBinding) binding;
			if (symbolBinding.getBinding().equals(Environment.FREE)
					&& (symbolBinding.getScope() == Scope.DYNAMIC)) {
				// get the local variable slot
				final Allocation alloc = symbolBinding.getAllocation();
				// (:local . n)
				if (alloc instanceof LocalAllocation) {
					final LocalAllocation localAllocation = (LocalAllocation) alloc;
					final int slot = localAllocation.getPosition();
					// now gen some code (whew)
					// gen code to either intern a symbol or call make-symbol if uninterned
					final SymbolStruct<?> symbol = symbolBinding.getSymbolStruct();
					if (symbol.getSymbolPackage() == null) {
						final String name = symbol.getName();
						// have to gen a make-symbol
						emitter.emitLdc(name);
						emitter.emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Ljava/lang/String;)", "Llisp/common/type/Symbol;", false);
					} else {
						genCodeSpecialVariable(symbol);
					}
					// store the symbol in the indicated local variable
					emitter.emitAstore(slot);
				}
			}
		}
	}

	private void doCheckArguments(final int numParams) {
		// Create a basic checkArguments method to do some checking.
		// This will be more complex as the compiler gets smarter
		//--------> checkArguments <-------------
		final int checkArgsTestCtr = 0;
		emitter.newMethod(Opcodes.ACC_PUBLIC, "checkArguments", "(Llisp/common/type/ListStruct;)", "Llisp/common/type/Boolean;", null, null);
		// the most basic check is the number of arguments
		emitter.emitAload(1); // get the list argument
		emitter.emitInvokeinterface("java/util/Collection", "size", "()", "I", true);
		emitter.emitLdc(numParams);
		final Label label = new Label();
		emitter.emitIf_icmpeq(label);
		//throw an exception
		emitter.emitNew("lisp/common/exceptions/FunctionException");
		emitter.emitDup();
		// 1st arg to fn Excp
		emitter.emitLdc("Wrong number of arguments to function. Should be " + numParams);
		// 2nd arg to fn Excp
		emitter.emitNew("lisp/common/exceptions/WrongNumberOfArgsException");
		emitter.emitDup();
		emitter.emitInvokespecial("lisp/common/exceptions/WrongNumberOfArgsException", "<init>", "()", "V", false);
		emitter.emitInvokespecial("lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;Ljava/lang/Throwable;)", "V", false);
		emitter.emitAthrow();
		//done with that test
		emitter.visitMethodLabel(label);
		// done
		emitter.emitGetstatic("lisp/common/type/T", "T", "Llisp/common/type/Symbol;");
		emitter.emitAreturn();
		emitter.endMethod();
	}

	public int countRequiredParams(final List<Binding> bindingSetBody) {
		int countRequired = 0;
		// go through the list counting the usage :required entries
		for (final Binding binding : bindingSetBody) {
			if (binding instanceof RequiredBinding) {
				countRequired++;
			} else {
				break;
			}
		}
		return countRequired;
	}
}
