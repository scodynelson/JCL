package jcl.compiler.old;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.old.expander.MacroFunctionExpander;
import jcl.compiler.old.functions.AssocFunction;
import jcl.compiler.old.functions.CompileFunction;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.old.functions.GetPlist;
import jcl.compiler.old.symbol.DeclarationOld;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.old.symbol.SpecialOperatorOld;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.ComplexStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.numbers.RatioStruct;
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.DefstructSymbolStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;

import java.util.Stack;

public class IntermediateCodeGenerator {

	public static final SymbolStruct LAMBDA = SpecialOperatorOld.LAMBDA_MARKER;
	public static final SymbolStruct MACRO = SpecialOperatorOld.MACRO_MARKER;
	public static final SymbolStruct LET = SpecialOperatorOld.LET_MARKER;
	public static final SymbolStruct FLET = SpecialOperatorOld.FLET_MARKER;
	public static final SymbolStruct LABELS = SpecialOperatorOld.LABELS_MARKER;
	public static final SymbolStruct MACROLET = SpecialOperatorOld.MACROLET;
	public static final SymbolStruct DEFSTRUCT = SpecialOperatorOld.DEFSTRUCT;
	// this is the current binding environment. It always matches the value
	// on top of the binding stack
	private ListStruct bindingEnvironment;
	// Whenever a binding environment is encountered, it is pushed on the stack and
	// bindingEnvironment is set to the new environment. When that binding is no
	// longer in force, the stack is popped and the value of bindingEnvironment is
	// set to the new top of stack
	private Stack<ListStruct> bindingStack;
	// make a stack of current class names
	private Stack<String> classNames;
	private Emitter emitter;
	private int tagCounter;
	private boolean allowMultipleValues;
	private Stack<Stack<TagbodyLabel>> tagbodyStack;
	private ListStruct sourceFile = null;
	private int LineNumber = 0;
	/**
	 * the rest Lambda generated will be for a macro
	 */
	private boolean MacroLambda;

	/**
	 * Creates a new instance of IntermediateCodeGenerator
	 */
	public IntermediateCodeGenerator() {
//        initialize();
	}

	public void initialize() {
		MacroLambda = false;
		emitter = new Emitter();
		bindingEnvironment = NullStruct.INSTANCE;
		bindingStack = new Stack<>();
		bindingStack.push(NullStruct.INSTANCE);
		classNames = new Stack<>();
		tagCounter = 0;
		allowMultipleValues = false;
		tagbodyStack = new Stack<>();
	}

	public Object apply(ListStruct argList) {
		return funcall(argList.getFirst());
	}

	public Object funcall(Object lispFunc) {
		initialize();
		icgMainLoop((ListStruct) lispFunc);
//        assert(closureDepth == 0) : "Unbalanced closure depth: " + closureDepth;
		return emitter.getClasses();
	}

	private void icgMainLoop(Object obj, boolean allowMultipleValues) {
		boolean currentMV = this.allowMultipleValues;
		try {
			this.allowMultipleValues = allowMultipleValues;
			icgMainLoop(obj);
		} finally {
			this.allowMultipleValues = currentMV;
		}
	}

	private void icgMainLoop(final Object obj) {

		if (obj.equals(NullStruct.INSTANCE)) {
			genNIL();
		} else if (obj instanceof CharacterStruct) {
			genCharacterStructCode((CharacterStruct) obj);
		} else if (obj instanceof IntegerStruct) {
			genCodeInteger((IntegerStruct) obj);
		} else if (obj instanceof FloatStruct) {
			genCodeFloat((FloatStruct) obj);
		} else if (obj instanceof RatioStruct) {
			genCodeRatio((RatioStruct) obj);
		} else if (obj instanceof ComplexStruct) {
			genCodeComplex((ComplexStruct) obj);
		} else if (obj instanceof SymbolStruct) {
			genCodeSymbolValue((SymbolStruct) obj);
		} else if (obj instanceof ListStruct) {
			genCodeList((ListStruct) obj);
		} else {
			System.out.println("ICG: Found thing I can't generate code for: " + obj + ", class: " + obj.getClass().getName());
		}
	}

	/*
	 *********************************************************
	 * Generators
	 *********************************************************
	 */

	private void genNIL() {
		emitter.emitGetstatic("jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
	}

	private void genCharacterStructCode(final CharacterStruct characterStruct) {
		emitter.emitIconst(characterStruct.getCodePoint());
		emitter.emitInvokestatic("jcl/characters/CharacterStruct", "<init>", "(I)", "V", false);
	}

	private void genCodeInteger(final IntegerStruct integerStruct) {
		emitter.emitLdc(integerStruct.getBigInteger().toString());
		emitter.emitInvokestatic("jcl/numbers/IntegerStruct", "<init>", "(Ljava/lang/String;)", "V", false);
	}

	private void genCodeFloat(final FloatStruct floatStruct) {
		emitter.emitLdc(floatStruct.getBigDecimal().toString());
		emitter.emitInvokestatic("jcl/numbers/FloatStruct", "<init>", "(Ljava/lang/String;)", "V", false);
	}

	private void genCodeRatio(final RatioStruct ratioStruct) {
		emitter.emitLdc(ratioStruct.getBigFraction().getNumerator().toString());
		emitter.emitLdc(ratioStruct.getBigFraction().getDenominator().toString());
		emitter.emitInvokestatic("jcl/numbers/RatioStruct", "<init>", "(Ljava/lang/String;Ljava/lang/String;)", "V", false);
	}

	private void genCodeComplex(final ComplexStruct complexStruct) {
		final RealStruct real = complexStruct.getReal();
		final RealStruct imaginary = complexStruct.getImaginary();

		if (real instanceof IntegerStruct) {
			genCodeInteger((IntegerStruct) real);
		} else if (real instanceof FloatStruct) {
			genCodeFloat((FloatStruct) real);
		} else if (real instanceof RatioStruct) {
			genCodeRatio((RatioStruct) real);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		if (imaginary instanceof IntegerStruct) {
			genCodeInteger((IntegerStruct) imaginary);
		} else if (imaginary instanceof FloatStruct) {
			genCodeFloat((FloatStruct) imaginary);
		} else if (imaginary instanceof RatioStruct) {
			genCodeRatio((RatioStruct) imaginary);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		emitter.emitInvokestatic("jcl/numbers/ComplexStruct", "getInstance", "(Ljcl/numbers/RealStruct;Ljcl/numbers/RealStruct;)", "Ljcl/numbers/ComplexStruc;", false);
	}

	private void genCodeSymbolValue(final SymbolStruct<?> symbolStruct) {
		// must determine one of 4 options:
		// 1. this is in a closure that's local to the environment
		// => assoc on the closure property of the current env
		// 2. it is found in the symbol table and is a closure which must be found and accessed through the chain
		// => assoc on the symbol table property of the current environment
		// ==> allocation :closure, follow chain
		// 3. it is found in the symbol table and is a special variable. All work is done to the symbol itself
		// ==> free and dynamic
		// 4. the binding is completely local and allocated to a JVM local
		//    If there is no binding and this is special, it's really free!
		final ListStruct closure = AssocFunction.funcall(KeywordOld.Closure, bindingEnvironment.getRest());
		ListStruct entry = AssocFunction.funcall(symbolStruct, closure.getRest());
		if (entry.equals(NullStruct.INSTANCE)) {
			// set up for 2 or 3
			entry = EnvironmentAccessor.getSymbolInTable(bindingEnvironment, symbolStruct);
			// (:allocation ... :
			if (entry.equals(NullStruct.INSTANCE)) {
				// it's 4
				final ListStruct binding = EnvironmentAccessor.getBindingEnvironment(bindingEnvironment, symbolStruct, true);
				if (binding.equals(NullStruct.INSTANCE)) {
					// This is a truly free variable, check to make sure it's special
					// if not, issue a warning, then treat it as special
					if (!symbolStruct.isSpecial()) {
						System.out.println("; Warning: variable " + symbolStruct + " is assumed free");
					}
					genCodeSpecialSymbol(symbolStruct);
					emitter.emitInvokeinterface("lisp/common/type/Symbol", "getValue", "()", "Ljava/lang/Object;", true);
				} else {
					final int slot = genLocalSlot(symbolStruct, binding.getRest());
					emitter.emitAload(slot);
				}
			} else {
				// it's 2 or 3
				// check the scope, if :dynamic it's 3
				if (GetPlist.funcall(entry, KeywordOld.Scope).equals(KeywordOld.Dynamic)) {
					// it's number 3
					genCodeSpecialSymbol(symbolStruct);
					emitter.emitInvokeinterface("lisp/common/type/Symbol", "getValue", "()", "Ljava/lang/Object;", true);
				} else {
					// it's door number 2
					// get the allocation parameter
					final ConsStruct allocation = (ConsStruct) GetPlist.funcall(entry, KeywordOld.Allocation);
					// may be a lexical binding up a few levels
					if (allocation.getCar().equals(KeywordOld.Closure)) {
						// (:closure . #n#)
						final ListStruct parentScope = (ListStruct) allocation.getCdr();
						// now we have the environment where the closure is defined
						// so pick it up, get the nesting depth and the position
						final ListStruct parentClosure = AssocFunction.funcall(KeywordOld.Closure, parentScope.getRest());
						// (:closure (:depth . n) (x ...)...)
						final IntegerStruct parentDepth = (IntegerStruct) ((ConsStruct) AssocFunction.funcall(KeywordOld.Depth, parentClosure.getRest())).getCdr();
						// (:depth . n) => n
						final ListStruct parentEntry = AssocFunction.funcall(symbolStruct, parentClosure.getRest());
						// (x :position m :references n)
						final IntegerStruct position = (IntegerStruct) GetPlist.funcall(parentEntry.getRest(), KeywordOld.Position);
						// get the current closure depth if any
						int nesting = 0;

						// have to find the first closure with a :depth in it. That's
						// the one that will be on the stack of the current lambda. The difference of
						// the 2 depths is the nesting level.
						if (!closure.getRest().equals(NullStruct.INSTANCE)) {
							final IntegerStruct closureDepth = (IntegerStruct) ((ConsStruct) AssocFunction.funcall(KeywordOld.Depth, closure.getRest())).getCdr();
							nesting = closureDepth.getBigInteger().intValue() - parentDepth.getBigInteger().intValue();
						}
						// Whew!! Now we can gen some code
						// get this
						emitter.emitAload(0);
						// get the current closure
						emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
						// set up the constants for seeking
						emitter.emitLdc(position.getBigInteger().intValue());
						emitter.emitLdc(nesting);
						// now give chase up the chain
						emitter.emitInvokeinterface("lisp/extensions/type/Closure", "getBindingAt", "(II)", "Ljava/lang/Object;", true);
					} else {
						// go find it
						final ListStruct binding = EnvironmentAccessor.getBindingEnvironment(bindingEnvironment, symbolStruct, true);
						final int slot = genLocalSlot(symbolStruct, binding.getRest());
						emitter.emitAload(slot);
					}
				}
			}
		} else {
			// #1. it's in a local closure
			// get the position in the closure
			final IntegerStruct position = (IntegerStruct) GetPlist.funcall(entry.getRest(), KeywordOld.Position);
			// now get the object out of the current closure
			// get this
			emitter.emitAload(0);
			emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			emitter.emitLdc(position.getBigInteger().intValue());
			emitter.emitLdc(0);
			emitter.emitInvokeinterface("lisp/extensions/type/Closure", "getBindingAt", "(II)", "Ljava/lang/Object;", true);
		}
	}

	/**
	 * Looks up the symbol in the :symbol-table and retrieves the local JVM
	 * variable number. It generates code to fetch that symbol and put it on
	 * the stack.
	 */
	private void genCodeSpecialSymbol(SymbolStruct sym) {
		IntegerStruct theInt = EnvironmentAccessor.getSymbolAllocation(bindingEnvironment, sym);
		int slot;
		//****** this is a premature optimization. Good idea, but should wait for a new version of the compiler ****
		slot = 0; // forces it to do it the long way.
		// it may not be in one of the accessible lambdas, so do it the old fashioned way
		if (slot > 0) {
			// now put the ALoad in the instruction stream
			emitter.emitAload(slot);
			emitter.emitCheckcast("lisp/common/type/Symbol");
		} else {
			genCodeSpecialVariable(sym);
		}
	}

	private boolean bindingsContain(SymbolStruct sym) {
		return (getSymbolPList(bindingEnvironment, sym) != NullStruct.INSTANCE);
	}

	@SuppressWarnings("unchecked")
	private ListStruct findNearestClosure(ListStruct bindingEnv) {
		// get the current closure
		ListStruct closure = (ListStruct) AssocFunction.funcall(KeywordOld.Closure, bindingEnv.getRest());
		if ((closure != NullStruct.INSTANCE) && (closure.getRest() != NullStruct.INSTANCE)) {
			// return the closure. It's an error if there's no depth value
			return closure;
		} else {
			ListStruct parent = (ListStruct) AssocFunction.funcall(KeywordOld.Parent, bindingEnv.getRest());
			// (:Parent ...)
			if (parent.getRest() != NullStruct.INSTANCE) {
				// go up to the top if necessary
				return findNearestClosure((ListStruct) parent.getRest().getFirst());
			} else {
				return NullStruct.INSTANCE;
			}
		}
	}

	@SuppressWarnings("unchecked")
	private int genLocalSlot(SymbolStruct sym, ListStruct binding) {
		// get the :bindings list
		ListStruct binds = ((ListStruct) AssocFunction.funcall(KeywordOld.Bindings, binding)).getRest();
		// ((x :allocation ...) (y :allocation ...) ...)
		binds = ((ListStruct) AssocFunction.funcall(sym, binds)).getRest();
		// (:allocation ... :scope ... )
		// get the allocated slot for the symbol and put it on the stack
		ConsStruct alloc = (ConsStruct) GetPlist.funcall(binds, KeywordOld.Allocation);
		int slot = ((IntegerStruct) alloc.getCdr()).getBigInteger().intValue();
		return slot;
	}

	/**
	 * This method handles a simple tail recursion. Instead of looking up a function,
	 * either by symbol name or from the list of std CL functions, the method
	 * just sets up to call the enclosing function's funcall or apply method. Since the
	 * enclosing function is the current object, the method only generates an ALOAD 0 -
	 * the reference to 'this'
	 */
	private void genCodeTailRecursionSetup(SymbolStruct sym) {
		emitter.emitAload(0);
	}

	// the list is of the form (%tail-recursion fn-symbol arg...)
	private void genCodeTailRecursion(ListStruct list) {
		// drop the special operator
		list = list.getRest();
		// set up the proper function object (this)
		genCodeTailRecursionSetup((SymbolStruct) list.getFirst());
		// now set up the rest of the call just like any other fn call
		genCodeFunctionCall(list, false);
	}

	@SuppressWarnings("UseSpecificCatch")
	private void genCodeSymbolFunction(SymbolStruct sym) {
		// there are multiple ways to handle this
		// we add an optimization for calling a CL function
		// it becomes a static field reference instead of a runtime symbol lookup
		// +0 ->
		if (sym.getSymbolPackage() == GlobalPackageStruct.COMMON_LISP) {
			String fnFieldName = "FUNCTION NAME"; // TODO: CommonLispFunctions.getFieldName(sym.getName().toString());
			// get the type of the field as well...
			if (fnFieldName != null) {
//				CommonLispFunctions clf = CommonLispFunctions.StdFunctions; // TODO
				String canonicalName = null;
				try {
					canonicalName = "CANONICAL NAME"; // TODO: clf.getClass().getDeclaredField(fnFieldName).getType().getCanonicalName().toString();
					canonicalName = canonicalName.replace('.', '/');
					canonicalName = 'L' + canonicalName + ';';
				} catch (Exception ex) {
					System.out.println("Exception: " + ex);
				}
				String[] strs = fnFieldName.split("\\.");
				if (strs.length > 0) {
					fnFieldName = strs[strs.length - 1];
				}
				Label label = new Label();
				emitter.visitMethodLabel(label);
				emitter.emitGetstatic("lisp/extensions/type/CommonLispFunctions", "StdFunctions", "Llisp/extensions/type/CommonLispFunctions;");
				// +1 -> StdFns
				if (sym.getFunction() instanceof MacroFunctionExpander) {
					emitter.emitGetfield("lisp/extensions/type/CommonLispFunctions", fnFieldName, "Llisp/common/type/MacroFunction;");
				} else {
					emitter.emitGetfield("lisp/extensions/type/CommonLispFunctions", fnFieldName, canonicalName);//"Llisp/common/type/Function;");
				}
				// +1 -> fn
			} else {
				genGeneralSymbolFn(sym);
			}
		} else {
			genGeneralSymbolFn(sym);
		}
	}

	private void genCodeSpecialVariable(SymbolStruct sym) {
		// Some symbols are really special
		if (sym == null) { // TODO: DO NIL
			emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
//        } else if (sym == Symbol.T) { // TODO: DO T
//			emitter.emitGetstatic("lisp/common/type/T", "T", "Llisp/common/type/Symbol;");
		} else {
			// push current package
			emitSymbolPackage(sym);
			emitter.emitLdc(sym.getName().toString());
			// invoke package.intern() - we may not have seen it before
			emitter.emitInvokeinterface("lisp/common/type/Package", "intern", "(Ljava/lang/String;)", "[Llisp/common/type/Symbol;", true);
			emitter.emitLdc(0);
			emitter.emitAaload();
		}
		// leaves the symbol on the stack
	}

	/**
	 * Emitter method for Java function EMIT-SYMBOL-PACKAGE and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param sym lisp.common.type.Sybmbol sym
	 */
	private Object emitSymbolPackage(SymbolStruct sym) {
		// There are optimizations for the standard packages
		if (sym.getSymbolPackage() != null) {
			PackageStruct homePkgName = sym.getSymbolPackage();
			if (homePkgName == GlobalPackageStruct.COMMON_LISP) {
				emitter.emitGetstatic("lisp/common/type/Package", "CommonLisp", "Llisp/common/type/Package;");
			} else if (homePkgName == GlobalPackageStruct.COMMON_LISP_USER) {
				emitter.emitGetstatic("lisp/common/type/Package", "CommonLispUser", "Llisp/common/type/Package;");
			} else if (homePkgName == GlobalPackageStruct.KEYWORD) {
				emitter.emitGetstatic("lisp/common/type/Package", "Keyword", "Llisp/common/type/Package;");
			} else if (homePkgName == GlobalPackageStruct.SYSTEM) {
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
	 * @param name lisp.common.type.Package name
	 */
	private Object emitPackage(PackageStruct name) {
//        Label label = new Label();
//        visitMethodLabel(label);
//        emitLine(++LineNumber, label);
		emitter.emitLdc(name.getName().toString());
		//String owner, String name, String descr
		emitter.emitInvokestatic("lisp/system/PackageImpl", "findPackage", "(Ljava/lang/String;)", "Llisp/common/type/Package;", false);
		return NILStruct.INSTANCE;
	}

	private void genGeneralSymbolFn(SymbolStruct sym) {
		Label label = new Label();
		emitter.visitMethodLabel(label);
		genCodeSpecialVariable(sym);
		// invoke symbol.getFunction()
		emitter.emitInvokeinterface("lisp/common/type/Symbol", "getFunction", "()", "Llisp/common/type/Function;", true);
		// if the symbol has defined less than 12 params, we can say that it takes that number of args

	}

	private int lineNumber = 0;

	private void genCodeList(ListStruct list) {
		// see if there a CONS here - looking for a line #
//        if (list instanceof ConsStruct) {
//            int currNbr = ((ConsStruct)list).getLineNumber();
//            if (currNbr > lineNumber) {
//
//            }
//        }
		Object elt = list.getFirst();
		if (elt instanceof SymbolStruct) {
			// generally an application (foobar ...)
			if (elt instanceof SpecialOperatorOld) {
				genCodeSpecialForm(list);
			} else if (elt instanceof DeclarationOld) {
//                genCodeDeclare(list);
			} else if (formOptimizable(list)) {
				genOptimizedForm(list);
			} else {
				genCodeSymbolFunction((SymbolStruct) elt);
				genCodeFunctionCall(list,
						((elt == GlobalPackageStruct.COMMON_LISP.intern("FUNCALL").getSymbolStruct())
								|| (elt == GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbolStruct())));
			}
		} else if (elt instanceof ListStruct) {
			ListStruct first = (ListStruct) elt;
			ListStruct maybeLast = list.getRest();
			// could be ((%lambda bindings...) body) or
			// could be (((%lambda bindings...) body) ...args...)
			if (first.getFirst() instanceof SymbolStruct) {
				// it's ((%lambda bindings...) body)
				if (first.getFirst() == LAMBDA) {
					genCodeLambda(list);
				} else if (first.getFirst() == MACRO) {
					genCodeMacroLambda(list);
				} else if (first.getFirst() == LET) {
					genCodeLet(list);
				} else if (first.getFirst() == FLET) {
					genCodeFlet(list);
				} else if (first.getFirst() == LABELS) {
					genCodeLabels(list);
				} else if (first.getFirst() == MACROLET) {
					genCodeMacrolet(list);
				} else {
					System.out.println("It's something else, " + first);
				}
			} else {
				// assume it's (((%lambda bindings...) body) ...args...)
				genCodeList(first);
				genCodeFunctionCall(list, false);
			}
		}
	}

	private boolean formOptimizable(ListStruct list) {
		if (list.getFirst() == GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbolStruct()) {
			return true;
		} else {
			return false;
		}
	}

	private void genOptimizedForm(ListStruct list) {
		SymbolStruct sym = (SymbolStruct) list.getFirst();
		if (sym == GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbolStruct()) {
			ListStruct args = list.getRest();
			// gen the 2 arguments and leave their values on the stack
			icgMainLoop(args.getFirst());
			icgMainLoop(args.getRest().getFirst());
			// now gen the VM if test
			// just generate direct VM instructions for eq refs
			// get a uniquifier value
			Label trueLabel = new Label();
			Label endLabel = new Label();
			emitter.emitIf_acmpeq(trueLabel);
			// if not eq, then the value is NIL
			emitter.emitGetstatic("lisp/common/type/Boolean", "NIL", "Llisp/common/type/Symbol;");
			emitter.emitGoto(endLabel);
			emitter.visitMethodLabel(trueLabel);
			emitter.emitGetstatic("lisp/common/type/Boolean", "T", "Llisp/common/type/Symbol;");
			emitter.visitMethodLabel(endLabel);
		}
	}

	/**
	 * Function must already be on stack
	 * It selectively generates either a funcall or apply. The method prefers the funcall
	 * option when it can
	 * 1)determine that the function implements one of the FunctionN
	 * interfaces that is appropriate to the number of arguments, or
	 * 2) has access to the function class object and can determine if it supports
	 * a function method with the correct number of parameters, or
	 * 3) if it cannot determine either 1 or 2, it generates an apply.
	 */
	@SuppressWarnings("DeadBranch")
	private void genCodeFunctionCall(ListStruct list, boolean acceptsMultipleValues) {
		// +1 -> fn
		int count;
		int argsExistCt = 0;
		SymbolStruct theFnName = null;
		if (list.getFirst() instanceof SymbolStruct) {
			theFnName = (SymbolStruct) list.getFirst();
		}

		// drop leading function name or lambda
		list = list.getRest();
		int numParams = list.size();
		// +2 -> fn, fn
		// +1 -> fn
		// still have fn on stack
		// make function call - done when it can prove that there's a function that
		// supports the FunctionN interface
		boolean fnOk = false;
		if (theFnName != null) {
			// get the interfaces of the fn
			FunctionStruct theFn = theFnName.getFunction();
			if (theFn != null) {
				String ifName = "lisp.extensions.type.Function" + numParams;
				Class[] interfaces = theFn.getClass().getInterfaces();
				for (int index = 0; index < interfaces.length; index++) {
					if (ifName.equals(interfaces[index].getName())) {
						fnOk = true;
						break;
					}
				}
			}
		}

		// add a call to checkArguments if the compiler safety is other than 0
		if (false) { //(compilerSafety != 0) {
			// Dup the list and the object on the stack
			emitter.emitAload(1);  // put the list on the stack
			emitter.emitCheckcast("lisp/common/type/ListStruct");
			//
			emitter.emitInvokeinterface("lisp/common/type/Function", "checkArguments",
					"(Llisp/common/type/ListStruct;)", "Llisp/common/type/Boolean;", true);
			// throw away the result (it will throw an exception if something is wrong)
			emitter.emitPop();
			// now the stack is where it was a little while ago
		}
		// +1 -> fn
		if (fnOk) {
			// Now evaluate the arguments. Puts all of them on the stack
			while (list != NullStruct.INSTANCE) {
				icgMainLoop(list.getFirst());
				// check for multiple value returns
				// maybe this returned multiple values
				if (!(allowMultipleValues || acceptsMultipleValues)) {
					// call a short routine to handle a possible return,
					// leaving the value on top - except...
					// the routine messes with reg 0, you have to restore it
					// ... fnVal or fnVal[]
//                    emitter.emitJsr(mvfCheckStack.peek());
//                    // ... fnVal, this
//                    emitter.emitAstore(0);
					// ... fnVal
					Label outLabel = new Label();
					emitter.emitDup();
					emitter.emitInstanceof("[Ljava/lang/Object;");
					emitter.emitIfeq(outLabel);
					emitter.emitCheckcast("[Ljava/lang/Object;");
					emitter.emitLdc(0);
					emitter.emitAaload();
					emitter.visitMethodLabel(outLabel);
				}
				//TODO this isn't the best way to do this. Better if the compiler
				// knows all of the data flow.
				list = list.getRest();
			}
			// +numParams -> (fn), p, p, ...
			// if (numParams >= 0 || numParams <= 9) make funcall
			String paramsDesc;
			paramsDesc = "(";
			for (int i = 0; i < numParams; i++) {
				paramsDesc += "Ljava/lang/Object;";
			}
			paramsDesc += ")";
//*******
			emitter.emitInvokeinterface("lisp/extensions/type/Function" + numParams, "funcall", paramsDesc, "Ljava/lang/Object;", true);

			// +1 -> result
		} else {
			// apply
			// +1 -> fn
			count = 0;
			emitter.emitLdc(numParams);
			// +2 -> fn, numParams
			emitter.emitAnewarray("java/lang/Object");
			// +2 -> fn, the array
			while (list != NullStruct.INSTANCE) {
				emitter.emitDup();
				// +3 -> fn, array, array
				emitter.emitLdc(count);
				// +4 -> fn, array, array, index
				icgMainLoop(list.getFirst());
				// check for multiple value returns
				//TODO this isn't the best way to do this. Better if the compiler
				// knows all of the data flow.
				// check for multiple value returns
				// maybe this returned multiple values
				if (!(allowMultipleValues || acceptsMultipleValues)) {
					// call a short routine to handle a possible return,
					// leaving the value on top - except...
					// the routine messes with reg 0, you have to restore it
					// ... fnVal or fnVal[]
//                    emitter.emitJsr(mvaCheckStack.peek());
//                    // ... fnVal, this
//                    emitter.emitAstore(0);
					// ... fnVal
					Label outLabel = new Label();
					emitter.emitDup();
					emitter.emitInstanceof("[Ljava/lang/Object;");
					emitter.emitIfeq(outLabel);
					emitter.emitCheckcast("[Ljava/lang/Object;");
					emitter.emitLdc(0);
					emitter.emitAaload();
					emitter.visitMethodLabel(outLabel);
				}
				// +5 -> fn, array, array, index, value
				emitter.emitAastore();
				// +2 -> fn, array
				list = list.getRest();
				count++;
			}
			// +2 -> fn, array
			emitter.emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			// +2 -> fn, the list
			// Now if we have the list, if the compiler is set to safety > 0 - call checkArguments
			// Dup the list and the object on the stack
			if (false) { //(compilerSafety != 0) {
				// +2 -> fn, list
				// Dup the list and the object on the stack
				emitter.emitDup2(); // we need the arg list to still be there to be there
				// +4 -> fn, list, fn, list
				emitter.emitInvokeinterface("lisp/common/type/Function", "checkArguments", "(Llisp/common/type/ListStruct;)", "Llisp/common/type/Boolean;", true);
				// +3 -> fn, list, T
				// throw away the result (it will throw an exception if something is wrong)
				emitter.emitPop();
				// +2 -> fn, list
				// now the stack is where it was a little while ago
			}
			emitter.emitInvokeinterface("lisp/common/type/Function", "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", true);
			// maybe this returned multiple values
			// +1 -> result
		}
	}

	private void genCodeSpecialForm(ListStruct list) {
		if (list.getFirst() instanceof SymbolStruct) {
			SymbolStruct symName = (SymbolStruct) list.getFirst();

			// Determine the special form ) generate its code.
			if (symName == SpecialOperatorOld.BLOCK) {
				genCodeBlock(list);
			} else if (symName == SpecialOperatorOld.CATCH) {
				genCodeCatch(list);
//            } else if (symName == SpecialOperatorOld.DECLARE) {
//                genCodeDeclare(list);
			} else if (symName == SpecialOperatorOld.DEFSTRUCT) {
				genCodeDefstruct(list);
			} else if (symName == SpecialOperatorOld.EVAL_WHEN) {
				genCodeEvalWhen(list);
			} else if (symName == SpecialOperatorOld.FLET) {
				genCodeFlet(list);
			} else if (symName == SpecialOperatorOld.FUNCTION) {
				genCodeFunction(list);
			} else if (symName == SpecialOperatorOld.GO) {
				genCodeGo(list);
			} else if (symName == SpecialOperatorOld.IF) {
				genCodeIf(list);
			} else if (symName == SpecialOperatorOld.LAMBDA) {
				genCodeLambda(list);
			} else if (symName == SpecialOperatorOld.MACRO_LAMBDA) {
				genCodeMacroLambda(list);
			} else if (symName == SpecialOperatorOld.LABELS) {
				genCodeLabels(list);
			} else if (symName == SpecialOperatorOld.LOAD_TIME_VALUE) {
				genCodeLoadTimeValue(list);
			} else if (symName == SpecialOperatorOld.LOCALLY) {
				genCodeLocally(list);
			} else if (symName == SpecialOperatorOld.MACROLET) {
				genCodeMacrolet(list);
			} else if (symName == SpecialOperatorOld.MULTIPLE_VALUE_CALL) {
				genCodeMultipleValueCall(list);
			} else if (symName == SpecialOperatorOld.MULTIPLE_VALUE_PROG1) {
				genCodeMultipleValueProg1(list);
			} else if (symName == SpecialOperatorOld.PROGN) {
				genCodeProgn(list);
			} else if (symName == SpecialOperatorOld.PROGV) {
				genCodeProgv(list);
			} else if (symName == SpecialOperatorOld.QUOTE) {
				genCodeQuote(list);
			} else if (symName == SpecialOperatorOld.RETURN_FROM) {
				genCodeReturnFrom(list);
			} else if (symName == SpecialOperatorOld.SETQ) {
				genCodeSetq(list);
			} else if (symName == SpecialOperatorOld.SYMBOL_MACROLET) {
				genCodeSymbolMacrolet(list);
			} else if (symName == SpecialOperatorOld.TAGBODY) {
				genCodeTagbody(list);
			} else if (symName == SpecialOperatorOld.TAIL_RECURSION) {
				genCodeTailRecursion(list);
			} else if (symName == SpecialOperatorOld.THE) {
				genCodeThe(list);
			} else if (symName == SpecialOperatorOld.THROW) {
				genCodeThrow(list);
			} else if (symName == SpecialOperatorOld.UNWIND_PROTECT) {
				genCodeUnwindProtect(list);
			}
		} else {
			// handle when the car is a list - ((%lambda ....)... ) or ((%let...) ...)
//            System.out.println("Before call genCodeLambda: " + list);
//            if (list.getCar() instanceof ListStruct) {
//                //get car of the car of the list - (%lambda ...)
//                if (((ListStruct)list.getCar()).getCar() == LAMBDA) {
//                    genCodeLambda(list);
//                    //not done yet
//                }
//            }
		}
	}

	private void genCodeBlock(ListStruct list) {
		Label startTryBlock;                //The start of the try block
		Label catchBlock;                   //The start of the catch block
		Label continueBlock;                //Subsequent code after the try/catch construct
		Label ifBlock;                      //If block executed if this catches someone else's excepton

		startTryBlock = new Label();
		catchBlock = new Label();
		continueBlock = new Label();
		ifBlock = new Label();

		// Get rid of the BLOCK symbol
		list = list.getRest();
		SymbolStruct sym = (SymbolStruct) list.getFirst();
		list = list.getRest();

		// ... ,
		genCodeSpecialVariable(sym);
		// ..., sym

		emitter.emitGetstatic("lisp/system/TransferOfControl", "BLOCK", "Ljava/lang/String;");
		// ..., sym, BLOCK
		emitter.emitSwap();
		// ... , BLOCK, sym
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);


        /* Call icgMainLoop() for each expression in the PROGN call,
		 * and remove all but the last expression's value from the stack  */
		emitter.visitMethodLabel(startTryBlock);
		while (list != NullStruct.INSTANCE) {
			icgMainLoop(list.getFirst());
			list = list.getRest();
			if (list != NullStruct.INSTANCE) {
				emitter.emitNop();
				emitter.emitPop();
				emitter.emitNop();
			}
		}
		emitter.emitGoto(continueBlock);

		//Start catch block
		emitter.visitMethodLabel(catchBlock);
		// ..., throw_excep
		emitter.emitDup();
		// ..., throw_excep, throw_excep
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "isMine", "(Ljava/lang/Throwable;)", "Ljava/lang/Object;", false);
		// ..., throw_excep, result
		emitter.emitDup();
		// ..., throw_excep, result, result
		emitter.emitIfnull(ifBlock);
		//Else block start
		// ..., throw_excep, result
		emitter.emitSwap();
		// ..., result, throw_excep
		emitter.emitPop();
		// ..., result
		emitter.emitGoto(continueBlock);
		//Else block end

		//If block start
		emitter.visitMethodLabel(ifBlock);
		// ..., throw_excep, result
		emitter.emitSwap();
		// ..., result, throw_excep
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ..., result
		//If block end

		emitter.visitMethodLabel(continueBlock);

		emitter.visitTryCatchBlock(
				startTryBlock, //blockName + "_BlockA",
				catchBlock, //blockName + "_BlockB",
				catchBlock, //blockName + "_BlockB",
				"java/lang/Throwable");

		//Here is the finally code
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);

		emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}

	/**
	 * Implements the initial base code for a basis catch statement
	 *
	 * @author CC Shapiro
	 * @date 28 Jan 2006
	 */
	private void genCodeCatch(ListStruct list) {
		Object catchTag;                    //The first parameter to CATCH that must first be evaluated
		Label startTryBlock;                //The start of the try block
		Label catchBlock;                   //The start of the catch block
		Label continueBlock;                //Subsequent code after the try/catch construct
		Label ifBlock;                      //If block executed if this catches someone else's excepton

		// Burn off the special symbol (CATCH)
		list = list.getRest();

		//Get the catchTag and set up for runtime eval of the catchTag
		catchTag = list.getFirst();
		list = list.getRest();

		// ... ,
		icgMainLoop(catchTag);
		// ..., catchTag

		emitter.emitGetstatic("lisp/system/TransferOfControl", "CATCH", "Ljava/lang/String;");
		// ..., catchTag, CATCH
		emitter.emitSwap();
		// ... , CATCH, catchTag
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);

		//Create the exception table
		startTryBlock = new Label();
		catchBlock = new Label();
		continueBlock = new Label();
		ifBlock = new Label();

		//Mark the start of the try block
		emitter.visitMethodLabel(startTryBlock);

		//Evalute the rest of the list
		while (list != NullStruct.INSTANCE) {
			icgMainLoop(list.getFirst());
			list = list.getRest();
			if (list != NullStruct.INSTANCE) {
				emitter.emitPop();
			}
		}

		//If an exception wasn't thrown, go past the catch block to the finally block
		emitter.emitGoto(continueBlock);

		//Start the catch block
		emitter.visitMethodLabel(catchBlock);
		emitter.emitDup();
		// ..., throw_excep, throw_excep
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "isMine", "(Ljava/lang/Throwable;)", "Ljava/lang/Object;", false);
		// ..., throw_excep, result
		emitter.emitDup();
		// ..., throw_excep, result, result
		emitter.emitIfnull(ifBlock);
		//Else block start
		// ..., throw_excep, result
		emitter.emitSwap();
		// ..., result, throw_excep
		emitter.emitPop();
		// ..., result
		emitter.emitGoto(continueBlock);
		//Else block end

		//If block start
		emitter.visitMethodLabel(ifBlock);
		// ..., throw_excep, result
		emitter.emitSwap();
		// ..., result, throw_excep
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ..., result
		//If block end

		//Signify the remainder of the code block
		emitter.visitMethodLabel(continueBlock);

		emitter.visitTryCatchBlock(
				startTryBlock,
				catchBlock,
				catchBlock,
				"java/lang/Throwable");
		//"lisp/system/compiler/exceptions/ThrowException");

		//Here is the finally code
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);

		emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}

	/**
	 * This method processes all the info needed to create a new definition of struct.
	 * The arg to this method looks something like this for a struct named FOO that
	 * includes the struct named BAR:
	 * (%DEFSTRUCT (Defstruct12071904607348921 BAR FOO]) (A TYPE T) (B TYPE T))
	 * NOTE: Some important parts of this method:
	 * 1. javaName - the name a struct is known by in Java (e.g. Defstruct12071904607348921)
	 * 2. lispName - the name a struct in known by in Lisp (e.g. BAR)
	 * 3. includeName - the name of the struct that this struct includes (e.g. FOO - or may be null)
	 * When the parent struct was created we attached the javaName to the lispName symbol
	 * in the field defstructJavaName (with public getter and setter). Then we can get
	 * the included struct's javaName from the includeName symbol.
	 * 4. fieldList - this is the list of all the slot names and their types for this struct
	 */
	@SuppressWarnings("unchecked")
	private void genCodeDefstruct(ListStruct list) {
		//Chop off %defstruct part. We don't need it.
		ListStruct arguments = list.getRest();

		//Get the Java name of struct
		ListStruct classStuff = (ListStruct) arguments.getFirst();
		//now classStuff ~= (Defstruct12071907613439006 BAR FOO)

		SymbolStruct javaName = (SymbolStruct) classStuff.getFirst();
		classStuff = classStuff.getRest();
		//now classStuff ~= (BAR FOO) or just (BAR) if no include struct

		DefstructSymbolStruct lispName = (DefstructSymbolStruct) classStuff.getFirst();
		//cache the javaName with the lispName
		lispName.setJavaName(javaName.toString());

		classStuff = classStuff.getRest();
		//now classStuff ~= (FOO) or NIL
		DefstructSymbolStruct includeName = (DefstructSymbolStruct) classStuff.getFirst();

		LispStruct printer = arguments.getRest().getFirst();

		Object printerFunction;
		if (printer instanceof SymbolStruct && printer != NullStruct.INSTANCE) {
			printerFunction = printer;
		} else if (printer instanceof ListStruct && printer != NullStruct.INSTANCE) {
			printerFunction = CompileFunction.FUNCTION.funcall(printer);
		} else {
			printerFunction = null;
		}

		//Get field list.
		NumberStruct includedSlotNumber = (NumberStruct) arguments.getRest().getRest().getFirst();
		int includedSlotNumberAsInt = ((IntegerStruct) includedSlotNumber).getBigInteger().intValue();

		//Get field list.
		ListStruct fieldList = (ListStruct) arguments.getRest().getRest().getRest();
		//fieldList now ~= ((A TYPE T) (B TYPE T))

		//interface used by the struct impl. Always javaName.
		String[] implImplementing = {javaName.toString()};
		//interface used by the struct interface
		String[] ifaceImplementing = new String[1];

		if (includeName == null) { // TODO: null OR NIL
			ifaceImplementing[0] = "lisp/common/type/StructureClass";
		} else {
			ifaceImplementing[0] = ((DefstructSymbolStruct) includeName).getJavaName();
		}

		//Process the fields (i.e. slots)
		int fieldListSize = fieldList.size();
		SymbolStruct[] fields = new SymbolStruct[fieldListSize];

		//values = new Object[fieldListSize];
		for (int i = 0; i < fieldListSize; i++) {
			//get first set of field info
			SymbolStruct tempName = (SymbolStruct) fieldList.getFirst();
			//if there are more fields, get the rest of them
			fieldList = fieldList.getRest();
			//parse out the field name, type, and init value info
			fields[i] = tempName;
		}

		//these methods generate the byte code to create the struct stuff
		icgCreateDefstructFactory(javaName.toString());
		icgCreateDefstructAbstractFactory(javaName.toString());
		icgCreateDefstruct(javaName.toString(), ifaceImplementing, lispName);
		icgCreateDefstructImplFactory(javaName.toString(), fields.length);
		icgCreateDefstructImplClass(javaName.toString(), implImplementing, lispName, fields, printerFunction, includeName, includedSlotNumberAsInt);

		// initializing code in the enclosing lambda
		emitter.emitGetstatic(javaName + "Impl$Factory", "initialize", "Z");
		emitter.emitPop();
		emitter.emitGetstatic(javaName + "Impl", "initialize", "Z");
		emitter.emitPop();

		genCodeSpecialSymbol(javaName);
		emitter.emitGetstatic("lisp/common/type/StructureClass", "DEFSTRUCT_INDICATOR", "Llisp/common/type/Symbol;");
		emitter.emitLdc(javaName + "$Factory");
		emitter.emitInvokestatic("java/lang/Class", "forName", "(Ljava/lang/String;)", "Ljava/lang/Class;", false);
		emitter.emitInvokeinterface("lisp/common/type/Symbol", "setprop", "(Ljava/lang/Object;Ljava/lang/Object;)", "V", true);

		// it balances something that's popping...
		emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
	}

	private void genCodeEvalWhen(ListStruct list) {
		//TODO unimplemented 'eval-when'
	}

	private void genCodeFlet(ListStruct list) {
		genCodeProgn(list);
	}

	private void genCodeLabels(ListStruct list) {
		genCodeProgn(list);
	}

	private void genCodeFunction(ListStruct list) {
		list = list.getRest();
		Object fn = list.getFirst();
		if (fn instanceof SymbolStruct) {
			genCodeSymbolFunction((SymbolStruct) fn);
		} else if (fn instanceof ListStruct) {
			ListStruct fnList = (ListStruct) fn;
//            if (fnList.getCar() == SpecialOperatorOld.LAMBDA) {
//                genCodeLambda(fnList);
//            } else {
			// this is a setf function (setf foo)
			// this is a call to return the setf function in the specified symbol
			// It's ok if there is no function right now. This is just code to
			// get it when needed
			// Step 1: get the symbol
			// Step 2: return the function stashed in the symbol or NIL if not there
			// The SETF expander will ensure that there will be a FUNCALL #'(setf foo) with args
			if (!(((ListStruct) fn).getRest().getFirst() instanceof SymbolStruct)) {
				System.out.println("** 1129: " + ((ListStruct) fn).getRest().getFirst());
				System.out.println("++ 1129: " + ((ListStruct) fn).getRest());
				System.out.println("-- 1129: " + fn);
			}
			SymbolStruct setfSymbol = (SymbolStruct) ((ListStruct) fn).getRest().getFirst();
			genCodeSpecialVariable(setfSymbol); // now we have the symbol on the stack
			// number the invoke
			Label label = new Label();
			emitter.visitMethodLabel(label);
			// extract the setf function if there is one
			emitter.emitCheckcast("lisp/system/SymbolImpl");
			emitter.emitInvokevirtual("lisp/system/SymbolImpl", "getSetfFunction", "()", "Llisp/common/type/Function;", false);
			emitter.emitDup();      // need to test to see it's there
			Label yesSetfFunction = new Label();
			emitter.emitIfnonnull(yesSetfFunction); // there is no setf function, return NIL
			emitter.emitPop();      // balance the stack
			emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
			emitter.visitMethodLabel(yesSetfFunction);
//            }
		} else {
			icgMainLoop(fn);
		}
	}

	private void genCodeGo(ListStruct list) {
		/* Get the symbol out of the list. */
		list = list.getRest();
		SymbolStruct sym = (SymbolStruct) list.getFirst();

        /*
		// first, try to see if this is a go in the same tagbody (the most common)
        TagbodyLabel tbl = findTagbodyBySymbol(tagbodyStack.peek(), sym);
        if (tbl != null) {
        emitter.emitGoto(tbl.label);
        } else {
         */
        /* Throw a GoException. */
		emitter.emitNew("lisp/system/compiler/exceptions/GoException");
		emitter.emitDup();
		//genCodeSpecialSymbol(sym);
		emitter.emitLdc("" + (findTagbodyInStack(tagbodyStack, sym)).index);   // me
		//emitter.emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Llisp/common/type/Symbol;)V"); //me
		emitter.emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Ljava/lang/Object;)", "V", false);
		emitter.emitAthrow();
		//}
	}

	private void genCodeIf(ListStruct list) {
		Object testObj;
		Object thenObj;
		Object elseObj;

		list = list.getRest();
		testObj = list.getFirst();
		list = list.getRest();
		thenObj = list.getFirst();
		list = list.getRest();
		elseObj = list.getFirst();

		icgMainLoop(testObj);
		Label outLabel = new Label();
		emitter.emitDup();
		emitter.emitInstanceof("[Ljava/lang/Object;");
		emitter.emitIfeq(outLabel);
		emitter.emitCheckcast("[Ljava/lang/Object;");
		emitter.emitLdc(0);
		emitter.emitAaload();
		emitter.visitMethodLabel(outLabel);
		emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");

		Label thenLabel = new Label();
		Label elseLabel = new Label();
		Label endLabel = new Label();

		emitter.emitIf_acmpeq(elseLabel);
		emitter.visitMethodLabel(thenLabel);
		icgMainLoop(thenObj);
		emitter.emitGoto(endLabel);

		emitter.visitMethodLabel(elseLabel);
		icgMainLoop(elseObj);

		emitter.visitMethodLabel(endLabel);
	}

	private void genCodeMacroLambda(ListStruct list) {
		MacroLambda = true;
		genCodeLambda(list);
	}

	@SuppressWarnings("unchecked")
	private void doStaticInit(String className, SymbolStruct lispName) {
		// static init
		emitter.newMethod(Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC, "<clinit>", "()", "V", null, null);
		// init the SYMBOL field with the LISP name symbol
		if (lispName.getSymbolPackage() != null) {
			genCodeSpecialVariable(lispName);
		} else {
			//make the symbol
			emitter.emitLdc(lispName.toString());
			// make it into a Lisp string
			emitter.emitInvokestatic("lisp/common/type/String$Factory", "newInstance", "(Ljava/lang/CharSequence;)", "Llisp/common/type/String;", false);
			// now create the symbol
			emitter.emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Llisp/common/type/String;)", "Llisp/common/type/Symbol;", false);
		}
		emitter.emitPutstatic(className, "SYMBOL", "Llisp/common/type/Symbol;");

		// Creating and initializing any necessary load-time-values
		ListStruct env = bindingEnvironment;

		// see if we have to add any static fields for load-time-value
		ListStruct ltvList = ((ListStruct) AssocFunction.funcall(KeywordOld.LoadTimeValue, env.getRest())).getRest();
		// ltvList is a plist of the field names and lambda forms
		while (ltvList != NullStruct.INSTANCE) {
			String fldName = ltvList.getFirst().toString();
			// add the field
			emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL,
					fldName, "Ljava/lang/Object;", null, null);
			// now get down to the function
			ltvList = ltvList.getRest();
			// gen code for the function

			genCodeLambdaInContext((ListStruct) ltvList.getFirst(), true);
			// now there's an instance of the function on the stack, call it
			emitter.emitInvokeinterface("lisp/extensions/type/Function0", "funcall", "()", "Ljava/lang/Object;", true);
			// now put the value into the static field
			emitter.emitPutstatic(className, fldName, "Ljava/lang/Object;");
			// next...
			ltvList = ltvList.getRest();
		}
		// all done
		emitter.emitReturn();
		emitter.endMethod();
	}

	private void doConstructor(ListStruct list, String className) {
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
		Label isNull = new Label();
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

	private void undoClosureSetup(ListStruct environment) {
		ListStruct closureSetBody = EnvironmentAccessor.getClosureSet(environment).getRest();
		int numParams = closureSetBody.size() - 1; // remove :closure and (:depth . n) from contention
		if (numParams > 0) {
			// keep a copy of the 'this' reference
			emitter.emitAload(0);
			// blow up the current closure
			emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "popClosure", "()", "Llisp/extensions/type/Closure;", false);
			emitter.emitPop();
		}
	}

	@SuppressWarnings("unchecked")
	private void doClosureSetup(ListStruct environment) {
		ListStruct closureSetBody = EnvironmentAccessor.getClosureSet(environment).getRest();
		int numParams = closureSetBody.size() - 1; // remove :closure and (:depth . n) from contention

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
		environment = environment.getRest();
		ListStruct closureStuff = ((ListStruct) AssocFunction.funcall(KeywordOld.Closure, environment)).getRest().getRest();
		ListStruct bindings = ((ListStruct) AssocFunction.funcall(KeywordOld.Bindings, environment)).getRest();
		// (:closure (:depth . n) (x ....) (y ....) ...)
		// if there is one, allocate the object
		if ((bindings != NullStruct.INSTANCE) && (closureStuff != NullStruct.INSTANCE)) {
			// get the top closure object
			emitter.emitAload(0);
			emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			emitter.emitDup();
			// run the list of variables
			//TODO handle parameters that are special variables
			while (bindings != NullStruct.INSTANCE) {
				ListStruct binding = (ListStruct) bindings.getFirst();
				SymbolStruct variable = (SymbolStruct) binding.getFirst();
				ListStruct closureEntry = ((ListStruct) AssocFunction.funcall(variable, closureStuff)).getRest();
				if (closureEntry != NullStruct.INSTANCE) {
					// this entry is a closure
					// Since this is a lambda, it's a parameter. So put the value into the closure
					ConsStruct allocation = (ConsStruct) GetPlist.funcall(binding.getRest(), KeywordOld.Allocation);
					IntegerStruct param = (IntegerStruct) allocation.getCdr();
					// now where does it go
					IntegerStruct position = (IntegerStruct) GetPlist.funcall(closureEntry, KeywordOld.Position);
					emitter.emitLdc(position.getBigInteger().intValue()); // index
					emitter.emitLdc(0);                   // nesting (current one)
					emitter.emitAload(param.getBigInteger().intValue());      // value from the arg list
					emitter.emitInvokeinterface("lisp/extensions/type/Closure", "setBindingAt", "(IILjava/lang/Object;)", "V", true);
					// the closure is left on the stack
					// dup it for the rest loop, except the last time around
					emitter.emitDup();
				}
				bindings = bindings.getRest();
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
	@SuppressWarnings("unchecked")
	private void doFreeVariableSetup() {
		//-- get the symbol-table
		ListStruct symbolTable = EnvironmentAccessor.getSymbolTable(bindingEnvironment).getRest();
		// Now iterate over the entries, looking for ones to allocate
		while (symbolTable != NullStruct.INSTANCE) {
			ListStruct entry = (ListStruct) symbolTable.getFirst();
			// (symbol :allocation ... :binding ... :scope ... :type ...)
			ListStruct pList = entry.getRest();
			// (:allocation ... :binding ... :scope ... :type ...)
			// for free and dynamic
			if ((GetPlist.funcall(pList, KeywordOld.Binding) == KeywordOld.Free)
					&& (GetPlist.funcall(pList, KeywordOld.Scope) == KeywordOld.Dynamic)) {
				// get the local variable slot
				ConsStruct alloc = (ConsStruct) GetPlist.funcall(pList, KeywordOld.Allocation);
				// (:local . n)
				if (alloc.getCar() == KeywordOld.Local) {
					IntegerStruct slot = (IntegerStruct) alloc.getCdr();
					// now gen some code (whew)
					// gen code to either intern a symbol or call make-symbol if uninterned
					SymbolStruct symbol = (SymbolStruct) entry.getFirst();
					if (symbol.getSymbolPackage() == null) {
						String name = symbol.getName().toString();
						// have to gen a make-symbol
						emitter.emitLdc(name);
						emitter.emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Ljava/lang/String;)", "Llisp/common/type/Symbol;", false);
					} else {
						genCodeSpecialVariable(symbol);
					}
					// store the symbol in the indicated local variable
					emitter.emitAstore(slot.getBigInteger().intValue());
				}
			}
			symbolTable = symbolTable.getRest();
		}
	}

	private void doCheckArguments(int numParams) {
		// Create a basic checkArguments method to do some checking.
		// This will be more complex as the compiler gets smarter
		//--------> checkArguments <-------------
		int checkArgsTestCtr = 0;
		emitter.newMethod(Opcodes.ACC_PUBLIC, "checkArguments", "(Llisp/common/type/ListStruct;)", "Llisp/common/type/Boolean;", null, null);
		// the most basic check is the number of arguments
		emitter.emitAload(1); // get the list argument
		emitter.emitInvokeinterface("java/util/Collection", "size", "()", "I", true);
		emitter.emitLdc(numParams);
		Label label = new Label();
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

	private int countRequiredParams(ListStruct bindingSetBody) {
		int countRequired = 0;
		// go through the list counting the usage :required entries
		for (LispStruct nextEntry : bindingSetBody.getAsJavaList()) {
			ListStruct binding = (ListStruct) nextEntry;
			if (GetPlist.funcall(binding.getRest(), KeywordOld.Required) != NullStruct.INSTANCE) {
				countRequired++;
			} else {
				break;
			}
		}
		return countRequired;
	}

	//TODO when checking bindings, in handling the init-forms, start with the just previous
	// bindings in the lambda list. Differs from how LET handles it
	private void genCodeLambda(ListStruct list) {
		genCodeLambdaInContext(list, false);
	}

	@SuppressWarnings({"unchecked", "unchecked"})
	private void genCodeLambdaInContext(ListStruct list, boolean inStaticContext) {
		ListStruct funcallList;
		int numParams;
		String funcallParams = "";
		String className;

		//--------
		// get the class name out of the list
		ListStruct decl = (ListStruct) getCadr(list);
		// (declare (mumble...) (more-mumble...))
		decl = decl.getRest();
		// ((mumble...) (more-mumble...))
		ListStruct javaSymbolName = (ListStruct) AssocFunction.funcall(DeclarationOld.JAVA_CLASS_NAME, decl);
		className = getCadr(javaSymbolName).toString().replace('.', '/');
		classNames.push(className);

		// now lispify it
		ListStruct lispSymbolName = (ListStruct) AssocFunction.funcall(DeclarationOld.LISP_NAME, decl);
		if (lispSymbolName == NullStruct.INSTANCE) {
			lispSymbolName = javaSymbolName;
		}
		SymbolStruct lispName = (SymbolStruct) getCadr(lispSymbolName);
		//
		ListStruct documentation = (ListStruct) AssocFunction.funcall(DeclarationOld.DOCUMENTATION, decl);
		if ((sourceFile == null) || (sourceFile == NullStruct.INSTANCE)) {
			sourceFile = (ListStruct) AssocFunction.funcall(DeclarationOld.SOURCE_FILE, decl);
		}

		// compile the new function class
		@SuppressWarnings("UseOfObsoleteCollectionType")
		java.util.Vector<java.lang.String> interfaces = new java.util.Vector<>();
		interfaces.add("lisp/common/type/CompiledFunction");
		if (MacroLambda) {
			interfaces.add("lisp/common/type/MacroFunction");
			MacroLambda = false;
		}
		// see if it's a FunctionN
		if (!(list.getFirst() instanceof ListStruct)) {
			System.out.println("** 1502 -> " + list.getFirst());
			System.out.println("++ 1502 -> " + list.getRest());
			System.out.println("-- 1502 -> " + list);
		}
		ListStruct bindingSetBody = EnvironmentAccessor.getBindingSet((ListStruct) list.getFirst()).getRest();

		numParams = bindingSetBody.size();
		if (numParams <= 11) {
			interfaces.add("lisp/extensions/type/Function" + numParams);
		}

		int numRequiredParams = countRequiredParams(bindingSetBody);

		// compile the new function class
		emitter.newClass(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, className, null,
				"lisp/common/function/FunctionBaseClass",
				interfaces.toArray(new String[1]));

		SymbolStruct docUID = (SymbolStruct) GensymFunction.funcall("docUID_" + System.currentTimeMillis());

		// if this is from a compile-file, add the source file name
		String fileName = getCadr(sourceFile).toString();
		emitter.visitClassSource(fileName, null);

		String docString = "";

		// add the documentation for the class
		if (documentation != NullStruct.INSTANCE) {
			docString = getCadr(documentation).toString();
		}

		emitter.newAnnotation("Llisp/system/documentation/DocStringAnn;", true);
		emitter.visitAnnotationValue("docUID", docUID.toString());
		emitter.visitAnnotationValue("docString", docString);
		emitter.visitAnnotationValue("generated", System.currentTimeMillis());
		emitter.visitAnnotationValue("javaName", className);
		emitter.endAnnotation();

		emitter.newAnnotation("Llisp/extensions/type/SourceFileAnnotation;", true);
		emitter.visitAnnotationValue("dateTime", new java.util.Date().toString());
		if (sourceFile != NullStruct.INSTANCE) {
			emitter.visitAnnotationValue("sourceFile", fileName);
		} else {
			emitter.visitAnnotationValue("sourceFile", "#<in-memory>");
		}
		emitter.endAnnotation();

		//Add the UID to the class so we can find our DOM later.
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL,
				"DOCUMENTATION_UID", "Ljava/lang/String;", null, docUID.toString());

		// add the static initialization
		// Need to separate the static init and any static methods and fields in here
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL,
				"SYMBOL", "Llisp/common/type/Symbol;", null, null);

		// constructor();
		doConstructor(list, className);

		// Handle all of the binding information
		try {
			bindingEnvironment = bindingStack.push((ListStruct) list.getFirst());

			// now create the check arguments method that's used when safety > 1
			//-----------> checkArguments <--------------------
//            doCheckArguments(numRequiredParams);

			//---------> funcall <-----------
			// funcall method
			for (int i = 0; i < numParams; i++) {
				funcallParams += "Ljava/lang/Object;";
			}
			emitter.newMethod(Opcodes.ACC_PUBLIC, "funcall", '(' + funcallParams + ')', "Ljava/lang/Object;", null, null);

			// allocate and fill a closure if there is one defined
			doClosureSetup(bindingEnvironment);

			// set up the free radicals - 1960's!!
			doFreeVariableSetup();

			// Beginning gen code for the body
			java.util.List<LispStruct> copyListJavaList = list.getAsJavaList();
			ListStruct copyList = ListStruct.buildProperList(copyListJavaList);
			funcallList = copyList.getRest().getRest();

			while (NullStruct.INSTANCE != funcallList) {
				icgMainLoop(funcallList.getFirst());
				funcallList = funcallList.getRest();
				if (NullStruct.INSTANCE != funcallList) {
					emitter.emitPop();
				}
			}
			// pop the closure if there was a new one
			undoClosureSetup(bindingEnvironment);

			// now we'return done..
			emitter.emitAreturn();
			emitter.endMethod();

			//------> apply <----------

			// Set up the apply(lisp.common.type.ListStruct) method
			emitter.newMethod(Opcodes.ACC_PUBLIC, "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", null, null);
			// Generate the JVM code to turn the list into an array. Then call the apply(Object[]) method
			// Local 1 is the list argument
			emitter.emitAload(0); // this

			// Now unfurl the arg list onto the stack and call the funcall method
			//...
			// Roll out the number of params defined for the fn - numParams
			for (int i = 0; i < numParams; i++) {
				emitter.emitAload(1); // get the current value of arg list
				// get the car of the list
				emitter.emitInvokeinterface("lisp/common/type/ListStruct", "getCar", "()", "Ljava/lang/Object;", true);
				// get the cdr
				emitter.emitAload(1); // get the current value of arg list
				emitter.emitInvokeinterface("lisp/common/type/ListStruct", "rest", "()", "Llisp/common/type/ListStruct;", true);
				emitter.emitAstore(1);
			}
//*****
			emitter.emitInvokevirtual(className, "funcall", '(' + funcallParams + ')', "Ljava/lang/Object;", false);
			emitter.emitAreturn();
			emitter.endMethod();

			// put the static components into the class definition
			// putting it here gives us the ability to put the load-time-value forms
			// in the class after we know what they are
			// 1. gather the LTV property from the lambda environment
			// 2. if non-NIL,
			// 2a. for each field name, add field, gen code for lambda,
			//     add code to init the field
			// 2b. again
			doStaticInit(className, lispName);

			emitter.endClass();
		} finally {
			bindingStack.pop();
			bindingEnvironment = bindingStack.peek();
		}

		// ** finished compiling the new lambda class **
		// Now make an instance and leave it on the stack
		if (!emitter.isClassStackEmpty()) {
			// push new function object onto stack
			emitter.emitNew(className);
			emitter.emitDup();
			// call constructor
			if (inStaticContext) {
				// here we have to init the instance but there's no outer context
				// so we put a null on the stack since the load-time-value runs in the
				// global environment
				emitter.emitAconst_null();
			} else {
				// get whatever is on top of the closure stack
				// get this
				emitter.emitAload(0);
				// get the closure stack
				emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			}
			emitter.emitInvokespecial(className, "<init>", "(Llisp/extensions/type/Closure;)", "V", false);
		}

		// pop off the current class name, we're done with it
		classNames.pop();
	}

	@SuppressWarnings("PackageVisibleInnerClass")
	class SymbolBindingLabel {

		@SuppressWarnings("PackageVisibleField")
		Label endLabel = null;
		@SuppressWarnings("PackageVisibleField")
		Label finallyLabel = null;
		@SuppressWarnings("PackageVisibleField")
		Label handlerLabel = null;
		@SuppressWarnings("PackageVisibleField")
		SymbolStruct dynamicSymbol = null;

		SymbolBindingLabel(Label endLabel, Label finallyLabel, Label handlerLabel, SymbolStruct dynamicSymbol) {
			this.endLabel = endLabel;
			this.finallyLabel = finallyLabel;
			this.handlerLabel = handlerLabel;
			this.dynamicSymbol = dynamicSymbol;
		}
	}

	@SuppressWarnings("unchecked")
	private void genCodeLet(ListStruct list) {
		// ((%let... (:parent ...) (:bindings ...) (:symbol-table ...) (:closure ...)))
		ListStruct funcallList;
		Stack<SymbolBindingLabel> bindingLabels = new Stack<>();

		// are we building a closure here?
		//----->
		bindingEnvironment = bindingStack.push((ListStruct) list.getFirst());
		ListStruct closureSetBody = EnvironmentAccessor.getClosureSet(bindingEnvironment).getRest();
//        int numParams = closureSetBody.size() - 1;

		try {
			// (%let... (:parent ...) (:bindings ...) (:symbol-table ...) (:closure ...))
			// Handle all of the binding information
			//----->
			ListStruct bindings = bindingEnvironment.getRest();
			// ((:parent ...) (:bindings ...) (:symbol-table ...) (:closure ...)))
			// Now get just the bindings list and drop the :bindings
			ListStruct bindingList = ((ListStruct) AssocFunction.funcall(KeywordOld.Bindings, bindings)).getRest();
			// ((sym1 :allocation ... :binding ... :scope ... :type ... :init-form ...)
			//  (sym2 :allocation ... :binding ... :scope ... :type ... :init-form ...)...)
			// Now to loop thru the bindings, gen code for the init forms and store them in the
			// proper slots. Note that init forms are evaluated in the enclosing environment
			ListStruct tmpEnv = bindingEnvironment;
			// any init forms get evaluated in the parent binding
			bindingEnvironment = EnvironmentAccessor.getParent(bindingEnvironment);
			// now, run the bindings
			while (bindingList != NullStruct.INSTANCE) {
				ListStruct binding = (ListStruct) bindingList.getFirst();
				SymbolStruct sym = (SymbolStruct) binding.getFirst();
				binding = binding.getRest();
				// (:allocation ... :binding ... :scope ... :type ... :init-form ...)
				// get the variable's init form
				LispStruct initForm = GetPlist.funcall(binding, KeywordOld.InitForm);
				// is this a local or dynamic variable?
				SymbolStruct scope = (SymbolStruct) GetPlist.funcall(binding, KeywordOld.Scope);
				//** this is the place where the ICG has to choose to allocate a variable
				//** in a local or it's a binding of a special variable
				// now, which is it: :local or :dynamic
				if (scope != KeywordOld.Dynamic) {
					// Now get the allocation value
					ConsStruct alloc = (ConsStruct) GetPlist.funcall(binding, KeywordOld.Allocation);
					int slot = ((IntegerStruct) alloc.getCdr()).getBigInteger().intValue();
					// hand the init form to icgMainLoop...
					// the generated code leaves its value on the stack
					icgMainLoop(initForm);
					// store the value in the proper local slot
					emitter.emitAstore(slot);
				} else {
					// handle binding a dynamic variable
					// 0. create an end and a handler Label, add them to a stack, create a start Label
					Label startLabel = new Label();
					Label endLabel = new Label();
					Label finallyLabel = new Label();
					Label handlerLabel = new Label();
					SymbolBindingLabel labelSym = new SymbolBindingLabel(endLabel, finallyLabel, handlerLabel, sym);
					// 1. emit the tryFinally node with these labels
					emitter.visitTryCatchBlock(startLabel, endLabel, handlerLabel, null);
					// 2. emit the binding call
					genCodeSpecialVariable(sym);
					emitter.emitCheckcast("lisp/system/SymbolImpl");
					// 3. emit the eval of the init form
					// hand the init form to icgMainLoop...
					// the generated code leaves its value on the stack
					icgMainLoop(initForm);
					emitter.emitInvokevirtual("lisp/system/SymbolImpl", "bind", "(Ljava/lang/Object;)", "V", false);
					// 4. set handler start label
					emitter.visitMethodLabel(startLabel);
					// 5. push end/handler label and the symbol on a stack
					bindingLabels.push(labelSym);
				}
				// do it again, Sam
				bindingList = bindingList.getRest();
			}
			bindingEnvironment = tmpEnv;

			// we may have a closure to handle as well
			doClosureSetup(bindingEnvironment);
			doFreeVariableSetup();

			// all args are in the proper local slots, so do the body of the let
			java.util.List<LispStruct> copyListJavaList = list.getAsJavaList();
			ListStruct copyList = ListStruct.buildProperList(copyListJavaList);
			funcallList = copyList.getRest();

			while (NullStruct.INSTANCE != funcallList) {
				Object firstElt = funcallList.getFirst();
				if ((firstElt instanceof ListStruct) && (((ListStruct) firstElt).getFirst() == SpecialOperatorOld.DECLARE)) {
					funcallList = funcallList.getRest();
				} else {
					icgMainLoop(funcallList.getFirst());
					funcallList = funcallList.getRest();
					if (NullStruct.INSTANCE != funcallList) {
						emitter.emitPop();
					}
				}
			}

			// Now we construct the set of unbinds that constitutes the finally blocks
			// -> pop off labels on stack...
			while (!bindingLabels.empty()) {
				Label outLabel = new Label();
				// 1. emit the end/handler label
				SymbolBindingLabel labelSym = bindingLabels.pop();
				emitter.visitMethodLabel(labelSym.endLabel); // end of the try block
				// now call the finally block
				genCodeSpecialVariable(labelSym.dynamicSymbol);
				emitter.emitCheckcast("lisp/system/SymbolImpl");
				emitter.emitInvokevirtual("lisp/system/SymbolImpl", "unbind", "()", "Ljava/lang/Object;", false);
				emitter.emitPop(); // would mask the real return
				// now jump to the end of this block
				emitter.emitGoto(outLabel);

				// now for the handler part
				emitter.visitMethodLabel(labelSym.handlerLabel);
				// I have no idea why adding this DUP works, but it does...
				emitter.emitDup();
				emitter.emitAstore(1); // save the exception
				genCodeSpecialVariable(labelSym.dynamicSymbol);
				emitter.emitCheckcast("lisp/system/SymbolImpl");
				emitter.emitInvokevirtual("lisp/system/SymbolImpl", "unbind", "()", "Ljava/lang/Object;", false);
				emitter.emitPop(); // would mask the real return
				emitter.emitAload(1); // reload the exception
				emitter.emitAthrow(); // re-throw it

				// 2. emit unbind (finally clause) - it gets there either way
				emitter.visitMethodLabel(labelSym.finallyLabel); // start of the finally block
				// -- however it gets into the sequence of unbinds, it just runs them in
				// -- reverse order of binding
				emitter.visitMethodLabel(outLabel);
			}
		} finally {
			bindingStack.pop();
			bindingEnvironment = bindingStack.peek();
		}
	}

	private void genCodeLoadTimeValue(ListStruct list) {
		// This list looks like (load-time-value some-field-name)
		// all we have to do is get the value of the field
		emitter.emitGetstatic(classNames.peek(), list.getRest().getFirst().toString(), "Ljava/lang/Object;");
	}

	private void genCodeLocally(ListStruct list) {
		//TODO unimplemented 'locally'
	}

	private void genCodeMacrolet(ListStruct list) {
		// Get rid of the MACROLET symbol
		list = list.getRest();

        /* Call icgMainLoop() for each expression in the PROGN call,
         * and remove all but the last expression's value from the stack  */
		while (list != NullStruct.INSTANCE) {
			icgMainLoop(list.getFirst());
			list = list.getRest();
			if (list != NullStruct.INSTANCE) {
				emitter.emitPop();
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void genCodeMultipleValueCall(ListStruct list) {
		Object fn;

		// evaluate the lambda, leaving an instance on the stack
		list = list.getRest();
		fn = list.getFirst();
		icgMainLoop(fn);

		// now process each of the arguments, leaving them on the stack
		list = list.getRest();

		// now stuff into a list. It's a bit tricky sense functions can return
		// objects or arrays of objects. Have to check at runtime
		boolean firstPass = true;
		// make a private field to hold the resulting list
		SymbolStruct mvcFieldName = (SymbolStruct) GensymFunction.funcall("MVC_Field_" + System.currentTimeMillis());
		emitter.newField(Opcodes.ACC_PRIVATE, mvcFieldName.toString(), "Llisp/common/type/ListStruct;", null, null);
		// initialize it to NIL
		emitter.emitAload(0);
		emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		emitter.emitPutfield(classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");
		while (list != NullStruct.INSTANCE) {
			// this puts a value or value[] on the stack
			icgMainLoop(list.getFirst(), true);
			emitter.emitDup();
			// which is it?
			emitter.emitInstanceof("[Ljava/lang/Object;");
			// one if by sea... (it is)
			Label isntArray = new Label();
			Label allDone = new Label();

			emitter.emitIfeq(isntArray);  // jumps if it's NOT an array
			// so, make the array into a lisp list
			emitter.emitCheckcast("[Ljava/lang/Object;");
			emitter.emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			emitter.emitGoto(allDone);
			// ....
			emitter.visitMethodLabel(isntArray);
			// so, make the object into a lisp list
			emitter.emitCheckcast("java/lang/Object");
			emitter.emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "(Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			// now we have to splice it to the previous
			emitter.visitMethodLabel(allDone);

			// now, just nconc the prior list to the current one
			// Don't forget to restore the result into the local field.
			//    stack now holds the current list
			// get the nconc function
			emitter.emitGetstatic("lisp/extensions/type/CommonLispFunctions", "StdFunctions", "Llisp/extensions/type/CommonLispFunctions;");
			emitter.emitGetfield("lisp/extensions/type/CommonLispFunctions", "NConc", "Llisp/common/function/NConc;");
			emitter.emitDup();

			Label hackLabel = new Label(); // used to get out of the current loop from a (values)
			emitter.emitInstanceof("lisp/extensions/type/Function2");
			emitter.emitIfeq(hackLabel);
			emitter.visitMethodLabel(hackLabel);
			emitter.emitCheckcast("lisp/extensions/type/Function2");

			// get them in the right order [ ...fn, second =>
			emitter.emitSwap();
			// get the held value
			emitter.emitAload(0);
			emitter.emitGetfield(classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");
			// now looks like ...fn, second, first
			// get them in the right order
			emitter.emitSwap();
			// now looks like ...fn, first, second
			// now call nconc
			emitter.emitInvokeinterface("lisp/extensions/type/Function2", "funcall",
					"(Ljava/lang/Object;Ljava/lang/Object;)", "Ljava/lang/Object;", true);
			// now we have the two list spliced, now stow away
			emitter.emitCheckcast("lisp/common/type/ListStruct");
			emitter.emitAload(0);
			emitter.emitSwap();
			emitter.emitPutfield(classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

			list = list.getRest();
		}

		// get the held value
		emitter.emitAload(0);
		emitter.emitGetfield(classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

		// now that we have the final list on the stack,
		//   we have to NIL-out the field to prevent sticky garbage
		emitter.emitAload(0);
		emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		emitter.emitPutfield(classNames.peek(), mvcFieldName.toString(), "Llisp/common/type/ListStruct;");

		// now apply the function to the evaluated args
		emitter.emitInvokeinterface("lisp/common/type/Function", "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", true);
	}

	private void genCodeMultipleValueProg1(ListStruct list) {
		//TODO unimplemented 'prog1'
	}

	private void genCodeProgn(ListStruct list) {
		// Get rid of the PROGN symbol
		list = list.getRest();

        /* Call icgMainLoop() for each expression in the PROGN call,
         * and remove all but the last expression's value from the stack  */
		while (list != NullStruct.INSTANCE) {
			icgMainLoop(list.getFirst());
			list = list.getRest();
			if (list != NullStruct.INSTANCE) {
				emitter.emitPop();
			}
		}
	}

	private void genCodeProgv(ListStruct list) {
		//TODO unimplemented 'progv'
	}

	// this method can ONLY handle simple constants such as numbers, strings,
	// and literal symbols
	private void genCodeQuote(ListStruct list) {
		Object quotedObj = list.getRest().getFirst();
		if (quotedObj instanceof SymbolStruct) {
			SymbolStruct sym = (SymbolStruct) quotedObj;
			//TODO work out a way to handle uninterned symbols that have been encountered already
			// need symbol package lookup here!
			if (sym.getSymbolPackage() == null) {
				emitter.emitLdc(sym.getName().toString());
				emitter.emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Ljava/lang/String;)", "Llisp/common/type/Symbol;", false);
			} else {
				genCodeSpecialVariable(sym);
			}
		} else if (quotedObj instanceof IntegerStruct) {
			genCodeInteger((IntegerStruct) quotedObj);
		} else if (quotedObj instanceof FloatStruct) {
			genCodeFloat((FloatStruct) quotedObj);
		} else if (quotedObj instanceof RatioStruct) {
			genCodeRatio((RatioStruct) quotedObj);
		} else if (quotedObj instanceof ComplexStruct) {
			genCodeComplex((ComplexStruct) quotedObj);
		} else {
			throw new RuntimeException("Unable to quote: " + quotedObj);
		}
	}

	private void genCodeReturnFrom(ListStruct list) {
		// Get rid of the RETURN-FROM symbol
		list = list.getRest();
		SymbolStruct sym = (SymbolStruct) list.getFirst();
		list = list.getRest();

		emitter.emitNew("lisp/system/compiler/exceptions/ReturnFromException");
		// +1 -> exception
		emitter.emitDup();
		// +2 -> exception, exception
		genCodeSpecialVariable(sym);
		// +3 -> exception, exception, name
		icgMainLoop(list.getFirst());
		emitter.emitInvokespecial("lisp/system/compiler/exceptions/ReturnFromException", "<init>", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", false);
		emitter.emitAthrow();
	}

	private void genCodeSetq(ListStruct list) {
		list = list.getRest();
		while (NullStruct.INSTANCE != list) {
			// get the first symbol
			SymbolStruct symbol = (SymbolStruct) list.getFirst();
			// step over the variable
			list = list.getRest();
			// get the form to evaluate
			icgMainLoop(list.getFirst());
			// value is now on the stack, we have to determine where to put it
			// determine if this is a local variable or a special variable
			ListStruct binding = EnvironmentAccessor.getBindingEnvironment(bindingEnvironment, symbol, true);
			if ((binding != NullStruct.INSTANCE)
					&& (EnvironmentAccessor.getSymbolScope(bindingEnvironment, symbol) != KeywordOld.Dynamic)) {
				// so find what local slot it is
				int slot = genLocalSlot(symbol, binding.getRest()); // drop the %let
				// if this is the last set, dup the value so it's returned
				if (list.getRest() == NullStruct.INSTANCE) {
					emitter.emitDup(); // leaves the value on the stack
				}
				emitter.emitAstore(slot);
			} else {
				// now the value is on the stack, is the variable local or special?
				genCodeSpecialSymbol(symbol);
				emitter.emitSwap();
				emitter.emitInvokeinterface("lisp/common/type/Symbol", "setValue", "(Ljava/lang/Object;)", "Ljava/lang/Object;", true);
				if (list.getRest() != NullStruct.INSTANCE) {
					emitter.emitPop(); // pop the value on the stack execpt for the last one
				}
			}
			// step through the rest pair or done
			list = list.getRest();
		}
	}

	private void genCodeSymbolMacrolet(ListStruct list) {
		//TODO unimplemented 'symbol-macrolet'
	}

	private void genCodeTagbody(ListStruct list) {
		Stack<TagbodyLabel> tagStack;
		String tagbodyName;
		Object obj;
		SymbolStruct sym;
		String DELIM = "\t";      //The delimiter used for the string
		String allTagbodyLabels = "";       //A delim string of all the valid ints in the tagbody
		int size;                           //Size of the tagbodylabel stack
		Label startTryBlock;                //The start of the try block
		Label catchBlock;                   //The start of the catch block
		Label continueBlock;                //Subsequent code after the try/catch construct
		Label ifBlock;                      //If block executed if this catches someone else's excepton
		Label elseBlock;                    //If the exception is caught block

		startTryBlock = new Label();
		catchBlock = new Label();
		continueBlock = new Label();
		ifBlock = new Label();
		elseBlock = new Label();

        /* Skip past the TAGBODY symbol. */
		list = list.getRest();

        /* Read all the tags within the TAGBODY form. */
		tagStack = tagbodyReadLabels(list);
		tagbodyStack.push(tagStack);

        /* Create a string with all of the int index values from all of the labels in this tagbody
         * This will be used to setup the TOCMgmt stack record
         */
		size = tagStack.size();
		while (size-- > 0) {
			allTagbodyLabels = allTagbodyLabels + tagStack.get(size).index + DELIM;
		}

		//Set up the TOC management record
		// ... ,
		emitter.emitLdc(allTagbodyLabels);
		// ..., tagBodyLabels
		emitter.emitGetstatic("lisp/system/TransferOfControl", "TAGBODY", "Ljava/lang/String;");
		// ..., tagBodyLabels, TAGBODY
		emitter.emitSwap();
		// ... , TAGBODY, tagBodyLabels
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);

        /* Invoke the ICG for each non-tag statement within the TAGBODY form. */
		emitter.visitMethodLabel(startTryBlock);
		// +0
		while (list != NullStruct.INSTANCE) {
			obj = list.getFirst();

            /* If the car of the list is a Symbol, then its a tag, which means
             * a label needs to be emitted. Otherwise call the ICG on the car of
             * the list. */
			if (obj instanceof SymbolStruct) {
				sym = (SymbolStruct) obj;
				// find the symbol in the tagbody stack
				TagbodyLabel tbl = findTagbodyInStack(tagbodyStack, (SymbolStruct) obj);
				emitter.visitMethodLabel(tbl.label);
			} else {
				icgMainLoop(obj);
				emitter.emitPop(); // Throws away the results of any forms in the tag body
			}
			list = list.getRest();
		}

        /* If execution makes it all the way through with no exception then skip
         * over the exception handler. */
		// +0
		emitter.emitGoto(continueBlock);

		emitter.visitMethodLabel(catchBlock);
		// ..., excep
		emitter.emitDup();
		// ..., excep, excep

		emitter.emitInvokestatic("lisp/system/TransferOfControl", "isMine", "(Ljava/lang/Throwable;)", "Ljava/lang/Object;", false);

		// ..., excep, result
		emitter.emitDup();
		// ..., excep, result, result
		emitter.emitIfnull(ifBlock);
		emitter.emitGoto(elseBlock);
		//If block start
		emitter.visitMethodLabel(ifBlock);
		// ..., excep, result
		emitter.emitPop();
		// ..., excep
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ...,
		emitter.emitGoto(continueBlock);
		//If block end

		//Else block start
		emitter.visitMethodLabel(elseBlock);
		// ..., excep, result
		emitter.emitSwap();
		// ..., result, excep
		emitter.emitPop();
		// ..., result

		emitter.emitInvokevirtual("java/lang/Object", "toString", "()", "Ljava/lang/String;", false);
		// ..., resultString

		emitter.emitInvokestatic("java/lang/Integer", "parseInt", "(Ljava/lang/String;)", "I", false);

		// +1 - int
		// create a lookup switch for the labels
		int tagsSize = tagStack.size();
		int[] tagNumbers = new int[tagsSize];
		Label[] tagLabels = new Label[tagsSize];
		for (int index = 0; index < tagsSize; index++) {
			tagNumbers[index] = tagStack.get(index).index;
			tagLabels[index] = tagStack.get(index).label;
		}
		Label defaultLabel = new Label();
		// now create the tableswitch
		// +1 - int
		emitter.emitTableswitch(tagNumbers[0], tagNumbers[tagsSize - 1], defaultLabel, tagLabels);
		// +0
		/* Throw another exception to the most enclosing TAGBODY. */
		emitter.visitMethodLabel(defaultLabel);
		emitter.emitGoto(continueBlock);
		//Else block end

        /* Emit the post-exception handler label, and pop the tag stack from
         * 'tagbodyStack'. */
		emitter.visitMethodLabel(continueBlock);

        /* TAGBODY always returns NIL, so put a NIL on the stack to be
         * returned. */
		emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");

		//This is compilation only code
		tagbodyStack.pop();
		emitter.visitTryCatchBlock(startTryBlock, catchBlock, catchBlock, "java/lang/Throwable");

		//Here is the finally code
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}

	/**
	 * Reads all the tags in the TAGBODY form and inserts them into a stack
	 * which is returned. Its necessary to do this first since a GO can be
	 * executed for a tag declared later in the form.
	 */
	@SuppressWarnings("PackageVisibleInnerClass")
	class TagbodyLabel {

		final SymbolStruct symbol;
		final Label label;
		final int index = tagCounter++;

		TagbodyLabel(SymbolStruct symbol, Label label) {
			this.symbol = symbol;
			this.label = label;
		}
	}

	private Stack<TagbodyLabel> tagbodyReadLabels(ListStruct list) {
		Object obj;
		Stack<TagbodyLabel> tagStack = new Stack<>();

		while (list != NullStruct.INSTANCE) {
			obj = list.getFirst();
			if (obj instanceof SymbolStruct) {
				// Insert the tag into the stack.
				tagStack.push(new TagbodyLabel((SymbolStruct) obj, new Label()));
			}
			list = list.getRest();
		}

		return tagStack;
	}

	private TagbodyLabel findTagbodyBySymbol(Stack<TagbodyLabel> stack, SymbolStruct symbol) {
		int size = stack.size();
		TagbodyLabel tbl;
		while (size-- > 0) {
			tbl = stack.get(size);
			if (tbl.symbol == symbol) {
				return tbl;
			}
		}
		return null;
	}

	private TagbodyLabel findTagbodyInStack(Stack<Stack<TagbodyLabel>> stack, SymbolStruct symbol) {
		Stack<TagbodyLabel> tbs;
		int size = stack.size();
		TagbodyLabel tbl;
		while (size-- > 0) {
			tbs = stack.get(size);
			tbl = findTagbodyBySymbol(tbs, symbol);
			if (tbl != null) {
				return tbl;
			}
		}
		return null;
	}

	private void genCodeThe(ListStruct list) {
		//TODO unimplemented 'the'
	}

	private void genCodeThrow(ListStruct list) {
		Object catchTag;         //The catch tag value that must be evaluated

		// Remove the special symbol (THROW) from the list
		list = list.getRest();

		//Get the catch tag and store for later evaluation
		catchTag = list.getFirst();
		list = list.getRest();


		emitter.emitNew("lisp/system/compiler/exceptions/ThrowException");
		// +1 -> exception
		emitter.emitDup();
		// +2 -> exception, exception

		//Run the catch tag through the compiler for eval with the intent that the result
		//will be on the stack ready to be a parameter to the following method invokation
		icgMainLoop(catchTag);
		// +3 -> exception, exception, name
		icgMainLoop(list.getFirst());
		emitter.emitInvokespecial("lisp/system/compiler/exceptions/ThrowException", "<init>", "(Ljava/lang/Object;Ljava/lang/Object;)", "V", false);
		emitter.emitAthrow();
	}

	/**
	 * Transfer of Control Sequence for unwind-protect
	 * http://clforjava.cs.cofc.edu/twiki/bin/view/CLJcompiler/TransferOfControl#Compilation_Time_TOC_Sequences_f
	 * ---------------------------
	 * 1. Start the try block
	 * 2. Setup for the evaluation of the protected form
	 * 3. Setup a goto for the finally block
	 * 4. Start the catch block
	 * 5. Using the exception, store the exception so that it can be rethrown after the cleanup form is executed
	 * 6. In the finally block, disable invalid TOC Exit Points
	 * 7. Setup a finally block
	 * 8. Setup the evalution of the cleanup form
	 * 9. Setup for a runtime call to process the return exception (if there was one, it will be rethrown automatically)
	 * 10. Register an exception handler for type Throwable
	 */
	private void genCodeUnwindProtect(ListStruct list) {
		ListStruct protectedForm;                //The protected form
		ListStruct cleanupForm;                  //The cleanup form
		Label startTryBlock;               //The start of the try block
		Label catchBlock;                  //The start of the catch block
		Label finallyBlock;                //The start of the finally block

		// Burn off the special symbol (UNWIND-PROTECT)
		list = list.getRest();

		//Get the protected form and the cleanup form(s) after the UNWIND-PROTECT symbol
		protectedForm = (ListStruct) list.getFirst();
		cleanupForm = list.getRest();

		//Create the exception table
		startTryBlock = new Label();
		catchBlock = new Label();
		finallyBlock = new Label();

		//1. Start the try block
		//Mark the start of the try block
		emitter.visitMethodLabel(startTryBlock);

		//2. Setup for the evaluation of the protected form
		//Evalute the protected form
		icgMainLoop(protectedForm);

		//3. Setup a goto for the finally block
		//If an exception wasn't thrown, go past the catch block to the finally block
		emitter.emitGoto(finallyBlock);

		//4. Start the catch block
		//Start the catch block
		emitter.visitMethodLabel(catchBlock);

		//5. Using the exception, store the exception so that it can be rethrown after the cleanup form is executed
		// ..., throw_excep
		emitter.emitDup();
		// ..., throw_excep, throw_excep
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ..., throw_excep

		//6. In the finally block, disable invalid TOC Exit Points
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "disableExitPoints", "(Ljava/lang/Throwable;)", "V", false);
		// ...
		emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		//7. Setup a finally block
		//Start the finally block
		emitter.visitMethodLabel(finallyBlock);

		//8. Setup the evalution of the cleanup form
		//This is the finally code
		//Evalute the cleanup form
		while (cleanupForm != NullStruct.INSTANCE) {
			icgMainLoop(cleanupForm.getFirst());
			cleanupForm = cleanupForm.getRest();
			emitter.emitPop();
		}

		//9. Setup for a runtime call to process the return exception (if there was one, it will be rethrown automatically)
		//Throw the stored exception if an exception was thrown in the protected form
		emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);

		//10. Register an exception handler for type Throwable
		emitter.visitTryCatchBlock(
				startTryBlock,
				catchBlock,
				catchBlock,
				"java/lang/Throwable");
	}

	public Object getSymbolProperty(ListStruct bindings, SymbolStruct sym, Object propName) {
		Object retval = null;
		bindings = getSymbolPList(bindings, sym);
		if (bindings != NullStruct.INSTANCE) {
			while (bindings.getFirst() != NullStruct.INSTANCE && bindings.getFirst() != propName) {
				bindings = bindings.getRest();
			}
		}
		if (bindings != NullStruct.INSTANCE) {
			retval = bindings.getRest().getFirst();
		}
		return retval;
	}

	private ListStruct getSymbolPList(ListStruct bindings, SymbolStruct sym) {
		while ((bindings != NullStruct.INSTANCE) && sym != ((ListStruct) bindings.getFirst()).getFirst()) {
			bindings = bindings.getRest();
		}
		return (ListStruct) bindings.getFirst();
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//
	// DEFSTRUCT
	//
	//
	// Making FooStruct$Factory
	private void icgCreateDefstructFactory(String name) {
		emitter.newClass(Opcodes.ACC_PUBLIC, name + "$Factory", null, "java/lang/Object", new String[]{"lisp/extensions/type/StructureClassFactory"});
		emitter.addInnerClass(name + "$Factory", name, "Factory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);
		emitter.addInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);

		// <clinit>
		emitter.newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		emitter.emitReturn();
		emitter.endMethod();

		// <init>
		emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		emitter.emitAload(0);
		emitter.emitInvokespecial("java/lang/Object", "<init>", "()", "V", false);
		emitter.emitReturn();
		emitter.endMethod();

		// newInstance
		emitter.newMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", null, null);
		emitter.emitGetstatic(name, "factory", "L" + name + "$AbstractFactory;");
		emitter.emitGetfield(name + "$AbstractFactory", "trueFactory", "Llisp/extensions/type/StructureClassFactory;");
		emitter.emitAload(0);
		emitter.emitInvokeinterface("lisp/extensions/type/StructureClassFactory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", true);
		emitter.emitAreturn();
		emitter.endMethod();
		emitter.endClass();
	}

	// Making FooStruct$AbstractFactory
	private void icgCreateDefstructAbstractFactory(String name) {
		emitter.newClass(Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, name + "$AbstractFactory", null, "java/lang/Object", null);
		emitter.addInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC);
		emitter.newField(0, "trueFactory", "Llisp/extensions/type/StructureClassFactory;", null, null);
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);

		// <clinit>
		emitter.newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		emitter.emitLdc(1);
		emitter.emitPutstatic(name + "$AbstractFactory", "initialize", "Z");
		emitter.emitReturn();
		emitter.endMethod();

		// <init>
		emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		emitter.emitAload(0);
		emitter.emitInvokespecial("java/lang/Object", "<init>", "()", "V", false);
		emitter.emitAload(0);
		emitter.emitAconst_null();
		emitter.emitPutfield(name + "$AbstractFactory", "trueFactory", "Llisp/extensions/type/StructureClassFactory;");
		emitter.emitReturn();
		emitter.endMethod();
		emitter.endClass();
	}

	// Making FooStruct
	private void icgCreateDefstruct(String name, String[] implementing, SymbolStruct lispName) {
		emitter.newClass(Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT + Opcodes.ACC_INTERFACE, name, null, "java/lang/Object", implementing);
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "factory", "L" + name + "$AbstractFactory;", null, null);
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "typeName", "Llisp/common/type/Symbol;", null, null);

		emitter.addInnerClass(name + "$Factory", name, "Factory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);
		emitter.addInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);

		// <clinit>
		emitter.newMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		genCodeSpecialSymbol(lispName);
		emitter.emitPutstatic(name, "typeName", "Llisp/common/type/Symbol;");
		emitter.emitNew(name + "$AbstractFactory");
		emitter.emitDup();
		emitter.emitInvokespecial(name + "$AbstractFactory", "<init>", "()", "V", false);
		emitter.emitPutstatic(name, "factory", "L" + name + "$AbstractFactory;");
		emitter.emitReturn();
		emitter.endMethod();
		emitter.endClass();
	}

	// Making FooStructImpl$Factory
	public void icgCreateDefstructImplFactory(String name, int length) {
		String implName = name + "Impl";
		String implFactoryName = name + "Impl$Factory";
		emitter.newClass(Opcodes.ACC_PUBLIC, implFactoryName, null, "java/lang/Object", new String[]{"lisp/extensions/type/StructureClassFactory"});
		emitter.addInnerClass(implFactoryName, implName, "Factory", Opcodes.ACC_STATIC);
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);

		// <clinit>
		emitter.newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		emitter.emitLdc(1);
		emitter.emitPutstatic(implFactoryName, "initialize", "Z");
		emitter.emitReturn();
		emitter.endMethod();

		// <init>
		emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		emitter.emitAload(0);
		emitter.emitInvokespecial("java/lang/Object", "<init>", "()", "V", false);
		emitter.emitReturn();
		emitter.endMethod();

		// newInstance
		emitter.newMethod(Opcodes.ACC_PUBLIC, "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", null, null);
		emitter.emitNew(implName);
		emitter.emitDup();
		emitter.emitAload(1);
		emitter.emitInvokespecial(implName, "<init>", "([Ljava/lang/Object;)", "V", false);
		emitter.emitAreturn();
		emitter.endMethod();
		emitter.endClass();
	}

	// Making FooStructImpl$Class.
	public void icgCreateDefstructImplClass(String name, String[] interfaces, SymbolStruct lispName, SymbolStruct[] fields, Object printer, DefstructSymbolStruct includedStruct, int includedSlotNumber) {

		String implName = name + "Impl";
		String implFactoryName = name + "Impl$Factory";
		String abstractFactoryName = name + "$AbstractFactory";

		String includedStructFactory;
		if (includedStruct != null) { // TODO: null or NIL
			includedStructFactory = ((DefstructSymbolStruct) includedStruct).getJavaName() + "$Factory";
		} else {
			includedStructFactory = null;
		}
		;

		// class definition
		emitter.newClass(Opcodes.ACC_PUBLIC, implName, null, "lisp/system/StructureClassImpl", interfaces);
		// reference to the Factory and AbstractFactory classes
		emitter.addInnerClass(implFactoryName, implName, "Factory", Opcodes.ACC_STATIC);
		emitter.addInnerClass(abstractFactoryName, name, "AbstractFactory", Opcodes.ACC_STATIC);

		// add the static fields (trueFactory, initialize, slotCount, slotInfo, slotInitForms, and slotNames)
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "trueFactory", "L" + implFactoryName + ";", null, null);
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "slotCount", "I", null, null);
		emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "lispName", "Llisp/common/type/Symbol;", null, null);
		emitter.newField(Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "slotNames", "[Llisp/common/type/Symbol;", null, null);
		if (printer instanceof SymbolStruct) {
			emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/common/type/Symbol;", null, null);
		} else if (printer instanceof FunctionStruct) { // TODO: Function2
			emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/extensions/type/Function2;", null, null);
		} else if (printer instanceof FunctionStruct) { // TODO: Function3
			emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/extensions/type/Function3;", null, null);
		} else {
			emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Ljava/lang/Object;", null, null);
		}

		// add the instance fields: field1, field2...
		if (includedStructFactory == null) {
			for (int i = 0; i < fields.length; i++) {
				emitter.newField(0, "field" + (i + 1), "Ljava/lang/Object;", null, null);
			}
		} else {
			for (int i = 0; i < fields.length; i++) {
				emitter.newField(0, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;", null, null);
			}
		}

		//<clinit>
		emitter.newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		emitter.emitLdc(1);
		emitter.emitPutstatic(implName, "initialize", "Z");
		// hold the slot count
		emitter.emitLdc(fields.length);
		emitter.emitPutstatic(implName, "slotCount", "I");

		// if a print option was passed in, make an instance and store it
		if (printer instanceof SymbolStruct) {
			genCodeSpecialVariable((SymbolStruct) printer);
			emitter.emitPutstatic(implName, "printDefstructFunction", "Llisp/common/type/Symbol;");
		} else if (printer instanceof FunctionStruct || printer instanceof FunctionStruct) { // TODO: Function2 || Function3
			emitter.emitNew(printer.getClass().getName().toString());
			emitter.emitDup();
			emitter.emitInvokespecial(printer.getClass().getName().toString(), "<init>", "()", "V", false);
			if (printer instanceof FunctionStruct) { // TODO: Function2
				emitter.emitPutstatic(implName, "printDefstructFunction", "Llisp/extensions/type/Function2;");
			} else if (printer instanceof FunctionStruct) { // TODO: Function3
				emitter.emitPutstatic(implName, "printDefstructFunction", "Llisp/extensions/type/Function3;");
			}
		}

		// hold on to the original lispName
		genCodeSpecialVariable(lispName);
		emitter.emitPutstatic(implName, "lispName", "Llisp/common/type/Symbol;");

		// make an instance of the nested Factory class
		emitter.emitNew(implFactoryName);
		emitter.emitDup();
		emitter.emitInvokespecial(implFactoryName, "<init>", "()", "V", false);
		// now stow the Factory instance into the trueFactory static final slot
		emitter.emitPutstatic(implName, "trueFactory", "L" + implFactoryName + ";");

		// static { FooStruct.factory.trueFactory = trueFactory; }
		emitter.emitGetstatic(name, "factory", "L" + abstractFactoryName + ";");
		emitter.emitGetstatic(implName, "trueFactory", "L" + implFactoryName + ";");
		emitter.emitPutfield(abstractFactoryName, "trueFactory", "Llisp/extensions/type/StructureClassFactory;");

		// initialize the slotNames field with the slot names (inline)
		emitter.emitLdc(fields.length);
		emitter.emitAnewarray("lisp/common/type/Symbol");
		for (int i = 0; i < fields.length; i++) {
			emitter.emitDup();
			emitter.emitLdc(i);
			genCodeSpecialVariable(fields[i]);
			emitter.emitAastore();
		}
		emitter.emitDup();
		emitter.emitPutstatic(implName, "slotNames", "[Llisp/common/type/Symbol;");

		// put the same array of slot names into the type symbol
		genCodeSpecialSymbol(lispName);
		emitter.emitCheckcast("lisp/system/SymbolImpl");
		emitter.emitSwap();
		emitter.emitInvokevirtual("lisp/system/SymbolImpl", "setDefstructSlotNames", "([Llisp/common/type/Symbol;)", "V", false);
		emitter.emitReturn();
		emitter.endMethod();
		// struct impl class initialized

		//<init> for a non-included instance
		if (includedStructFactory == null) {
			//build FooStructImpl constructor here
			emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "([Ljava/lang/Object;)", "V", null, null);
			emitter.emitAload(0);
			emitter.emitAconst_null();      // not a Java parent reference
			emitter.emitInvokespecial("lisp/system/StructureClassImpl", "<init>", "(Llisp/system/StructureClassImpl;)", "V", false);
			for (int i = 0; i < fields.length; i++) {
				emitter.emitAload(0);
				emitter.emitAload(1);
				emitter.emitLdc(i);
				emitter.emitAaload();
				emitter.emitPutfield(implName, "field" + (i + 1), "Ljava/lang/Object;");
			}
			emitter.emitReturn();
			emitter.endMethod();

			// <init> for an included instance
		} else {
			//build BarStructImpl constructor here
			emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "([Ljava/lang/Object;)", "V", null, null);
			emitter.emitAload(0);     // this
			emitter.emitAload(1);     // args array
			// make the parent impl
			emitter.emitInvokestatic(includedStructFactory, "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", false);
			// parent struct, this
			emitter.emitCheckcast("lisp/system/StructureClassImpl"); // the parent is ok
			emitter.emitInvokespecial("lisp/system/StructureClassImpl", "<init>", "(Llisp/system/StructureClassImpl;)", "V", false);
			// the impl has created a parent included impl and stashed it into the instance

			// Now it gets interesting...
			// The parent(s) have claimed some of the slots. We have to find out which is now ours.
			// We do this by asking the parents how many have they used. We just then take the
			// remaining slots (recursion is involved...).
			for (int i = 0; i < (fields.length); i++) {
				emitter.emitAload(0);
				emitter.emitAload(1);
				emitter.emitLdc(i + includedSlotNumber);
				emitter.emitAaload();
				emitter.emitPutfield(implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
			}
			emitter.emitReturn();
			emitter.endMethod();
		}

		///////////////////////////
		// GET-SLOT METHOD CODE ///
		///////////////////////////

		// getSlot(Symbol sym) method
		emitter.newMethod(Opcodes.ACC_PUBLIC, "getSlot", "(Llisp/common/type/Symbol;)", "Ljava/lang/Object;", null, null);
		emitter.emitAload(0);
		emitter.emitAload(1);
		emitter.emitGetstatic(implName, "slotNames", "[Llisp/common/type/Symbol;");
		emitter.emitInvokevirtual(implName, "getSlotIndex", "(Llisp/common/type/Symbol;[Llisp/common/type/Symbol;)", "I", false);

		// now implement the switch code that gets to the right field
		Label getDefLabel = new Label();
		Label[] getHandlerBlocks = new Label[fields.length];
		int getKeys[] = new int[fields.length + 1];
		for (int i = 0; i < getKeys.length; i++) {
			getKeys[i] = i;
		}
		for (int i = 0; i < getHandlerBlocks.length; i++) {
			getHandlerBlocks[i] = new Label();
		}
		emitter.emitLookupswitch(getDefLabel, getKeys, getHandlerBlocks);
		if (includedStructFactory == null) {
			for (int i = 0; i < getHandlerBlocks.length; i++) {
				emitter.visitMethodLabel(getHandlerBlocks[i]);
				emitter.emitAload(0);
				emitter.emitGetfield(implName, "field" + (i + 1), "Ljava/lang/Object;");
				emitter.emitAreturn();
			}
		} else {
			for (int i = 0; i < getHandlerBlocks.length; i++) {
				emitter.visitMethodLabel(getHandlerBlocks[i]);
				emitter.emitAload(0);
				emitter.emitGetfield(implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
				emitter.emitAreturn();
			}
		}
		emitter.visitMethodLabel(getDefLabel);

		// the default choices
		// If this has an included component, it is delegated to the parent
		// If this is the top of a chain (or there were an included), it throws an exception
		Label excpLabel = new Label();

		// Here is the delegation code
		emitter.emitAload(0);  // this
		emitter.emitInvokevirtual(implName, "getParent", "()", "Llisp/system/StructureClassImpl;", false);
		emitter.emitDup();
		emitter.emitIfnull(excpLabel);
		// call the superclass
		emitter.emitAload(1);  // the symbol
		emitter.emitInvokevirtual("lisp/system/StructureClassImpl", "getSlot", "(Llisp/common/type/Symbol;", "Ljava/lang/Object;", false);
		emitter.emitAreturn();

		// Here is the exception code
		emitter.visitMethodLabel(excpLabel);
		emitter.emitPop();
		emitter.emitNew("lisp/common/exceptions/FunctionException");
		emitter.emitDup();
		emitter.emitNew("java/lang/StringBuilder");
		emitter.emitDup();
		emitter.emitInvokespecial("java/lang/StringBuilder", "<init>", "()", "V", false);
		emitter.emitLdc("Slot  ");
		emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		emitter.emitAload(1);
		emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/Object;)", "Ljava/lang/StringBuilder;", false);
		emitter.emitLdc(" not Found");
		emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		emitter.emitInvokevirtual("java/lang/StringBuilder", "toString", "()", "Ljava/lang/String;", false);
		emitter.emitInvokespecial("lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;)", "V", false);
		emitter.emitAthrow();

		emitter.endMethod();

		///////////////////////////
		// SET-SLOT METHOD CODE ///
		///////////////////////////

		//setSlot method
		emitter.newMethod(Opcodes.ACC_PUBLIC, "setSlot", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", null, null);
		emitter.emitAload(0);
		emitter.emitAload(1);
		emitter.emitGetstatic(implName, "slotNames", "[Llisp/common/type/Symbol;");
		emitter.emitInvokevirtual(implName, "getSlotIndex", "(Llisp/common/type/Symbol;[Llisp/common/type/Symbol;)", "I", false);

		Label setDefLabel = new Label();
		Label[] setHandlerBlocks = new Label[fields.length];
		int setKeys[] = new int[fields.length + 1];
		for (int i = 0; i < setKeys.length; i++) {
			setKeys[i] = i;
		}
		for (int i = 0; i < setHandlerBlocks.length; i++) {
			setHandlerBlocks[i] = new Label();
		}
		emitter.emitLookupswitch(setDefLabel, setKeys, setHandlerBlocks);
		if (includedStructFactory == null) {
			for (int i = 0; i < setHandlerBlocks.length; i++) {
				emitter.visitMethodLabel(setHandlerBlocks[i]);
				emitter.emitAload(0); // this
				emitter.emitAload(2); // the new value
				emitter.emitPutfield(implName, "field" + (i + 1), "Ljava/lang/Object;");
				emitter.emitReturn();
			}
		} else {
			for (int i = 0; i < setHandlerBlocks.length; i++) {
				emitter.visitMethodLabel(setHandlerBlocks[i]);
				emitter.emitAload(0); // this
				emitter.emitAload(2); // the new value
				emitter.emitPutfield(implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
				emitter.emitReturn();
			}
		}
		emitter.visitMethodLabel(setDefLabel);

		Label exDefLabel = new Label();

		// Here is the delegation code
		emitter.emitAload(0);  // this
		emitter.emitInvokevirtual(implName, "getParent", "()", "Llisp/system/StructureClassImpl;", false);
		emitter.emitDup();
		emitter.emitIfnull(exDefLabel);
		// call the superclass
		emitter.emitAload(1);  // the symbol
		emitter.emitAload(2); // the new value
		emitter.emitInvokevirtual("lisp/system/StructureClassImpl", "setSlot", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", false);
		emitter.emitReturn();

		// The exception if it can't find the slot
		emitter.visitMethodLabel(exDefLabel);
		emitter.emitPop();
		emitter.emitNew("lisp/common/exceptions/FunctionException");
		emitter.emitDup();
		emitter.emitNew("java/lang/StringBuilder");
		emitter.emitDup();
		emitter.emitInvokespecial("java/lang/StringBuilder", "<init>", "()", "V", false);
		emitter.emitLdc("Slot  ");
		emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		emitter.emitAload(1);
		emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/Object;)", "Ljava/lang/StringBuilder;", false);
		emitter.emitLdc(" not Found");
		emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		emitter.emitInvokevirtual("java/lang/StringBuilder", "toString", "()", "Ljava/lang/String;", false);
		emitter.emitInvokespecial("lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;)", "V", false);
		emitter.emitAthrow();

		// End of the method
		emitter.endMethod();

		// All done here.
		emitter.endClass();
	}

	// a utility to replace the use of Cadr
	private Object getCadr(ListStruct lst) {
		return lst.getRest().getFirst();
	}
}
