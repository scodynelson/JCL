package jcl.compiler.old;

import jcl.LispStruct;
import jcl.compiler.old.symbol.Closure;
import jcl.compiler.old.symbol.SpecialOperatorOld;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

import java.util.Stack;

/**
 * Implements basic functionality for all Lisp functions implemented in Java.
 */
public abstract class FunctionBaseClass {

//    public static final Object PARSED_LAMBDA_LIST_FIELD = null;

	public static ListStruct PARSED_LAMBDA_LIST = NullStruct.INSTANCE;
	public ListStruct parsedLambdaList = NullStruct.INSTANCE;

	/**
	 * This is the default argument list munger. It takes an application
	 * (eg (foo 1 2) and
	 * conses the %compiler-funcall symbol onto the application. This makes
	 * it ready for the ICG. This works for function applications that only
	 * take required arguments. It does not check that the number of args
	 * are correct.
	 */
	public static FunctionStruct LAMBDA_ARGLIST_MUNGER =
			new FunctionStruct() {

				public LispStruct apply(LispStruct... lispStructs) {
					LispStruct arglist = lispStructs[0];
					LispStruct marker = lispStructs[1];

					return new ConsStruct(SpecialOperatorOld.FUNCTION_MARKER,
							new ConsStruct(marker, arglist));
				}

				public int numberOfValues() {
					return -1;
				}

				public LispStruct getDefinitionForm() {
					return NullStruct.INSTANCE;
				}

				public String getSourceFile() {
					return null;
				}
			};

	protected Stack<Closure> closures;
	private static LispStruct DEFINITION_FORM = NullStruct.INSTANCE;
	public String SOURCE_FILE_NAME = null;

	protected Object mungedArglistFn = NullStruct.INSTANCE;

	public Object getArglistMunger() {
		return mungedArglistFn;
	}

	public void setArglistMunger(FunctionStruct mungedArglistFn) {
		this.mungedArglistFn = mungedArglistFn;
	}

	public FunctionBaseClass() {
		closures = new Stack<Closure>();
	}

	public FunctionBaseClass(Closure closure) {
		this();
		if (closure != null) {
			this.addClosure(closure);
		}
	}

	public Closure getClosure() {
		Closure closure = null;
		if (!closures.empty()) {
			closure = closures.peek();
		}
		return closure;
	}

	public Closure addClosure(Closure closure) {
		return closures.push(closure);
	}

	public Closure popClosure() {
		Closure closure = closures.pop();
		return closure;
	}

	public Stack<Closure> getClosureStack() {
		return closures;
	}

	//    public Function1 getArglistAnalyzer() {
//        return arglistAnalyzer;
//    }
//    public void setArglistAnalyzer(Function1 arglistAnalyzer) {
//        this.arglistAnalyzer = arglistAnalyzer;
//    }
	public LispStruct getDefinitionForm() {
		return DEFINITION_FORM;
	}

	public void setDefinitionForm(LispStruct form) {
		DEFINITION_FORM = form;
	}

	// the function has to be done using ASM, like disassemble
	public String getSourceFile() {
		return "Not yet implemented";
	}

	public int numberOfValues() {
		return 1;
	}
}
