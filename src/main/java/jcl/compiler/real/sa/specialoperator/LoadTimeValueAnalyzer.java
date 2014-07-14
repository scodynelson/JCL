package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

public class LoadTimeValueAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LoadTimeValueAnalyzer INSTANCE = new LoadTimeValueAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		return analyze(input, "LOAD_TIME_VALUE_", GensymFunction.funcall(String.valueOf(System.currentTimeMillis()) + '_').toString());
	}

	public LispStruct analyze(final ListStruct input, final String ltvFieldName) {
		return analyze(input, ltvFieldName, "");
	}

	// 1. get the current lambda environment
	// 2. get the LTV list
	// 3. wrap the rest of the list in a (lambda () ...)
	// 4. make a new field name
	// 5. make a cons of the name and the lambda into a plist in the environment
	// 6. add the cons to the LTV list
	// 7. return the LTV marker and the field name
	//    (load-time-value 'this is the field name')
	public LispStruct analyze(final ListStruct input, final String ltvFieldName, final String tag) {
		final Environment lambdaEnv = EnvironmentAccessor.getEnclosingLambda(SemanticAnalyzer.environmentStack.peek());
		ListStruct ltv = input.getRest();
		// better name this sucker
		final SymbolStruct name = (SymbolStruct) GensymFunction.funcall(ltvFieldName + "_FN_" + tag);
		ListStruct decl = ListStruct.buildProperList(Declaration.JAVA_CLASS_NAME, name);
		decl = ListStruct.buildProperList(SpecialOperator.DECLARE, decl);
		// now it's (declare (%java-class-name "blah"))
		ltv = new ConsStruct(decl, ltv);
		ltv = new ConsStruct(NullStruct.INSTANCE, ltv);
		ltv = new ConsStruct(SpecialOperator.LAMBDA, ltv);
		// now it's (lambda () (declare (%java-class-name "blah")) ..body..)

		// Now we have to recursively munge the lambda, but in the global environment
		try {
			SemanticAnalyzer.environmentStack.push(SemanticAnalyzer.environmentStack.elementAt(0));
			// make a form ... ( ((%lambda ...)...) ) - gives room to add the name at front
			ltv = SemanticAnalyzer.saLambda(ltv);
		} finally {
			SemanticAnalyzer.environmentStack.pop();
		}
		final SymbolStruct ltvName = new SymbolStruct(ltvFieldName + tag);

		// now it has to be put into the lambda environmment
		final LoadTimeValue ltvAssoc = lambdaEnv.getLoadTimeValue();

		// CDR = (ltvName ltv CDR)
		ltvAssoc.getValues().put(ltvName, ltv);

		return ListStruct.buildProperList(input.getFirst(), ltvName);
	}
}
