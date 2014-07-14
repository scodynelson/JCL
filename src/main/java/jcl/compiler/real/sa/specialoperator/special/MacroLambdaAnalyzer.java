package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;

public class MacroLambdaAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MacroLambdaAnalyzer INSTANCE = new MacroLambdaAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		// if there is the new macro parser, use it
		// if not, do the original bootstrap
		if (false) {
			// call the macro parser and set up the updated list
			return SemanticAnalyzer.saMainLoop(input);
		} else {
			return LambdaAnalyzer.INSTANCE.analyze(input);
		}
	}

}
