package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.structs.lists.ListStruct;

public class MacroLambdaAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MacroLambdaAnalyzer INSTANCE = new MacroLambdaAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		// TODO: do this!!!

		return LambdaAnalyzer.INSTANCE.analyze(input);
	}
}
