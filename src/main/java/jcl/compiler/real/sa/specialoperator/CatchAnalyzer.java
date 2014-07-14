package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;

public class CatchAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final CatchAnalyzer INSTANCE = new CatchAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		return PrognAnalyzer.INSTANCE.analyze(input);
	}
}
