package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;

public class ThrowAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ThrowAnalyzer INSTANCE = new ThrowAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		return PrognAnalyzer.INSTANCE.analyze(input);
	}
}
