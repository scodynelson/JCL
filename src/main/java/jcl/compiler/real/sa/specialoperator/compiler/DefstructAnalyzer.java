package jcl.compiler.real.sa.specialoperator.compiler;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;

public class DefstructAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final DefstructAnalyzer INSTANCE = new DefstructAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		return input;
	}
}
