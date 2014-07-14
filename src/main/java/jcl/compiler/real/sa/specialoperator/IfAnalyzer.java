package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;

public class IfAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final IfAnalyzer INSTANCE = new IfAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if ((input.size() > 4) || (input.size() < 3)) {
			throw new RuntimeException("Wrong number of arguments to special operator If: " + input.size());
		}
		return PrognAnalyzer.INSTANCE.analyze(input);
	}
}
