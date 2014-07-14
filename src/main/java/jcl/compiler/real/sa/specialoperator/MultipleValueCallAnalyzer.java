package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;

public class MultipleValueCallAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MultipleValueCallAnalyzer INSTANCE = new MultipleValueCallAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		if (input.size() < 2) {
			throw new RuntimeException("Wrong number of arguments to special operator MULTIPLE-VALUE-CALL: " + input.size());
		}
		return PrognAnalyzer.INSTANCE.analyze(input);
	}
}
