package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;

public class TheAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final TheAnalyzer INSTANCE = new TheAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		// For now just ignores the type specialization
		return input.getRest().getRest().getFirst();
	}
}
