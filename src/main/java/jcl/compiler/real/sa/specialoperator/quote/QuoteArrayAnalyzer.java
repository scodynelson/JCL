package jcl.compiler.real.sa.specialoperator.quote;

import jcl.arrays.ArrayStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.ArrayStructAnalyzer;
import jcl.lists.ListStruct;

public class QuoteArrayAnalyzer implements Analyzer<ListStruct, ArrayStruct> {

	public static final QuoteArrayAnalyzer INSTANCE = new QuoteArrayAnalyzer();

	@Override
	public ListStruct analyze(ArrayStruct input) {
		return (ListStruct) ArrayStructAnalyzer.INSTANCE.analyze(input);
	}
}
