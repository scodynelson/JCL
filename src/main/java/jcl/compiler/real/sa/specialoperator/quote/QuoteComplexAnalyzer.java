package jcl.compiler.real.sa.specialoperator.quote;

import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import jcl.numbers.ComplexStruct;
import jcl.packages.GlobalPackageStruct;

public class QuoteComplexAnalyzer implements Analyzer<ListStruct, ComplexStruct> {

	public static final QuoteComplexAnalyzer INSTANCE = new QuoteComplexAnalyzer();

	@Override
	public ListStruct analyze(ComplexStruct input) {
		// gen (complex real imaginary)
		return ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP.findSymbol("COMPLEX").getSymbolStruct(), input.getReal(), input.getImaginary());
	}
}
