package jcl.compiler.real.sa.specialoperator.quote;

import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.packages.GlobalPackageStruct;

public class QuoteRatioAnalyzer implements Analyzer<ListStruct, RatioStruct> {

	public static final QuoteRatioAnalyzer INSTANCE = new QuoteRatioAnalyzer();

	@Override
	public ListStruct analyze(RatioStruct input) {
		// gen (/ numerator denominator)
		return ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP.findSymbol("/").getSymbolStruct(), new IntegerStruct(input.getBigFraction().getNumerator()), new IntegerStruct(input.getBigFraction().getDenominator()));
	}
}
