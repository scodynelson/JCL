package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.arrays.VectorStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteArrayAnalyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteComplexAnalyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteListAnalyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteRatioAnalyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteSymbolAnalyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteVectorAnalyzer;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.ComplexStruct;
import jcl.numbers.RatioStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

public class QuoteAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final QuoteAnalyzer INSTANCE = new QuoteAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		return analyze(input, null);
	}

	public LispStruct analyze(final ListStruct input, final String fieldName) {

		if (input.size() != 2) {
			throw new RuntimeException("QUOTE: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct element = input.getRest().getFirst();

		final ListStruct newForm;
		if (element instanceof ListStruct) {
			newForm = QuoteListAnalyzer.INSTANCE.analyze((ListStruct) element);
		} else if (element instanceof SymbolStruct) {
			newForm = QuoteSymbolAnalyzer.INSTANCE.analyze((SymbolStruct) element);
		} else if (element instanceof RatioStruct) {
			newForm = QuoteRatioAnalyzer.INSTANCE.analyze((RatioStruct) element);
		} else if (element instanceof ComplexStruct) {
			newForm = QuoteComplexAnalyzer.INSTANCE.analyze((ComplexStruct) element);
		} else if (element instanceof VectorStruct) {
			newForm = QuoteVectorAnalyzer.INSTANCE.analyze((VectorStruct) element);
		} else if (element instanceof ArrayStruct) {
			newForm = QuoteArrayAnalyzer.INSTANCE.analyze((ArrayStruct) element);
		} else {
			return element;
		}

		final ListStruct initForm = new ConsStruct(SpecialOperator.LOAD_TIME_VALUE, newForm);

		if (fieldName == null) {
			return LoadTimeValueAnalyzer.INSTANCE.analyze(initForm);
		} else {
			return LoadTimeValueAnalyzer.INSTANCE.analyze(initForm, fieldName);
		}
	}
}
