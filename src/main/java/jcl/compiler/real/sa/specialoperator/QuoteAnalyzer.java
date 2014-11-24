package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteListAnalyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteSymbolAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

public class QuoteAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final QuoteAnalyzer INSTANCE = new QuoteAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() != 2) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct element = input.getRest().getFirst();

		final ListStruct newForm;
		if (element instanceof ListStruct) {
			newForm = QuoteListAnalyzer.INSTANCE.analyze((ListStruct) element, analyzer);
		} else if (element instanceof SymbolStruct) {
			newForm = QuoteSymbolAnalyzer.INSTANCE.analyze((SymbolStruct) element, analyzer);
		} else {
			return element;
		}

		// If was ListStruct or SymbolStruct, wrap resulting form in Load-Time-Value.
		final ListStruct initForm = ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, newForm);
		return LoadTimeValueAnalyzer.INSTANCE.analyze(initForm, analyzer);
	}
}
