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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class QuoteAnalyzer implements Analyzer<LispStruct, ListStruct> {

	@Autowired
	private QuoteListAnalyzer quoteListAnalyzer;

	@Autowired
	private QuoteSymbolAnalyzer quoteSymbolAnalyzer;

	@Autowired
	private LoadTimeValueAnalyzer loadTimeValueAnalyzer;

	@Override
	public LispStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() != 2) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct element = input.getRest().getFirst();

		final ListStruct newForm;
		if (element instanceof ListStruct) {
			newForm = quoteListAnalyzer.analyze((ListStruct) element, analyzer);
		} else if (element instanceof SymbolStruct) {
			newForm = quoteSymbolAnalyzer.analyze((SymbolStruct) element, analyzer);
		} else {
			return element;
		}

		// If was ListStruct or SymbolStruct, wrap resulting form in Load-Time-Value.
		final ListStruct initForm = ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, newForm);
		return loadTimeValueAnalyzer.analyze(initForm, analyzer);
	}
}
