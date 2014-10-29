package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteListAnalyzer;
import jcl.compiler.real.sa.specialoperator.quote.QuoteSymbolAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

public class QuoteAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final QuoteAnalyzer INSTANCE = new QuoteAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input, final SemanticAnalyzer semanticAnalyzer) {
		return analyze(input, semanticAnalyzer, null);
	}

	public static LispStruct analyze(final ListStruct input, final SemanticAnalyzer semanticAnalyzer, final String fieldName) {

		if (input.size() != 2) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct element = input.getRest().getFirst();

		final ListStruct newForm;
		if (element instanceof ListStruct) {
			newForm = QuoteListAnalyzer.INSTANCE.analyze((ListStruct) element, semanticAnalyzer);
		} else if (element instanceof SymbolStruct) {
			newForm = QuoteSymbolAnalyzer.INSTANCE.analyze((SymbolStruct) element, semanticAnalyzer);
		} else {
			return element;
		}

		final ListStruct initForm = new ConsStruct(SpecialOperator.LOAD_TIME_VALUE, newForm);

		if (fieldName == null) {
			return LoadTimeValueAnalyzer.INSTANCE.analyze(initForm, semanticAnalyzer);
		} else {
			return LoadTimeValueAnalyzer.analyze(initForm, semanticAnalyzer, fieldName);
		}
	}
}
