package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.TheElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class TheAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 6723289642694216454L;

	@Override
	public TheElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 3) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: " + input.size() + ". Expected 3 arguments.");
		}

		final LispStruct valueType = input.getRest().getFirst();
		if (!(valueType instanceof SymbolStruct) && !(valueType instanceof ListStruct)) {
			throw new ProgramErrorException("THE: Type specifier must be of type SymbolStruct or ListStruct. Got: " + valueType);
		}
		// TODO: do we actually want to somehow factor in the 'TypeSpecifier' produced by the second value?

		final LispStruct form = input.getRest().getRest().getFirst();
		final LispStruct formAnalyzed = analyzer.analyzeForm(form, analysisBuilder);

		return new TheElement(null, formAnalyzed);
	}
}
