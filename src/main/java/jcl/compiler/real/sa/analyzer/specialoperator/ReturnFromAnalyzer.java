package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.ReturnFromElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class ReturnFromAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 3328790948675693554L;

	@Override
	public ReturnFromElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if ((input.size() < 2) || (input.size() > 3)) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: " + input.size() + ". Expected either 2 or 3 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new ProgramErrorException("RETURN-FROM: Name must be of type SymbolStruct. Got: " + second);
		}

		final SymbolStruct<?> name = (SymbolStruct) second;

		if (analysisBuilder.getBlockStack().search(name) == -1) {
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with name " + second + " is visible.");
		}

		if (input.size() == 3) {
			final LispStruct result = input.getRest().getRest().getFirst();
			final LispStruct analyzedResult = analyzer.analyzeForm(result, analysisBuilder);
			return new ReturnFromElement(name, analyzedResult);
		} else {
			return new ReturnFromElement(name);
		}
	}
}
