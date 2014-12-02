package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class ReturnFromAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if ((input.size() < 2) || (input.size() > 3)) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: " + input.size() + ". Expected either 2 or 3 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new ProgramErrorException("RETURN-FROM: Label must be of type SymbolStruct. Got: " + second);
		}

		if (analysisBuilder.getBlockStack().search(second) == -1) {
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with Label " + second + " is visible.");
		}

		final List<LispStruct> returnFromResultList = new ArrayList<>(input.size());
		returnFromResultList.add(SpecialOperator.RETURN_FROM);
		returnFromResultList.add(second);

		if (input.size() == 3) {
			final LispStruct third = input.getRest().getRest().getFirst();
			final LispStruct returnFromResult = analyzer.analyzeForm(third, analysisBuilder);
			returnFromResultList.add(returnFromResult);
		}

		return ListStruct.buildProperList(returnFromResultList);
	}
}
