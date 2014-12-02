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
public class TheAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 3) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: " + input.size() + ". Expected 3 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct) && !(second instanceof ListStruct)) {
			throw new ProgramErrorException("THE: Tag must be of type SymbolStruct or ListStruct. Got: " + second);
		}

		final List<LispStruct> theResultList = new ArrayList<>(3);
		theResultList.add(SpecialOperator.THE);
		theResultList.add(second);

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct thirdAnalyzed = analyzer.analyzeForm(third, analysisBuilder);
		theResultList.add(thirdAnalyzed);

		return ListStruct.buildProperList(theResultList);
	}
}
