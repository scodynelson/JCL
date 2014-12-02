package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class ThrowAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 3) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: " + input.size() + ". Expected 3 arguments.");
		}

		final List<LispStruct> throwResultList = new ArrayList<>(3);
		throwResultList.add(SpecialOperator.THROW);

		final LispStruct second = input.getRest().getFirst();
		final LispStruct secondAnalyzed = analyzer.analyzeForm(second, analysisBuilder);
		throwResultList.add(secondAnalyzed);

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct thirdAnalyzed = analyzer.analyzeForm(third, analysisBuilder);
		throwResultList.add(thirdAnalyzed);

		return ListStruct.buildProperList(throwResultList);
	}
}
