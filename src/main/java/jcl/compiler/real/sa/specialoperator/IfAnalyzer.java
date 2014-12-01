package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class IfAnalyzer implements Analyzer<ListStruct, ListStruct> {

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if ((input.size() < 3) || (input.size() > 4)) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: " + input.size() + ". Expected either 3 or 4 arguments.");
		}

		final List<LispStruct> ifResultList = new ArrayList<>(input.size());
		ifResultList.add(SpecialOperator.IF);

		final LispStruct second = input.getRest().getFirst();
		final LispStruct secondAnalyzed = analyzer.analyzeForm(second, analysisBuilder);
		ifResultList.add(secondAnalyzed);

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct thirdAnalyzed = analyzer.analyzeForm(third, analysisBuilder);
		ifResultList.add(thirdAnalyzed);

		if (input.size() == 4) {
			final LispStruct fourth = input.getRest().getRest().getRest().getFirst();
			final LispStruct fourthAnalyzed = analyzer.analyzeForm(fourth, analysisBuilder);
			ifResultList.add(fourthAnalyzed);
		}

		return ListStruct.buildProperList(ifResultList);
	}
}
