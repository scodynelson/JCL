package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class ThrowAnalyzer implements Analyzer<ListStruct, ListStruct> {

	public static final ThrowAnalyzer INSTANCE = new ThrowAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() != 3) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: " + input.size() + ". Expected 3 arguments.");
		}

		final List<LispStruct> throwResultList = new ArrayList<>(3);
		throwResultList.add(SpecialOperator.THROW);

		final LispStruct second = input.getRest().getFirst();
		final LispStruct secondAnalyzed = analyzer.analyzeForm(second);
		throwResultList.add(secondAnalyzed);

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct thirdAnalyzed = analyzer.analyzeForm(third);
		throwResultList.add(thirdAnalyzed);

		return ListStruct.buildProperList(throwResultList);
	}
}
