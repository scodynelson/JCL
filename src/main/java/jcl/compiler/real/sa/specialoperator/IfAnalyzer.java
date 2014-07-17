package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class IfAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final IfAnalyzer INSTANCE = new IfAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if ((input.size() < 3) || (input.size() > 4)) {
			throw new RuntimeException("IF: Incorrect number of arguments: " + input.size() + ". Expected either 3 or 4 arguments.");
		}

		final List<LispStruct> ifResultList = new ArrayList<>();
		ifResultList.add(SpecialOperator.IF);

		final LispStruct second = input.getRest().getFirst();
		final LispStruct secondAnalyzed = SemanticAnalyzer.saMainLoop(second);
		ifResultList.add(secondAnalyzed);

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct thirdAnalyzed = SemanticAnalyzer.saMainLoop(third);
		ifResultList.add(thirdAnalyzed);

		final LispStruct fourth = input.getRest().getRest().getRest().getFirst();
		final LispStruct fourthAnalyzed = SemanticAnalyzer.saMainLoop(fourth);
		ifResultList.add(fourthAnalyzed);

		return ListStruct.buildProperList(ifResultList);
	}
}
