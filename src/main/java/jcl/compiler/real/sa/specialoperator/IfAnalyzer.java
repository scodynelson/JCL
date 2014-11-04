package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class IfAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final IfAnalyzer INSTANCE = new IfAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if ((input.size() < 3) || (input.size() > 4)) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: " + input.size() + ". Expected either 3 or 4 arguments.");
		}

		final List<LispStruct> ifResultList = new ArrayList<>(input.size());
		ifResultList.add(SpecialOperator.IF);

		final LispStruct second = input.getRest().getFirst();
		final LispStruct secondAnalyzed = analyzer.analyzeForm(second);
		ifResultList.add(secondAnalyzed);

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct thirdAnalyzed = analyzer.analyzeForm(third);
		ifResultList.add(thirdAnalyzed);

		final LispStruct fourth = input.getRest().getRest().getRest().getFirst();
		final LispStruct fourthAnalyzed = analyzer.analyzeForm(fourth);
		ifResultList.add(fourthAnalyzed);

		return ListStruct.buildProperList(ifResultList);
	}
}
