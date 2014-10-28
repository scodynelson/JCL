package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class ThrowAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ThrowAnalyzer INSTANCE = new ThrowAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() != 3) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: " + input.size() + ". Expected 3 arguments.");
		}

		final List<LispStruct> throwResultList = new ArrayList<>();
		throwResultList.add(SpecialOperator.THROW);

		final LispStruct second = input.getRest().getFirst();
		final LispStruct secondAnalyzed = SemanticAnalyzer.saMainLoop(second);
		throwResultList.add(secondAnalyzed);

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct thirdAnalyzed = SemanticAnalyzer.saMainLoop(third);
		throwResultList.add(thirdAnalyzed);

		return ListStruct.buildProperList(throwResultList);
	}
}
