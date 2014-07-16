package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class ThrowAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ThrowAnalyzer INSTANCE = new ThrowAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() != 3) {
			throw new RuntimeException("THROW: Incorrect number of arguments: " + input.size() + ". Expected 3 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		final LispStruct evaluatedSecond = SemanticAnalyzer.saMainLoop(second);

		if (CatchAnalyzer.CATCH_STACK.search(evaluatedSecond) == -1) {
			throw new RuntimeException("THROW: No CATCH with Tag " + evaluatedSecond + " is visible.");
		}

		final List<LispStruct> throwResultList = new ArrayList<>();
		throwResultList.add(SpecialOperator.THROW);
		throwResultList.add(second);

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct throwResult = SemanticAnalyzer.saMainLoop(third);
		throwResultList.add(throwResult);

		return ListStruct.buildProperList(throwResultList);
	}
}
