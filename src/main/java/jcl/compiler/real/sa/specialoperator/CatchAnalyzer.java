package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class CatchAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final CatchAnalyzer INSTANCE = new CatchAnalyzer();

	public static final Stack<LispStruct> CATCH_STACK = new Stack<>();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("CATCH: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		final LispStruct evaluatedSecond = SemanticAnalyzer.saMainLoop(second);

		CATCH_STACK.push(evaluatedSecond);

		try {
			final ListStruct catchBody = input.getRest().getRest();
			final ListStruct prognResults = PrognAnalyzer.INSTANCE.analyze(catchBody);
			final List<LispStruct> javaPrognResults = prognResults.getAsJavaList();

			final List<LispStruct> catchResultList = new ArrayList<>();
			catchResultList.add(SpecialOperator.CATCH);
			catchResultList.add(evaluatedSecond);
			catchResultList.addAll(javaPrognResults);

			return ListStruct.buildProperList(catchResultList);
		} finally {
			CATCH_STACK.pop();
		}
	}
}
