package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class ProgvAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ProgvAnalyzer INSTANCE = new ProgvAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {
		// TODO: This is kind of like a 'let' for DYNAMIC bindings. The code below is temporary, but does not take care of the environment pieces...

		if (input.size() < 3) {
			throw new RuntimeException("PROGV: Incorrect number of arguments: " + input.size() + ". Expected at least 3 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new RuntimeException("PROGV: Symbols list must be of type ListStruct. Got: " + second);
		}
		final LispStruct secondAnalyzed = SemanticAnalyzer.saMainLoop(second);

		final LispStruct third = input.getRest().getRest().getFirst();
		if (!(third instanceof ListStruct)) {
			throw new RuntimeException("PROGV: Values list must be of type ListStruct. Got: " + third);
		}
		final LispStruct thirdAnalyzed = SemanticAnalyzer.saMainLoop(third);

		final ListStruct provBody = input.getRest().getRest().getRest();
		final ListStruct prognResults = PrognAnalyzer.INSTANCE.analyze(provBody);
		final List<LispStruct> javaPrognResults = prognResults.getAsJavaList();

		final List<LispStruct> progvResultList = new ArrayList<>();
		progvResultList.add(SpecialOperator.PROGV);
		progvResultList.add(secondAnalyzed);
		progvResultList.add(thirdAnalyzed);
		progvResultList.addAll(javaPrognResults);

		return ListStruct.buildProperList(progvResultList);
	}
}
