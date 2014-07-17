package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class MultipleValueCallAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MultipleValueCallAnalyzer INSTANCE = new MultipleValueCallAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("MULTIPLE-VALUE-CALL: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		final LispStruct secondAnalyzed = SemanticAnalyzer.saMainLoop(second);

		final ListStruct multipleValueCallBody = input.getRest().getRest();
		final ListStruct prognResults = PrognAnalyzer.INSTANCE.analyze(multipleValueCallBody);
		final List<LispStruct> javaPrognResults = prognResults.getAsJavaList();

		final List<LispStruct> multipleValueCallResultList = new ArrayList<>();
		multipleValueCallResultList.add(SpecialOperator.MULTIPLE_VALUE_CALL);
		multipleValueCallResultList.add(secondAnalyzed);
		multipleValueCallResultList.addAll(javaPrognResults);

		return ListStruct.buildProperList(multipleValueCallResultList);
	}
}
