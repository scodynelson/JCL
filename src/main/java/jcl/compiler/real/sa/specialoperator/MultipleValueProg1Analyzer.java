package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class MultipleValueProg1Analyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MultipleValueProg1Analyzer INSTANCE = new MultipleValueProg1Analyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("MULTIPLE-VALUE-PROG1: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		// NOTE: This will evaluate the first form first and keep it to return last. This does not happen in the Analyzer, but later in the Generator.

		final LispStruct second = input.getRest().getFirst();
		final LispStruct secondAnalyzed = SemanticAnalyzer.saMainLoop(second);

		final ListStruct multipleValueProg1Body = input.getRest().getRest();
		final ListStruct prognResults = PrognAnalyzer.INSTANCE.analyze(multipleValueProg1Body);
		final List<LispStruct> javaPrognResults = prognResults.getAsJavaList();

		final List<LispStruct> multipleValueProg1ResultList = new ArrayList<>();
		multipleValueProg1ResultList.add(SpecialOperator.MULTIPLE_VALUE_PROG1);
		multipleValueProg1ResultList.add(secondAnalyzed);
		multipleValueProg1ResultList.addAll(javaPrognResults);

		return ListStruct.buildProperList(multipleValueProg1ResultList);
	}
}
