package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;

import java.util.ArrayList;
import java.util.List;

public class PrognAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final PrognAnalyzer INSTANCE = new PrognAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {
		final ListStruct listAfterProgn = input.getRest();
		final List<LispStruct> javaListAfterProgn = listAfterProgn.getAsJavaList();

		final List<LispStruct> resultAnalysisList = new ArrayList<>();

		for (final LispStruct lispStruct : javaListAfterProgn) {
			final LispStruct saResult = SemanticAnalyzer.saMainLoop(lispStruct);
			resultAnalysisList.add(saResult);
		}

		return ListStruct.buildProperList(resultAnalysisList);
	}
}
