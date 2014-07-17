package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class PrognAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final PrognAnalyzer INSTANCE = new PrognAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		final ListStruct body = input.getRest();
		final List<LispStruct> bodyJavaList = body.getAsJavaList();

		final List<LispStruct> prognResultList = new ArrayList<>();
		prognResultList.add(SpecialOperator.PROGN);

		for (final LispStruct bodyForm : bodyJavaList) {
			final LispStruct saResult = SemanticAnalyzer.saMainLoop(bodyForm);
			prognResultList.add(saResult);
		}

		return ListStruct.buildProperList(prognResultList);
	}
}
