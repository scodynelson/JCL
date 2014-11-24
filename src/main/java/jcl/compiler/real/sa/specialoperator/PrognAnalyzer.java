package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class PrognAnalyzer implements Analyzer<ListStruct, ListStruct> {

	public static final PrognAnalyzer INSTANCE = new PrognAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		final List<LispStruct> prognResultList = new ArrayList<>();
		prognResultList.add(SpecialOperator.PROGN);

		final ListStruct body = input.getRest();
		final BodyProcessingResult bodyProcessingResult = BodyAnalyzer.INSTANCE.analyze(body, analyzer);
		prognResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(prognResultList);
	}
}
