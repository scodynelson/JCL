package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class PrognAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final PrognAnalyzer INSTANCE = new PrognAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		final List<LispStruct> prognResultList = new ArrayList<>();
		prognResultList.add(SpecialOperator.PROGN);

		final ListStruct body = input.getRest();
		final BodyProcessingUtil.BodyProcessingResult bodyProcessingResult = BodyProcessingUtil.processBody(body);
		prognResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(prognResultList);
	}
}
