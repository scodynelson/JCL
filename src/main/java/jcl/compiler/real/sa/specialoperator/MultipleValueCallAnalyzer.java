package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class MultipleValueCallAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MultipleValueCallAnalyzer INSTANCE = new MultipleValueCallAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer semanticAnalyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final List<LispStruct> multipleValueCallResultList = new ArrayList<>();
		multipleValueCallResultList.add(SpecialOperator.MULTIPLE_VALUE_CALL);

		// Body includes the 'Function Form'
		final ListStruct body = input.getRest();
		final BodyProcessingUtil.BodyProcessingResult bodyProcessingResult = BodyProcessingUtil.processBody(semanticAnalyzer, body);
		multipleValueCallResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(multipleValueCallResultList);
	}
}
