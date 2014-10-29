package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class MultipleValueProg1Analyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MultipleValueProg1Analyzer INSTANCE = new MultipleValueProg1Analyzer();

	@Override
	public LispStruct analyze(final ListStruct input, final SemanticAnalyzer semanticAnalyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-PROG1: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final List<LispStruct> multipleValueProg1ResultList = new ArrayList<>();
		multipleValueProg1ResultList.add(SpecialOperator.MULTIPLE_VALUE_PROG1);

		// Body includes the 'First Form'
		final ListStruct body = input.getRest();
		final BodyProcessingUtil.BodyProcessingResult bodyProcessingResult = BodyProcessingUtil.processBody(semanticAnalyzer, body);
		multipleValueProg1ResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(multipleValueProg1ResultList);
	}
}
