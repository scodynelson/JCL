package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class MultipleValueProg1Analyzer implements Analyzer<ListStruct, ListStruct> {

	public static final MultipleValueProg1Analyzer INSTANCE = new MultipleValueProg1Analyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-PROG1: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final List<LispStruct> multipleValueProg1ResultList = new ArrayList<>();
		multipleValueProg1ResultList.add(SpecialOperator.MULTIPLE_VALUE_PROG1);

		// Body includes the 'First Form'
		final ListStruct body = input.getRest();
		final BodyProcessingResult bodyProcessingResult = BodyAnalyzer.INSTANCE.analyze(body, analyzer);
		multipleValueProg1ResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(multipleValueProg1ResultList);
	}
}
