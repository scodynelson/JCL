package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class MultipleValueCallAnalyzer implements Analyzer<ListStruct, ListStruct> {

	@Autowired
	private BodyAnalyzer bodyAnalyzer;

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final List<LispStruct> multipleValueCallResultList = new ArrayList<>();
		multipleValueCallResultList.add(SpecialOperator.MULTIPLE_VALUE_CALL);

		// Body includes the 'Function Form'
		final ListStruct body = input.getRest();
		final BodyProcessingResult bodyProcessingResult = bodyAnalyzer.analyze(body, analyzer);
		multipleValueCallResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(multipleValueCallResultList);
	}
}
