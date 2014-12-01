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
public class UnwindProtectAnalyzer implements Analyzer<ListStruct, ListStruct> {

	@Autowired
	private BodyAnalyzer bodyAnalyzer;

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final List<LispStruct> unwindProtectResultList = new ArrayList<>();
		unwindProtectResultList.add(SpecialOperator.UNWIND_PROTECT);

		// Body includes the 'Protected Form'
		final ListStruct body = input.getRest();
		final BodyProcessingResult bodyProcessingResult = bodyAnalyzer.analyze(body, analyzer);
		unwindProtectResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(unwindProtectResultList);
	}
}
