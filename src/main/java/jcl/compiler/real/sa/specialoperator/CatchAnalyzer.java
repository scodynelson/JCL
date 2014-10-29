package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class CatchAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final CatchAnalyzer INSTANCE = new CatchAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new ProgramErrorException("CATCH: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final List<LispStruct> catchResultList = new ArrayList<>();
		catchResultList.add(SpecialOperator.CATCH);

		// Body includes the 'Catch Tag'
		final ListStruct body = input.getRest();
		final BodyProcessingUtil.BodyProcessingResult bodyProcessingResult = BodyProcessingUtil.processBody(body);
		catchResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(catchResultList);
	}
}
