package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class LocallyAnalyzer implements Analyzer<ListStruct, ListStruct> {

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		final ListStruct body = input.getRest();
		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(body, analyzer);

		final List<LispStruct> locallyResultList = new ArrayList<>();
		locallyResultList.add(SpecialOperator.LOCALLY);
		locallyResultList.addAll(bodyProcessingResult.getDeclarations()); // TODO: do we add these here really???
		locallyResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(locallyResultList);
	}
}
