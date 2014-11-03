package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class LocallyAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LocallyAnalyzer INSTANCE = new LocallyAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		final ListStruct body = input.getRest();
		final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAnalyzer.INSTANCE.analyze(body, analyzer);

		final List<LispStruct> locallyResultList = new ArrayList<>();
		locallyResultList.add(SpecialOperator.LOCALLY);
		locallyResultList.addAll(bodyProcessingResult.getDeclarations()); // TODO: do we add these here really???
		locallyResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(locallyResultList);
	}
}
