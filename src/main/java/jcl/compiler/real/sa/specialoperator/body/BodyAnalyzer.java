package jcl.compiler.real.sa.specialoperator.body;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.lists.ListStruct;

import java.util.ArrayList;
import java.util.List;

public class BodyAnalyzer implements Analyzer<BodyProcessingResult, ListStruct> {

	public static final BodyAnalyzer INSTANCE = new BodyAnalyzer();

	@Override
	public BodyProcessingResult analyze(final ListStruct input, final SemanticAnalyzer semanticAnalyzer) {
		final List<LispStruct> bodyJavaList = input.getAsJavaList();

		final List<LispStruct> bodyForms = new ArrayList<>(bodyJavaList.size());

		for (final LispStruct next : bodyJavaList) {
			final LispStruct analyzedForm = semanticAnalyzer.analyzeForm(next);
			bodyForms.add(analyzedForm);
		}

		return new BodyProcessingResult(null, null, bodyForms);
	}
}
