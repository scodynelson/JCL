package jcl.compiler.real.sa.specialoperator.body;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class BodyAnalyzer implements Analyzer<BodyProcessingResult, ListStruct> {

	@Override
	public BodyProcessingResult analyze(final ListStruct input, final SemanticAnalyzer analyzer) {
		final List<LispStruct> bodyJavaList = input.getAsJavaList();

		final List<LispStruct> bodyForms = new ArrayList<>(bodyJavaList.size());

		for (final LispStruct next : bodyJavaList) {
			final LispStruct analyzedForm = analyzer.analyzeForm(next);
			bodyForms.add(analyzedForm);
		}

		return new BodyProcessingResult(null, null, bodyForms);
	}
}
