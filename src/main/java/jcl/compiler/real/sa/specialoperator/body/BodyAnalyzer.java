package jcl.compiler.real.sa.specialoperator.body;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class BodyAnalyzer implements Analyzer<BodyProcessingResult, ListStruct> {

	@Override
	public BodyProcessingResult analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {
		final List<LispStruct> bodyJavaList = input.getAsJavaList();

		final List<LispStruct> bodyForms = bodyJavaList
				.stream()
				.map(e -> analyzer.analyzeForm(e, analysisBuilder))
				.collect(Collectors.toList());

		return new BodyProcessingResult(null, null, bodyForms);
	}
}
