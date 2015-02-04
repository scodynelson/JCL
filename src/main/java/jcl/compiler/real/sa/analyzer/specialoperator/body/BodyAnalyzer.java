package jcl.compiler.real.sa.analyzer.specialoperator.body;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.element.Element;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class BodyAnalyzer implements Serializable {

	private static final long serialVersionUID = -2597125456041175077L;

	public List<Element> analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {
		final List<LispStruct> bodyJavaList = input.getAsJavaList();

		return bodyJavaList
				.stream()
				.map(e -> analyzer.analyzeForm(e, analysisBuilder))
				.collect(Collectors.toList());
	}
}
