package jcl.compiler.real.sa.analyzer.specialoperator.compiler;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.SpecialOperatorAnalyzer;
import org.springframework.stereotype.Component;

@Component
public class DefstructAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 5336983779662053736L;

	@Override
	public Element analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {
		// TODO: what do we do here???
		return null;
	}
}
