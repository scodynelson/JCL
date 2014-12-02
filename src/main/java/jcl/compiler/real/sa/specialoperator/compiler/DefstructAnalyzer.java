package jcl.compiler.real.sa.specialoperator.compiler;

import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.SpecialOperatorAnalyzer;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

@Component
public class DefstructAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {
		return input;
	}
}
