package jcl.compiler.real.sa.analyzer.specialoperator.compiler;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.analyzer.specialoperator.SpecialOperatorAnalyzer;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

@Component
public class DefstructAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 5336983779662053736L;

	@Override
	public LispStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {
		// TODO: what do we do here???
		return null;
	}
}
