package jcl.compiler.real.sa.specialoperator.compiler;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

@Component
public class DefstructAnalyzer implements Analyzer<LispStruct, ListStruct> {

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {
		return input;
	}
}
