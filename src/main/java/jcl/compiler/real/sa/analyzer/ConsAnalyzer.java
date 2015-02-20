package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;

@Component
public class ConsAnalyzer implements Analyzer<Element, ConsStruct> {

	private static final long serialVersionUID = 5454983196467731873L;

	@Resource
	private Map<Class<? extends LispStruct>, Analyzer<? extends Element, LispStruct>> functionCallAnalyzerStrategies;

	@Override
	public Element analyze(final SemanticAnalyzer analyzer, final ConsStruct input, final AnalysisBuilder analysisBuilder) {

		final LispStruct first = input.getFirst();
		final Analyzer<? extends Element, LispStruct> functionCallAnalyzer = functionCallAnalyzerStrategies.get(first.getClass());
		if (functionCallAnalyzer == null) {
			throw new ProgramErrorException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		}

		return functionCallAnalyzer.analyze(analyzer, input, analysisBuilder);
	}
}
