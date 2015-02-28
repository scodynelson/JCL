package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;

@Component
public class ConsAnalyzer implements Analyzer<Element, ConsElement> {

	private static final long serialVersionUID = 5454983196467731873L;

	@Resource
	private Map<Class<? extends SimpleElement>, Analyzer<? extends Element, SimpleElement>> functionCallAnalyzerStrategies;

	@Override
	public Element analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final SimpleElement first = elements.getFirst();
		final Analyzer<? extends Element, SimpleElement> functionCallAnalyzer = functionCallAnalyzerStrategies.get(first.getClass());
		if (functionCallAnalyzer == null) {
			throw new ProgramErrorException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		}

		return functionCallAnalyzer.analyze(input, analysisBuilder);
	}
}
