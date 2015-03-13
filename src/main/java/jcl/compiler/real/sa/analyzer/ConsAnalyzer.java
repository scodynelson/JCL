package jcl.compiler.real.sa.analyzer;

import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.Analyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import org.springframework.stereotype.Component;

@Component
public class ConsAnalyzer implements Analyzer<LispStruct, ConsStruct> {

	private static final long serialVersionUID = 5454983196467731873L;

	@Resource
	private Map<Class<? extends LispStruct>, Analyzer<? extends LispStruct, LispStruct>> functionCallAnalyzerStrategies;

	@Override
	public LispStruct analyze(final ConsStruct input, final Environment environment) {

		final LispStruct first = input.getFirst();
		final Analyzer<? extends LispStruct, LispStruct> functionCallAnalyzer = functionCallAnalyzerStrategies.get(first.getClass());
		if (functionCallAnalyzer == null) {
			throw new ProgramErrorException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		}

		return functionCallAnalyzer.analyze(input, environment);
	}
}
