package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.Analyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConsAnalyzer implements Analyzer<LispStruct, ConsStruct> {

	private static final long serialVersionUID = 5454983196467731873L;

	@Autowired
	private SymbolFunctionCallAnalyzer symbolFunctionCallAnalyzer;

	@Autowired
	private LambdaFunctionCallAnalyzer lambdaFunctionCallAnalyzer;

	@Override
	public LispStruct analyze(final ConsStruct input, final Environment environment) {

		final LispStruct first = input.getFirst();

		if (first instanceof SymbolStruct) {
			return symbolFunctionCallAnalyzer.analyze(input, environment);
		} else if (first instanceof ConsStruct) {
			return lambdaFunctionCallAnalyzer.analyze(input, environment);
		} else {
			throw new ProgramErrorException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		}
	}
}
