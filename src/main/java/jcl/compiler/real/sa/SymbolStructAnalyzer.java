package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.Stack;

@Component
public class SymbolStructAnalyzer implements Analyzer<LispStruct, SymbolStruct<?>> {

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {
		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();
		EnvironmentAccessor.addSymbolToTable(environmentStack.peek(), input);
		return input;
	}
}
