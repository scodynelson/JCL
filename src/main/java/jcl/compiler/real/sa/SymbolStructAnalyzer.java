package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.structs.symbols.SymbolStruct;

import java.util.Stack;

public class SymbolStructAnalyzer implements Analyzer<LispStruct, SymbolStruct<?>> {

	public static final SymbolStructAnalyzer INSTANCE = new SymbolStructAnalyzer();

	@Override
	public LispStruct analyze(final SymbolStruct<?> input, final SemanticAnalyzer analyzer) {
		final Stack<Environment> environmentStack = analyzer.getEnvironmentStack();
		EnvironmentAccessor.addSymbolToTable(environmentStack.peek(), input);
		return input;
	}
}
