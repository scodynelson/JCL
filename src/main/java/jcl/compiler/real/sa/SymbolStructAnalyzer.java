package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.structs.symbols.SymbolStruct;

public class SymbolStructAnalyzer implements Analyzer<LispStruct, SymbolStruct<?>> {

	public static final SymbolStructAnalyzer INSTANCE = new SymbolStructAnalyzer();

	@Override
	public LispStruct analyze(final SymbolStruct<?> input) {
		EnvironmentAccessor.addSymbolToTable(SemanticAnalyzer.environmentStack.peek(), input);
		return input;
	}
}
