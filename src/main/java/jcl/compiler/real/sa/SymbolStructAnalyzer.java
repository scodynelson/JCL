package jcl.compiler.real.sa;

import jcl.compiler.old.EnvironmentAccessor;
import jcl.symbols.SymbolStruct;

public class SymbolStructAnalyzer implements Analyzer<SymbolStruct<?>, SymbolStruct<?>> {

	public static final Analyzer<SymbolStruct<?>, SymbolStruct<?>> INSTANCE = new SymbolStructAnalyzer();

	@Override
	public SymbolStruct<?> analyze(final SymbolStruct input) {
		EnvironmentAccessor.addSymbolToTable(SemanticAnalyzer.environmentStack.peek(), input);
		return input;
	}
}
