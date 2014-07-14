package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SymbolMacroletAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final SymbolMacroletAnalyzer INSTANCE = new SymbolMacroletAnalyzer();

	private static final Logger LOGGER = LoggerFactory.getLogger(SymbolMacroletAnalyzer.class);

	@Override
	public LispStruct analyze(final ListStruct input) {
		LOGGER.error("; Warning: SYMBOL-MACROLET not supported at this time.");
		return input;
	}
}
