package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class SymbolAnalyzerImpl implements SymbolAnalyzer {

	private static final long serialVersionUID = 4236867001501188408L;

	@Override
	public SymbolStruct<?> analyze(final SymbolStruct<?> input, final Environment environment) {
		return input;
	}
}
