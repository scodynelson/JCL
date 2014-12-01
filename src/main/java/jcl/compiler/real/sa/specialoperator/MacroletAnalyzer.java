package jcl.compiler.real.sa.specialoperator;

import jcl.compiler.real.environment.Marker;
import org.springframework.stereotype.Component;

@Component
public class MacroletAnalyzer extends InnerFunctionAnalyzer {

	protected MacroletAnalyzer() {
		super("MACROLET", Marker.MACROLET, true);
	}
}
