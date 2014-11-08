package jcl.compiler.real.sa.specialoperator;

import jcl.compiler.real.environment.Marker;

public class MacroletAnalyzer extends InnerFunctionAnalyzer {

	public static final MacroletAnalyzer INSTANCE = new MacroletAnalyzer();

	protected MacroletAnalyzer() {
		super("MACROLET", Marker.MACROLET, true);
	}
}
