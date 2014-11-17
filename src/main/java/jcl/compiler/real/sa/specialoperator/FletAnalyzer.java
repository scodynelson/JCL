package jcl.compiler.real.sa.specialoperator;

import jcl.compiler.real.environment.Marker;

public class FletAnalyzer extends InnerFunctionAnalyzer {

	public static final FletAnalyzer INSTANCE = new FletAnalyzer();

	protected FletAnalyzer() {
		super("FLET", Marker.FLET, false);
	}
}
