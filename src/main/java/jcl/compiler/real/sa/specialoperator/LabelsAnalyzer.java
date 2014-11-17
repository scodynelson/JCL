package jcl.compiler.real.sa.specialoperator;

import jcl.compiler.real.environment.Marker;

public class LabelsAnalyzer extends InnerFunctionAnalyzer {

	public static final LabelsAnalyzer INSTANCE = new LabelsAnalyzer();

	protected LabelsAnalyzer() {
		super("LABELS", Marker.LABELS, true);
	}
}
