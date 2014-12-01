package jcl.compiler.real.sa.specialoperator;

import jcl.compiler.real.environment.Marker;
import org.springframework.stereotype.Component;

@Component
public class LabelsAnalyzer extends InnerFunctionAnalyzer {

	protected LabelsAnalyzer() {
		super("LABELS", Marker.LABELS, true);
	}
}
