package jcl.compiler.real.sa.specialoperator;

import jcl.compiler.real.environment.Marker;
import org.springframework.stereotype.Component;

@Component
public class FletAnalyzer extends InnerFunctionAnalyzer {

	protected FletAnalyzer() {
		super("FLET", Marker.FLET, false);
	}
}
