package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.element.LabelsElement;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class LabelsAnalyzer extends InnerFunctionAnalyzer<LabelsElement, LabelsElement.LabelsVar> {

	protected LabelsAnalyzer() {
		super("LABELS", Marker.LABELS, true);
	}

	@Override
	protected LabelsElement getFunctionElement(final List<LabelsElement.LabelsVar> vars, final List<LispStruct> bodyForms, final Environment environment) {
		return new LabelsElement(vars, bodyForms, environment);
	}

	@Override
	protected LabelsElement.LabelsVar getFunctionElementVar(final SymbolStruct<?> var, final LispStruct initForm) {
		return new LabelsElement.LabelsVar(var, initForm);
	}
}
