package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.element.LabelsElement;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class LabelsAnalyzer extends InnerFunctionAnalyzer<LabelsElement, LabelsElement.LabelsVar> {

	private static final long serialVersionUID = -3698985413039911540L;

	protected LabelsAnalyzer() {
		super("LABELS", Marker.LABELS, true);
	}

	@Override
	protected LabelsElement getFunctionElement(final List<LabelsElement.LabelsVar> vars, final List<LispStruct> bodyForms,
	                                           final LexicalEnvironment lexicalEnvironment) {
		return new LabelsElement(vars, bodyForms, lexicalEnvironment);
	}

	@Override
	protected LabelsElement.LabelsVar getFunctionElementVar(final SymbolStruct<?> var, final LispStruct initForm) {
		return new LabelsElement.LabelsVar(var, initForm);
	}
}
