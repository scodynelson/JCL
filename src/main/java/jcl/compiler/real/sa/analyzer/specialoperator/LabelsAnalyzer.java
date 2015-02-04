package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.LabelsElement;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class LabelsAnalyzer extends InnerFunctionAnalyzer<LabelsElement, LabelsElement.LabelsVar> {

	private static final long serialVersionUID = -3698985413039911540L;

	protected LabelsAnalyzer() {
		super("LABELS", Marker.LABELS, true);
	}

	@Override
	protected LabelsElement getFunctionElement(final List<LabelsElement.LabelsVar> vars, final List<Element> bodyForms,
	                                           final LexicalEnvironment lexicalEnvironment) {
		return new LabelsElement(vars, bodyForms, lexicalEnvironment);
	}

	@Override
	protected LabelsElement.LabelsVar getFunctionElementVar(final SymbolElement<?> var, final Element initForm) {
		return new LabelsElement.LabelsVar(var, initForm);
	}
}
