package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.LabelsElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LabelsEnvironment;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class LabelsAnalyzer extends InnerFunctionAnalyzer<LabelsEnvironment, LabelsElement, LabelsElement.LabelsVar> {

	private static final long serialVersionUID = -3698985413039911540L;

	protected LabelsAnalyzer() {
		super("LABELS", true);
	}

	@Override
	protected LabelsEnvironment getInnerFunctionEnvironment(final Environment parent, final int closureDepth) {
		return new LabelsEnvironment(parent, closureDepth);
	}

	@Override
	protected LabelsElement getFunctionElement(final List<LabelsElement.LabelsVar> vars, final List<Element> bodyForms,
	                                           final LabelsEnvironment lexicalEnvironment) {
		return new LabelsElement(vars, bodyForms, lexicalEnvironment);
	}

	@Override
	protected LabelsElement.LabelsVar getFunctionElementVar(final SymbolElement<?> var, final Element initForm) {
		return new LabelsElement.LabelsVar(var, initForm);
	}
}
