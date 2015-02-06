package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.FletElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.FletEnvironment;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class FletAnalyzer extends InnerFunctionAnalyzer<FletEnvironment, FletElement, FletElement.FletVar> {

	private static final long serialVersionUID = -3183832254183452606L;

	protected FletAnalyzer() {
		super("FLET", false);
	}

	@Override
	protected FletEnvironment getInnerFunctionEnvironment(final Environment parent, final int closureDepth) {
		return new FletEnvironment(parent, closureDepth);
	}

	@Override
	protected FletElement getFunctionElement(final List<FletElement.FletVar> vars, final List<Element> bodyForms,
	                                         final FletEnvironment lexicalEnvironment) {
		return new FletElement(vars, bodyForms, lexicalEnvironment);
	}

	@Override
	protected FletElement.FletVar getFunctionElementVar(final SymbolElement<?> var, final Element initForm) {
		return new FletElement.FletVar(var, initForm);
	}
}
