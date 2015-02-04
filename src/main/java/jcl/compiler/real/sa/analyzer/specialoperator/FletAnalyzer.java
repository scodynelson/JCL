package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.FletElement;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class FletAnalyzer extends InnerFunctionAnalyzer<FletElement, FletElement.FletVar> {

	private static final long serialVersionUID = -3183832254183452606L;

	protected FletAnalyzer() {
		super("FLET", Marker.FLET, false);
	}

	@Override
	protected FletElement getFunctionElement(final List<FletElement.FletVar> vars, final List<Element> bodyForms,
	                                         final LexicalEnvironment lexicalEnvironment) {
		return new FletElement(vars, bodyForms, lexicalEnvironment);
	}

	@Override
	protected FletElement.FletVar getFunctionElementVar(final SymbolElement<?> var, final Element initForm) {
		return new FletElement.FletVar(var, initForm);
	}
}
