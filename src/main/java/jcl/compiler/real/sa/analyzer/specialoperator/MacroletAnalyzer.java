package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.MacroletElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.MacroletEnvironment;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class MacroletAnalyzer extends InnerFunctionAnalyzer<MacroletEnvironment, MacroletElement, MacroletElement.MacroletVar> {

	private static final long serialVersionUID = 920568167525914860L;

	protected MacroletAnalyzer() {
		super("MACROLET", true);
	}

	@Override
	protected MacroletEnvironment getInnerFunctionEnvironment(final Environment parent, final int closureDepth) {
		return new MacroletEnvironment(parent, closureDepth);
	}

	@Override
	protected MacroletElement getFunctionElement(final List<MacroletElement.MacroletVar> vars, final List<Element> bodyForms,
	                                             final MacroletEnvironment lexicalEnvironment) {
		return new MacroletElement(vars, bodyForms, lexicalEnvironment);
	}

	@Override
	protected MacroletElement.MacroletVar getFunctionElementVar(final SymbolElement<?> var, final Element initForm) {
		return new MacroletElement.MacroletVar(var, initForm);
	}
}
