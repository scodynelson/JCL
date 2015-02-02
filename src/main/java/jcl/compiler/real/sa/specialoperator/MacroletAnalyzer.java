package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.element.MacroletElement;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class MacroletAnalyzer extends InnerFunctionAnalyzer<MacroletElement, MacroletElement.MacroletVar> {

	protected MacroletAnalyzer() {
		super("MACROLET", Marker.MACROLET, true);
	}

	@Override
	protected MacroletElement getFunctionElement(final List<MacroletElement.MacroletVar> vars, final List<LispStruct> bodyForms,
	                                             final LexicalEnvironment lexicalEnvironment) {
		return new MacroletElement(vars, bodyForms, lexicalEnvironment);
	}

	@Override
	protected MacroletElement.MacroletVar getFunctionElementVar(final SymbolStruct<?> var, final LispStruct initForm) {
		return new MacroletElement.MacroletVar(var, initForm);
	}
}
