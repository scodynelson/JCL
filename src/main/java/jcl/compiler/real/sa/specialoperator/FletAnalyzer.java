package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.element.FletElement;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class FletAnalyzer extends InnerFunctionAnalyzer<FletElement, FletElement.FletVar> {

	protected FletAnalyzer() {
		super("FLET", Marker.FLET, false);
	}

	@Override
	protected FletElement getFunctionElement(final List<FletElement.FletVar> vars, final List<LispStruct> bodyForms,
	                                         final LexicalEnvironment lexicalEnvironment) {
		return new FletElement(vars, bodyForms, lexicalEnvironment);
	}

	@Override
	protected FletElement.FletVar getFunctionElementVar(final SymbolStruct<?> var, final LispStruct initForm) {
		return new FletElement.FletVar(var, initForm);
	}
}
