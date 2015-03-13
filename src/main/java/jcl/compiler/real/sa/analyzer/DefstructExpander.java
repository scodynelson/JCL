package jcl.compiler.real.sa.analyzer;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.stereotype.Component;

@Component
public class DefstructExpander extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = 5336983779662053736L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.DEFSTRUCT.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final Environment environment) {
		// TODO: what do we do here???
		return null;
	}
}
