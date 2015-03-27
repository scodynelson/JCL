package jcl.compiler.real.sa.analyzer;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import org.springframework.stereotype.Component;

@Component
public class DefstructExpander extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = 5336983779662053736L;

	/**
	 * Initializes the defstruct macro function and adds it to the special operator 'defstruct'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.DEFSTRUCT.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final Environment environment) {
		// TODO: what do we do here???
		return null;
	}
}
