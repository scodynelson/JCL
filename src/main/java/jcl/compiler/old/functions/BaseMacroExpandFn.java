package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.compiler.old.expander.MacroFunctionExpander;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.lists.ListStruct;

// Now we make the base function for *macroexpand-hook*
public class BaseMacroExpandFn extends FunctionStruct {

	public static final BaseMacroExpandFn FUNCTION = new BaseMacroExpandFn();

	private BaseMacroExpandFn() {
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return funcall((MacroFunctionExpander) lispStructs[0], (ListStruct) lispStructs[1], lispStructs[2]);
	}

	public LispStruct funcall(final MacroFunctionExpander expander, final ListStruct form, final LispStruct env) {
		return expander.expand(form, env);
	}
}