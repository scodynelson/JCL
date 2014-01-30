package jcl.types.lambdalist.variable;

import java.util.List;

/**
 * If &optional is present, the optional parameter specifiers are those following &optional up to the next lambda list
 * keyword or the end of the list. If optional parameters are specified, then each one is processed as follows. If any
 * unprocessed arguments remain, then the parameter variable var is bound to the next remaining argument, just as for a
 * required parameter. If no arguments remain, however, then init-form is evaluated, and the parameter variable is bound
 * to the resulting value (or to nil if no init-form appears in the parameter specifier). If another variable name
 * supplied-p-parameter appears in the specifier, it is bound to true if an argument had been available, and to false if
 * no argument remained (and therefore init-form had to be evaluated). Supplied-p-parameter is bound not to an argument
 * but to a value indicating whether or not an argument had been supplied for the corresponding var.
 *
 * @param <VAR>
 */
public class Optional<VAR> {

	private final List<OptionalVar<VAR>> vars;

	public Optional(final List<OptionalVar<VAR>> vars) {
		this.vars = vars;
	}

	public List<OptionalVar<VAR>> getVars() {
		return vars;
	}

	@Override
	public String toString() {
		return "Optional{" +
				"vars=" + vars +
				'}';
	}

	public static class OptionalVar<TYPE> {

		private final String var;
		private final TYPE initForm;
		private final boolean suppliedP;

		public OptionalVar(final String var) {
			this.var = var;
			initForm = null;
			suppliedP = false;
		}

		public OptionalVar(final String var, final TYPE initForm) {
			this.var = var;
			this.initForm = initForm;
			suppliedP = true;
		}

		public String getVar() {
			return var;
		}

		public TYPE getInitForm() {
			return initForm;
		}

		public boolean isSuppliedP() {
			return suppliedP;
		}

		@Override
		public String toString() {
			return "OptionalVar{" +
					"var='" + var + '\'' +
					", initForm=" + initForm +
					", suppliedP=" + suppliedP +
					'}';
		}
	}
}
