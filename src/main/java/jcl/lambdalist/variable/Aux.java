package jcl.lambdalist.variable;

import java.util.List;

/**
 * These are not really parameters. If the lambda list keyword &amp;aux is present, all specifiers after it are
 * auxiliary variable specifiers. After all parameter specifiers have been processed, the auxiliary variable specifiers
 * (those following &amp;aux) are processed from left to right. For each one, init-form is evaluated and var is bound to
 * that value (or to nil if no init-form was specified). &amp;aux variable processing is analogous to let* processing.
 * <p>
 * (lambda (x y &amp;aux (a (car x)) (b 2) c) (list x y a b c))
 * ==
 * (lambda (x y) (let* ((a (car x)) (b 2) c) (list x y a b c)))
 *
 * @param <VAR>
 * 		the type of the initial form
 */
public class Aux<VAR> {

	private final List<AuxVar<VAR>> vars;

	/**
	 * Constructor for a &amp;aux parameter.
	 *
	 * @param vars
	 * 		the vars of the &amp;aux parameter
	 */
	public Aux(final List<AuxVar<VAR>> vars) {
		this.vars = vars;
	}

	public List<AuxVar<VAR>> getVars() {
		return vars;
	}

	@Override
	public String toString() {
		return "Aux{"
				+ "vars=" + vars
				+ '}';
	}

	/**
	 * An {@link Aux} variable type.
	 *
	 * @param <TYPE>
	 * 		the type of the initial form
	 */
	public static class AuxVar<TYPE> {

		private final String var;

		private final TYPE initForm;

		/**
		 * Constructor for an &amp;aux variable.
		 *
		 * @param var
		 * 		the variable name
		 */
		public AuxVar(final String var) {
			this.var = var;
			initForm = null;
		}

		/**
		 * Constructor for an &amp;aux variable.
		 *
		 * @param var
		 * 		the variable name
		 * @param initForm
		 * 		the initial form
		 */
		public AuxVar(final String var, final TYPE initForm) {
			this.var = var;
			this.initForm = initForm;
		}

		public String getVar() {
			return var;
		}

		public TYPE getInitForm() {
			return initForm;
		}

		@Override
		public String toString() {
			return "AuxVar{"
					+ "var='" + var + '\''
					+ ", initForm=" + initForm
					+ '}';
		}
	}
}
