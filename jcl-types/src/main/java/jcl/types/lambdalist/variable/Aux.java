package jcl.types.lambdalist.variable;

import java.util.List;

/**
 * These are not really parameters. If the lambda list keyword &aux is present, all specifiers after it are auxiliary
 * variable specifiers. After all parameter specifiers have been processed, the auxiliary variable specifiers (those
 * following &aux) are processed from left to right. For each one, init-form is evaluated and var is bound to that value
 * (or to nil if no init-form was specified). &aux variable processing is analogous to let* processing.
 * <p/>
 * (lambda (x y &aux (a (car x)) (b 2) c) (list x y a b c)) ==  (lambda (x y) (let* ((a (car x)) (b 2) c) (list x y a b c)))
 *
 * @param <VAR>
 */
public class Aux<VAR> {

	private final List<AuxVar<VAR>> vars;

	public Aux(final List<AuxVar<VAR>> vars) {
		this.vars = vars;
	}

	public List<AuxVar<VAR>> getVars() {
		return vars;
	}

	@Override
	public String toString() {
		return "Aux{" +
				"vars=" + vars +
				'}';
	}

	public static class AuxVar<TYPE> {

		private final String var;
		private final TYPE initForm;

		public AuxVar(final String var) {
			this.var = var;
			initForm = null;
		}

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
			return "AuxVar{" +
					"var='" + var + '\'' +
					", initForm=" + initForm +
					'}';
		}
	}
}
