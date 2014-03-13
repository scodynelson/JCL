package jcl.lambdalist.variable;

import java.util.List;

/**
 * &rest, if present, must be followed by a single rest parameter specifier, which in turn must be followed by another
 * lambda list keyword or the end of the lambda list. After all optional parameter specifiers have been processed, then
 * there may or may not be a rest parameter. If there is a rest parameter, it is bound to a list of all as-yet-unprocessed
 * arguments. If no unprocessed arguments remain, the rest parameter is bound to the empty list. If there is no rest
 * parameter and there are no keyword parameters, then an error should be signaled if any unprocessed arguments remain;
 * see Section 3.5 (Error Checking in Function Calls). The value of a rest parameter is permitted, but not required, to
 * share structure with the last argument to apply.
 *
 * @param <TYPE>
 */
public class Rest<TYPE> {

	private final List<TYPE> forms;

	/**
	 * Constructor for a &rest parameter.
	 *
	 * @param forms the forms of the &rest parameter
	 */
	public Rest(final List<TYPE> forms) {
		this.forms = forms;
	}

	public List<TYPE> getForms() {
		return forms;
	}

	@Override
	public String toString() {
		return "Rest{"
				+ "forms=" + forms
				+ '}';
	}
}
