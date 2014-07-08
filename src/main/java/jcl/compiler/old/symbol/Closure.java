package jcl.compiler.old.symbol;

/**
 * This interface defines the Closure type. This is an extension to the Common Lisp
 * type system. The compiler generates instances of type Closure when there are free
 * variables used in lambda and let forms. A closure is an independent object when there
 * are lambda expresssions that share the closure. When the closure is not shared, it is
 * coalesced into the appropriate function class.
 * <p>
 * The occurence of a lambda expression with free variables automatically generates a
 * closure. This is not optimal with respect to space and time efficiency. That will have to
 * await the next big compiler.
 *
 * @author jboetje
 */
public interface Closure {

	/**
	 * This method returns an outer (or parent) closure object. If there is none, the method
	 * returns NIL.
	 *
	 * @return an outer closure object or NIL if there is none
	 */
	Closure getParent();

	/**
	 * This method returns the bound value of a lexical variable in the context of a binding
	 * environment. The binding values in a closure are indexed in the lexical order of the bound variables
	 * and by the level of the parent chain where the binding exists.<p>
	 * Examples
	 * <ul>
	 * <li>If the reference is to the 2nd bound variable in the current Closure,
	 * the the call will be <code>getBindingsAt(1, 0)</code.</li>
	 * <li>If the reference is the the first bound variable in the parent of the parent of the
	 * current Closure, the call witll be {@code getBindingsAt(0, 2)}</li>
	 * </ul>
	 */
	Object getBindingAt(int index, int nestingLevel);

	/**
	 * This method sets the bound value of a lexical variable in the context of a binding
	 * environment. The binding values in a closure are indexed in the lexical order of the bound variables.
	 * This method does not chain. It is commonly used with the current closure.<p>
	 */
	void setBindingAt(int index, int nestingLevel, Object value);
}
