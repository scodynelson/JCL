package jcl.compiler.old.exception;

public class GoException extends TOCException {
	private static final long serialVersionUID = 38689329419301770L;
	//private Symbol tagName;

	/**
	 * Creates a new instance of GoException
	 */
	//public GoException(Symbol tagName) {
	public GoException(Object tagName) {        //me
		this.tag = tagName;
	}
}