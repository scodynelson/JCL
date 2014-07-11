package jcl.compiler.old.exception;

public class ThrowException extends TRFException {

    /*
	static Stack<Object> catchTags = new Stack<Object>();
    
    public static void addTag(Object toAdd) {
    ThrowException.catchTags.add(toAdd);
    }
    
    public static Object removeTag() {
    return ThrowException.catchTags.pop();
    }
     */

	/**
	 * Creates a new instance of ThrowException
	 */
	public ThrowException(Object catchTag, Object value) {
		this.tag = catchTag;
		this.value = value;
	}

	/** This checks to see if the catch tag values match
	 * the current object's value with the object passed as a parameter. */
    /*
    public boolean equals(Object obj) {
    return (obj instanceof ThrowException &&
    (((ThrowException)obj).getCatchTag() == catchTag));
    }
     */

	/**
	 * This returns the current catch tag name
	 */
	public Object getCatchTag() {
		return getTag();
	}
	/** This returns the result-form that is returned with the transfer of control */
    /*
    public Object getValue() {
    return value;
    }
     */
	/** This determines if the current exception object is supposed to handle the current catch tag name;
	 * otherwise, it throws the exception back up the runtime stack for the
	 * next exception handler to catch and process */
    /*
    public Object process(Object name) {
    if (name != this.catchTag) {
    if (ThrowException.catchTags.isEmpty())
    System.out.println("\nERROR: A catch tag was thrown that did not match a valid catch tag in the current function.\n");
    throw this;
    }
    return value;
    } 
     */
}
