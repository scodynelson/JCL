package jcl.compiler.old.exception;

import jcl.structs.symbols.SymbolStruct;

public class ReturnFromException extends TRFException {
	private static final long serialVersionUID = -4154198139779543951L;
	//Symbol blockName;
	//Object value;

	/**
	 * Creates a new instance of ReturnFromException
	 */
	public ReturnFromException(SymbolStruct<?> blockName, Object value) {
		this.tag = blockName;
		this.value = value;
	}

	/*
	public boolean equals(Object obj) {
	return (obj instanceof ReturnFromException &&
	(((ReturnFromException)obj).getBlockName() == blockName));
	}
	 */
	public Object getBlockName() {
		return getTag();
	}
	/*
    public Object getValue() {
    return value;
    }
     */
    /*
    public Object process(Symbol name) {
    if (name != blockName) {
    throw this;
    }
    return value;
    }
     */
}
