package jcl.types;

import jcl.LispType;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

import java.lang.String;

/**
 * Abstract base class for all {@link LispType} implementations.
 */
public abstract class TypeBaseClass extends SymbolStruct<LispType> implements LispType {

	private static final long serialVersionUID = 3499497199319905092L;

	/**
	 * Protected constructor.
	 *
	 * @param name
	 * 		the name of the symbol type
	 */
	protected TypeBaseClass(final String name) {
		super(name, GlobalPackageStruct.JCL_TYPE);
		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		setValue(this);
	}

	@Override
	public String printStruct() {
		return null;
	}
}
