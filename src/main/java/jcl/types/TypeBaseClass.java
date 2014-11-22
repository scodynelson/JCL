package jcl.types;

import jcl.LispType;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.packages.PackageStruct;
import jcl.structs.symbols.SymbolStruct;

import java.lang.String;

/**
 * Abstract base class for all {@link LispType} implementations.
 */
public abstract class TypeBaseClass extends SymbolStruct<LispType> implements LispType {

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
