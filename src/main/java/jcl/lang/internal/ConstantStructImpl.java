package jcl.lang.internal;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import lombok.Getter;

/**
 * Symbol specialization where the symbol contains its expected type for use in Java code and is a guaranteed constant.
 * <p>
 * TODO: This specialization should not be necessary. However, currently the constants from
 *      {@link jcl.lang.statics.CommonLispSymbols} are reliant on the Java value typing for
 *      easier Java coding. Need a better way to do this.
 *
 * @param <TYPE>
 * 		the symbol value type
 */
public final class ConstantStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	/**
	 * The "typed" value of the constant.
	 */
	@Getter
	private TYPE constantValue;

	/**
	 * Private constructor.
	 *
	 * @param name
	 * 		the name of the symbol.
	 */
	private ConstantStructImpl(final String name) {
		super(name);
	}

	/**
	 * Create and return a new variable symbol.
	 *
	 * @param name
	 * 		the name of the symbol
	 * @param symbolPackage
	 * 		the package to import the symbol into
	 * @param <T>
	 * 		the symbol value type
	 *
	 * @return the new variable symbol
	 */
	public static <T extends LispStruct> ConstantStructImpl<T> valueOf(final String name, final PackageStruct symbolPackage) {
		final ConstantStructImpl<T> struct = new ConstantStructImpl<>(name);
		struct.setConstant();

		symbolPackage.importSymbol(struct);
		struct.setSymbolPackage(symbolPackage);
		return struct;
	}

	/**
	 * Method used to initialize the constant value. This method is used internally to "bypass" the normal "setf"
	 * method.
	 * <p>
	 * TODO: This is a type of "hack" to get around constant symbol initialization. This will not work long-term.
	 *
	 * @param newValue
	 * 		the new symbol value
	 */
	public void initializeConstant(final TYPE newValue) {
		value = newValue;
		constantValue = newValue;
	}
}
