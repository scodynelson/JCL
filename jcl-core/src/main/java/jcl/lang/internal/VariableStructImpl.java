package jcl.lang.internal;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import lombok.Getter;

/**
 * Symbol specialization where the symbol contains its expected type for use in Java code.
 * <p>
 * TODO: This specialization should not be necessary. However, currently the constants from
 *      {@link jcl.lang.statics.CommonLispSymbols} are reliant on the Java value typing for
 *      easier Java coding. Need a better way to do this.
 *
 * @param <TYPE>
 * 		the symbol value type
 */
public final class VariableStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	/**
	 * The "typed" value of the variable.
	 */
	@Getter
	private TYPE variableValue;

	/**
	 * Private constructor.
	 *
	 * @param name
	 * 		the name of the symbol.
	 */
	private VariableStructImpl(final String name) {
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
	public static <T extends LispStruct> VariableStructImpl<T> valueOf(final String name, final PackageStruct symbolPackage) {
		final VariableStructImpl<T> struct = new VariableStructImpl<>(name);
		symbolPackage.importSymbol(struct);
		struct.setSymbolPackage(symbolPackage);
		return struct;
	}

	@Override
	@SuppressWarnings("unchecked")
	public LispStruct setfSymbolValue(final LispStruct newValue) {
		// TODO: This isn't really safe...
		variableValue = (TYPE) newValue;
		return super.setfSymbolValue(newValue);
	}
}
