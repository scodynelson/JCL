package jcl.lang;

import java.util.Optional;

import jcl.lang.internal.SymbolStructImpl;

/**
 * The {@link SymbolStruct} is the object representation of a Lisp 'symbol' type.
 */
public interface SymbolStruct extends LispStruct {

	/**
	 * Returns the {@link String} name of the symbol.
	 *
	 * @return the name of the symbol
	 */
	String getName();

	/**
	 * Returns the {@link PackageStruct} package of the symbol. The return is {@link Optional} since the symbol may or
	 * may not have a package.
	 * <p>
	 * TODO: This method may only be necessary due to the return of {@link NILStruct#INSTANCE} from the {@link
	 *      #symbolPackage()} method.
	 *
	 * @return the package of the symbol
	 */
	Optional<PackageStruct> getSymbolPackage();

	/**
	 * Modifies the current symbol package, setting the value to the provided {@code symbolPackage}.
	 * <p>
	 * TODO: This value is mostly used for internally setting up symbol packages and should not be used externally.
	 *      Need to determine how to do this more cleanly.
	 *
	 * @param symbolPackage
	 * 		the new symbol package value
	 */
	void setSymbolPackage(final PackageStruct symbolPackage);

	/**
	 * Modification method used internally to make the symbol instance a "constant", meaning values are no longer
	 * modifiable.
	 * <p>
	 * TODO: This is currently used internally to control making a symbol a "constant".
	 *      There may be a better way to do this.
	 */
	void setConstant();

	/*
	SYMBOL-STRUCT
	 */

	/**
	 * Returns the {@link StringStruct} name of the symbol.
	 *
	 * @return the name of the symbol
	 */
	StringStruct symbolName();

	/**
	 * Returns the {@link PackageStruct} package of the symbol. If there is no package, {@link NILStruct#INSTANCE} is
	 * returned.
	 *
	 * @return the package of the symbol
	 */
	LispStruct symbolPackage();

	/**
	 * Returns whether or not the symbol has a value.
	 *
	 * @return whether or not the symbol has a value
	 */
	BooleanStruct boundP();

	/**
	 * Removes the symbol value. Returns the current instance.
	 *
	 * @return the current instance
	 */
	SymbolStruct makunbound();

	/**
	 * Returns the value of the symbol. If there is no value, an
	 * {@link jcl.lang.condition.exception.UnboundVariableException} is thrown.
	 *
	 * @return the value of the symbol
	 */
	LispStruct symbolValue();

	/**
	 * Sets the value of the symbol to {@code newValue}.
	 *
	 * @param newValue
	 * 		the new symbol value
	 *
	 * @return the new value
	 */
	LispStruct setfSymbolValue(final LispStruct newValue);

	/**
	 * Returns whether or not the symbol has a function assignment.
	 *
	 * @return whether or not the symbol has a function assignment
	 */
	BooleanStruct fBoundP();

	/**
	 * Removes the symbol function value. Returns the current instance.
	 *
	 * @return the current instance
	 */
	SymbolStruct fMakunbound();

	/**
	 * Returns the value of the symbol function. If there is no value, an
	 * {@link jcl.lang.condition.exception.UndefinedFunctionException} is thrown.
	 *
	 * @return the value of the symbol function
	 */
	FunctionStruct symbolFunction();

	/**
	 * Sets the value of the symbol function to {@code newFunction}.
	 *
	 * @param newFunction
	 * 		the new symbol function value
	 *
	 * @return the new function value
	 */
	FunctionStruct setfSymbolFunction(final FunctionStruct newFunction);

	/**
	 * Returns the symbol property list.
	 *
	 * @return the symbol property list
	 */
	ListStruct symbolPlist();

	/**
	 * Sets the symbol property list.
	 *
	 * @param newPlist
	 * 		the new property list
	 *
	 * @return the new property list
	 */
	ListStruct setfSymbolPlist(final ListStruct newPlist);

	/**
	 * Retrieves the property from the symbol {@link ListStruct} properties associated with the provided
	 * {@code indicator}. If the property is not found, {@code null} is returned.
	 *
	 * @param indicator
	 * 		the key for the property to retrieve
	 * @param defaultValue
	 * 		the default value of the property if property cannot be found
	 *
	 * @return the property from the symbol {@link ListStruct} properties or {@code null} if the property cannot be
	 * found.
	 */
	LispStruct getProp(final LispStruct indicator, final LispStruct defaultValue);

	/**
	 * Sets the property in the symbol {@link ListStruct} properties associated with the provided {@code indicator} to
	 * the provided {@code value}.
	 *
	 * @param indicator
	 * 		the key for the property to set
	 * @param newValue
	 * 		the value of the property
	 *
	 * @return the symbol properties {@link ListStruct}
	 */
	LispStruct setProp(final LispStruct indicator, final LispStruct newValue);

	/**
	 * Removes the first property in the symbol {@link ListStruct} properties associated with the provided
	 * {@code indicator}.
	 *
	 * @param indicator
	 * 		the key for the property to remove
	 *
	 * @return whether or not the property was removed
	 */
	BooleanStruct remProp(final LispStruct indicator);

	/**
	 * Copies the symbol and possibly its {@link ListStruct} properties.
	 *
	 * @param copyProperties
	 * 		whether or not to copy the symbol's {@link ListStruct} properties
	 *
	 * @return the newly copied symbol
	 */
	SymbolStruct copySymbol(final BooleanStruct copyProperties);

	/**
	 * Returns a new SymbolStruct with the provided {@link StringStruct} name.
	 *
	 * @param name
	 * 		the name of the new symbol
	 *
	 * @return a new SymbolStruct
	 */
	static SymbolStruct toLispSymbol(final StringStruct name) {
		return new SymbolStructImpl(name.toJavaString());
	}

	/**
	 * Returns a new SymbolStruct with the provided {@link String} name.
	 *
	 * @param name
	 * 		the name of the new symbol
	 *
	 * @return a new SymbolStruct
	 */
	static SymbolStruct toLispSymbol(final String name) {
		return new SymbolStructImpl(name);
	}
}
