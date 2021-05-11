package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.internal.SymbolStructImpl;

/**
 * The {@link SymbolStruct} is the object representation of a Lisp 'symbol' type.
 */
public interface SymbolStruct extends LispStruct {

	SymbolStruct MACRO_FUNCTION_DEFINITION = toLispSymbol("MACRO-FUNCTION-DEFINITION");
	SymbolStruct SYMBOL_MACRO_DEFINITION = toLispSymbol("SYMBOL-MACRO-DEFINITION");
	SymbolStruct SETF_DEFINITION = toLispSymbol("SETF-DEFINITION");

	String getName();

	default StringStruct getLispName() {
		return StringStruct.toLispString(getName());
	}

	default LispStruct getPackage() {
		final PackageStruct symbolPackage = getSymbolPackage();
		return (symbolPackage == null) ? NILStruct.INSTANCE : symbolPackage;
	}

	PackageStruct getSymbolPackage();

	void setSymbolPackage(final PackageStruct symbolPackage);

	BooleanStruct hasValue();

	LispStruct getValue();

	void setValue(final LispStruct value);

	default LispStruct setValue1(final LispStruct value) {
		setValue(value);
		return value;
	}

	boolean hasFunction();

	FunctionStruct getFunction();

	void setFunction(final FunctionStruct function);

	/**
	 * Getter for symbol {@link ListStruct} properties.
	 *
	 * @return symbol {@link ListStruct} properties
	 */
	ListStruct getProperties();

	void setProperties(final ListStruct properties);

	/**
	 * Retrieves the property from the symbol {@link ListStruct} properties associated with the provided {@code
	 * indicator}. If the property is not found, {@code null} is returned.
	 *
	 * @param indicator
	 * 		the key for the property to retrieve
	 * @param defaultValue
	 * 		the default value of the property if property cannot be found
	 *
	 * @return the property from the symbol {@link ListStruct} properties or {@code null} if the property cannot be
	 * found.
	 */
	LispStruct getProperty(final LispStruct indicator, final LispStruct defaultValue);

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
	ListStruct setProperty(final LispStruct indicator, final LispStruct newValue);

	/**
	 * Removes the first property in the symbol {@link ListStruct} properties associated with the provided {@code
	 * indicator}.
	 *
	 * @param indicator
	 * 		the key for the property to remove
	 *
	 * @return whether or not the property was removed
	 */
	BooleanStruct removeProperty(final LispStruct indicator);

	/**
	 * Copies the symbol and possibly its {@link ListStruct} properties.
	 *
	 * @param copyProperties
	 * 		whether or not to copy the symbol's {@link ListStruct} properties
	 *
	 * @return the newly copied symbol
	 */
	SymbolStruct copySymbol(final BooleanStruct copyProperties);

	static SymbolStruct toLispSymbol(final StringStruct struct) {
		return new SymbolStructImpl(struct.toJavaString());
	}

	static SymbolStruct toLispSymbol(final String name) {
		return new SymbolStructImpl(name);
	}

	static SymbolStruct toLispSymbol(final String name, final PackageStruct pkg) {
		final SymbolStructImpl struct = new SymbolStructImpl(name);
		struct.setSymbolPackage(pkg);
		return struct;
	}

	static SymbolStruct fromDesignator(final LispStruct struct) {
		if (struct instanceof SymbolStruct) {
			return (SymbolStruct) struct;
		} else {
			throw new TypeErrorException("Type cannot be converted to Symbol: " + struct);
		}
	}

	static MacroFunctionExpanderInter getMacroFunctionDefinition(final SymbolStruct symbol) {
		final LispStruct definition = symbol.getProperty(MACRO_FUNCTION_DEFINITION, NILStruct.INSTANCE);
		if (NILStruct.INSTANCE.eq(definition)) {
			return null; // TODO: return null??
		}
		if (!(definition instanceof MacroFunctionExpanderInter)) {
			throw new IllegalStateException("Invalid macro-function definition: " + definition);
		}
		return (MacroFunctionExpanderInter) definition;
	}

	static void setMacroFunctionDefinition(final SymbolStruct symbol, final MacroFunctionExpanderInter macro) {
		symbol.setProperty(MACRO_FUNCTION_DEFINITION, macro);
	}

	static SymbolMacroExpanderInter getSymbolMacroDefinition(final SymbolStruct symbol) {
		final LispStruct definition = symbol.getProperty(SYMBOL_MACRO_DEFINITION, NILStruct.INSTANCE);
		if (NILStruct.INSTANCE.eq(definition)) {
			return null; // TODO: return null??
		}
		if (!(definition instanceof SymbolMacroExpanderInter)) {
			throw new IllegalStateException("Invalid symbol-macro definition: " + definition);
		}
		return (SymbolMacroExpanderInter) definition;
	}

	static void setSymbolMacroDefinition(final SymbolStruct symbol, final SymbolMacroExpanderInter symbolMacro) {
		symbol.setProperty(SYMBOL_MACRO_DEFINITION, symbolMacro);
	}

	static FunctionStruct getSetfDefinition(final SymbolStruct symbol) {
		final LispStruct definition = symbol.getProperty(SETF_DEFINITION, NILStruct.INSTANCE);
		if (NILStruct.INSTANCE.eq(definition)) {
			return null; // TODO: return null??
		}
		if (!(definition instanceof FunctionStruct)) {
			throw new IllegalStateException("Invalid setf definition: " + definition);
		}
		return (FunctionStruct) definition;
	}

	static void setSetfDefinition(final SymbolStruct symbol, final FunctionStruct function) {
		symbol.setProperty(SETF_DEFINITION, function);
	}

	default SymbolStruct makunbound() {
		setValue(null);
		return this;
	}
}
