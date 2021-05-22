package jcl.lang;

import java.util.Optional;

import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.internal.SymbolStructImpl;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link SymbolStruct} is the object representation of a Lisp 'symbol' type.
 */
public interface SymbolStruct extends LispStruct {

	String getName();

	Optional<PackageStruct> getSymbolPackage();

	void setSymbolPackage(final PackageStruct symbolPackage);

	void setConstant();

	StringStruct symbolName();

	LispStruct symbolPackage();

	BooleanStruct boundP();

	SymbolStruct makunbound();

	LispStruct symbolValue();

	LispStruct setSymbolValue(final LispStruct value);

	BooleanStruct fBoundP();

	SymbolStruct fMakunbound();

	FunctionStruct symbolFunction();

	FunctionStruct setSymbolFunction(final FunctionStruct function);

	/**
	 * Getter for symbol {@link ListStruct} properties.
	 *
	 * @return symbol {@link ListStruct} properties
	 */
	ListStruct symbolPlist();

	ListStruct setSymbolPlist(final ListStruct properties);

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
	 * Removes the first property in the symbol {@link ListStruct} properties associated with the provided {@code
	 * indicator}.
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

	static SymbolStruct toLispSymbol(final StringStruct struct) {
		return new SymbolStructImpl(struct.toJavaString());
	}

	static SymbolStruct toLispSymbol(final String name) {
		return new SymbolStructImpl(name);
	}

	/*
	TODO: things below...
	 */

	static MacroFunctionExpanderInter getMacroFunctionDefinition(final SymbolStruct symbol) {
		final LispStruct definition = symbol.getProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, NILStruct.INSTANCE);
		if (NILStruct.INSTANCE.eq(definition)) {
			return null; // TODO: return null??
		}
		if (!(definition instanceof MacroFunctionExpanderInter)) {
			throw new IllegalStateException("Invalid macro-function definition: " + definition);
		}
		return (MacroFunctionExpanderInter) definition;
	}

	static void setMacroFunctionDefinition(final SymbolStruct symbol, final MacroFunctionExpanderInter macro) {
		symbol.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, macro);
	}

	static SymbolMacroExpanderInter getSymbolMacroDefinition(final SymbolStruct symbol) {
		final LispStruct definition = symbol.getProp(CommonLispSymbols.SYMBOL_MACRO_DEFINITION, NILStruct.INSTANCE);
		if (NILStruct.INSTANCE.eq(definition)) {
			return null; // TODO: return null??
		}
		if (!(definition instanceof SymbolMacroExpanderInter)) {
			throw new IllegalStateException("Invalid symbol-macro definition: " + definition);
		}
		return (SymbolMacroExpanderInter) definition;
	}

	static void setSymbolMacroDefinition(final SymbolStruct symbol, final SymbolMacroExpanderInter symbolMacro) {
		symbol.setProp(CommonLispSymbols.SYMBOL_MACRO_DEFINITION, symbolMacro);
	}

	static FunctionStruct getSetfDefinition(final SymbolStruct symbol) {
		final LispStruct definition = symbol.getProp(CommonLispSymbols.SETF_DEFINITION, NILStruct.INSTANCE);
		if (NILStruct.INSTANCE.eq(definition)) {
			return null; // TODO: return null??
		}
		if (!(definition instanceof FunctionStruct)) {
			throw new IllegalStateException("Invalid setf definition: " + definition);
		}
		return (FunctionStruct) definition;
	}

	static void setSetfDefinition(final SymbolStruct symbol, final FunctionStruct function) {
		symbol.setProp(CommonLispSymbols.SETF_DEFINITION, function);
	}
}
