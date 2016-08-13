package jcl.lang;

import jcl.lang.function.FunctionStructImpl;
import jcl.lang.function.expander.CompilerMacroFunctionExpanderInter;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;

/**
 * The {@link SymbolStruct} is the object representation of a Lisp 'symbol' type.
 */
public interface SymbolStruct extends LispStruct {

	/**
	 * Getter for symbol name value.
	 *
	 * @return symbol name value
	 */
	String getName();

	/**
	 * Getter for symbol {@link PackageStruct} property.
	 *
	 * @return symbol {@link PackageStruct} property
	 */
	PackageStruct getSymbolPackage();

	/**
	 * Setter for symbol {@link PackageStruct} property.
	 *
	 * @param symbolPackage
	 * 		new symbol {@link PackageStruct} property value
	 */
	void setSymbolPackage(final PackageStruct symbolPackage);

	boolean hasValue();

	//	/**
//	 * Getter for symbol {@link #value} property.
//	 *
//	 * @return symbol {@link #value} property
//	 */
	LispStruct getValue();

	LispStruct getLexicalValue();

	LispStruct getDynamicValue();

	//	/**
//	 * Setter for symbol {@link #value} property.
//	 *
//	 * @param value
//	 * 		new symbol {@link #value} property value
//	 */
	void setValue(final LispStruct value);

	void setLexicalValue(final LispStruct value);

	void setDynamicValue(final LispStruct value);

	void bindLexicalValue(final LispStruct value);

	void unbindLexicalValue();

	void bindDynamicValue(final LispStruct value);

	void unbindDynamicValue();

	boolean hasFunction();

	//	/**
//	 * Getter for symbol {@link #function} property.
//	 *
//	 * @return symbol {@link #function} property
//	 */
	FunctionStructImpl getFunction();

	//	/**
//	 * Setter for symbol {@link #function} property.
//	 *
//	 * @param function
//	 * 		new symbol {@link #function} property value
//	 */
	void setFunction(final FunctionStructImpl function);

	void bindFunction(final FunctionStructImpl function);

	FunctionStructImpl unbindFunction();

	/**
	 * Getter for symbol {@link MacroFunctionExpanderInter} property.
	 *
	 * @return symbol {@link MacroFunctionExpanderInter} property
	 */
	MacroFunctionExpanderInter getMacroFunctionExpander();

	/**
	 * Setter for symbol {@link MacroFunctionExpanderInter} property.
	 *
	 * @param macroFunctionExpander
	 * 		new symbol {@link MacroFunctionExpanderInter} property value
	 */
	void setMacroFunctionExpander(final MacroFunctionExpanderInter macroFunctionExpander);

	/**
	 * Getter for symbol {@link CompilerMacroFunctionExpanderInter} property.
	 *
	 * @return symbol {@link CompilerMacroFunctionExpanderInter} property
	 */
	CompilerMacroFunctionExpanderInter getCompilerMacroFunctionExpander();

	/**
	 * Setter for symbol {@link CompilerMacroFunctionExpanderInter} property.
	 *
	 * @param compilerMacroFunctionExpander
	 * 		new symbol {@link CompilerMacroFunctionExpanderInter} property value
	 */
	void setCompilerMacroFunctionExpander(final CompilerMacroFunctionExpanderInter compilerMacroFunctionExpander);

	//	/**
//	 * Getter for symbol {@link #symbolMacroExpander} property.
//	 *
//	 * @return symbol {@link #symbolMacroExpander} property
//	 */
	SymbolMacroExpanderInter getSymbolMacroExpander();

	//	/**
//	 * Setter for symbol {@link #symbolMacroExpander} property.
//	 *
//	 * @param symbolMacroExpander
//	 * 		new symbol {@link #symbolMacroExpander} property value
//	 */
	void setSymbolMacroExpander(final SymbolMacroExpanderInter symbolMacroExpander);

	void bindSymbolMacroExpander(final SymbolMacroExpanderInter symbolMacroExpander);

	void unbindSymbolMacroExpander();

	/**
	 * Getter for symbol {@link ListStruct} properties.
	 *
	 * @return symbol {@link ListStruct} properties
	 */
	ListStruct getProperties();

	void setProperties(final ListStruct properties);

	/**
	 * Getter for symbol {@link StructureClassStruct} property.
	 *
	 * @return symbol {@link StructureClassStruct} property
	 */
	StructureClassStruct getStructureClass();

	/**
	 * Setter for symbol {@link StructureClassStruct} property.
	 *
	 * @param structureClass
	 * 		new symbol {@link StructureClassStruct} property value
	 */
	void setStructureClass(final StructureClassStruct structureClass);

	/**
	 * Retrieves the property from the symbol {@link ListStruct} properties associated with the provided {@code
	 * indicator}. If the
	 * property is not found, {@code null} is returned.
	 *
	 * @param indicator
	 * 		the key for the property to retrieve
	 *
	 * @return the property from the symbol {@link ListStruct} properties or {@code null} if the property cannot be
	 * found.
	 */
	LispStruct getProperty(final LispStruct indicator, final LispStruct defaultValue);

	/**
	 * Sets the property in the symbol {@link ListStruct} properties associated with the provided {@code indicator} to
	 * the provided
	 * {@code value}.
	 *
	 * @param indicator
	 * 		the key for the property to set
	 * @param newValue
	 * 		the value of the property
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
	boolean removeProperty(final LispStruct indicator);

	/**
	 * Copies the symbol and possibly its {@link ListStruct} properties.
	 *
	 * @param copyProperties
	 * 		whether or not to copy the symbol's {@link ListStruct} properties
	 *
	 * @return the newly copied symbol
	 */
	SymbolStruct copySymbol(final boolean copyProperties);
}
