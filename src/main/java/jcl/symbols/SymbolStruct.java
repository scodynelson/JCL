package jcl.symbols;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.compiler.real.sa.analyzer.expander.CompilerMacroFunctionExpander;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.sa.analyzer.expander.SymbolMacroExpander;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.types.NIL;
import jcl.types.Symbol;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link SymbolStruct} is the object representation of a Lisp 'symbol' type.
 *
 * @param <TYPE>
 * 		the type of the symbol value
 */
public class SymbolStruct<TYPE extends LispStruct> extends BuiltInClassStruct {

	private static final long serialVersionUID = -986185868644037105L;

	protected final String name;

	protected PackageStruct symbolPackage;
	protected TYPE value;
	protected FunctionStruct function;

	protected final List<LispStruct> properties = new ArrayList<>();

	protected MacroFunctionExpander<?> macroFunctionExpander;
	protected CompilerMacroFunctionExpander<?> compilerMacroFunctionExpander;
	protected SymbolMacroExpander<?> symbolMacroExpander;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 */
	public SymbolStruct(final String name) {
		this(name, null, null, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 * @param symbolPackage
	 * 		the symbol package
	 */
	public SymbolStruct(final String name, final PackageStruct symbolPackage) {
		this(name, symbolPackage, null, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 * @param value
	 * 		the symbol value
	 */
	public SymbolStruct(final String name, final TYPE value) {
		this(name, null, value, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 * @param function
	 * 		the symbol function
	 */
	public SymbolStruct(final String name, final FunctionStruct function) {
		this(name, null, null, function);
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 * @param symbolPackage
	 * 		the symbol package
	 * @param value
	 * 		the symbol value
	 */
	public SymbolStruct(final String name, final PackageStruct symbolPackage, final TYPE value) {
		this(Symbol.INSTANCE, name, symbolPackage, value, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 * @param symbolPackage
	 * 		the symbol package
	 * @param value
	 * 		the symbol value
	 * @param function
	 * 		the symbol function
	 */
	public SymbolStruct(final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		this(Symbol.INSTANCE, name, symbolPackage, value, function);
	}

	/**
	 * Protected constructor.
	 *
	 * @param symbolType
	 * 		the symbol type
	 * @param name
	 * 		the symbol name
	 * @param symbolPackage
	 * 		the symbol package
	 * @param value
	 * 		the symbol value
	 * @param function
	 * 		the symbol function
	 */
	protected SymbolStruct(final Symbol symbolType,
	                       final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		super(symbolType, null, null);
		this.name = name;

		this.symbolPackage = symbolPackage;
		this.value = value;
		this.function = function;

		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		if (symbolPackage != null) {
			symbolPackage.importSymbols(this);
			// TODO: we REALLY shouldn't be exporting here, BUT so we can test things right now, we will.
			symbolPackage.export(this);
		}
	}

	/**
	 * Getter for symbol {@link #name} property.
	 *
	 * @return symbol {@link #name} property
	 */
	public String getName() {
		return name;
	}

	/**
	 * Getter for symbol {@link #symbolPackage} property.
	 *
	 * @return symbol {@link #symbolPackage} property
	 */
	public PackageStruct getSymbolPackage() {
		return symbolPackage;
	}

	/**
	 * Setter for symbol {@link #symbolPackage} property.
	 *
	 * @param symbolPackage
	 * 		new symbol {@link #symbolPackage} property value
	 */
	public void setSymbolPackage(final PackageStruct symbolPackage) {
		this.symbolPackage = symbolPackage;
	}

	/**
	 * Getter for symbol {@link #value} property.
	 *
	 * @return symbol {@link #value} property
	 */
	public TYPE getValue() {
		if (value == null) {
			String variableName = name;
			final PackageStruct currentPackage = PackageVariables.PACKAGE.getValue();

			if (!currentPackage.equals(symbolPackage)) {
				final String packageName = symbolPackage.getName();

				if (currentPackage.getExternalSymbols().containsKey(name)) {
					variableName = packageName + ':' + name;
				} else {
					variableName = packageName + "::" + name;
				}
			}

			throw new ErrorException("Unbound variable: " + variableName);
		}
		return value;
	}

	/**
	 * Setter for symbol {@link #value} property.
	 *
	 * @param value
	 * 		new symbol {@link #value} property value
	 */
	public void setValue(final TYPE value) {
		this.value = value;
	}

	/**
	 * Getter for symbol {@link #function} property.
	 *
	 * @return symbol {@link #function} property
	 */
	public FunctionStruct getFunction() {
		return function;
	}

	/**
	 * Setter for symbol {@link #function} property.
	 *
	 * @param function
	 * 		new symbol {@link #function} property value
	 */
	public void setFunction(final FunctionStruct function) {
		this.function = function;
	}

	/**
	 * Getter for symbol {@link #properties} property.
	 *
	 * @return symbol {@link #properties} property
	 */
	public List<LispStruct> getProperties() {
		return properties;
	}

	/**
	 * Getter for symbol {@link #macroFunctionExpander} property.
	 *
	 * @return symbol {@link #macroFunctionExpander} property
	 */
	public MacroFunctionExpander<?> getMacroFunctionExpander() {
		return macroFunctionExpander;
	}

	/**
	 * Setter for symbol {@link #macroFunctionExpander} property.
	 *
	 * @param macroFunctionExpander
	 * 		new symbol {@link #macroFunctionExpander} property value
	 */
	public void setMacroFunctionExpander(final MacroFunctionExpander<?> macroFunctionExpander) {
		this.macroFunctionExpander = macroFunctionExpander;
	}

	/**
	 * Getter for symbol {@link #compilerMacroFunctionExpander} property.
	 *
	 * @return symbol {@link #compilerMacroFunctionExpander} property
	 */
	public CompilerMacroFunctionExpander<?> getCompilerMacroFunctionExpander() {
		return compilerMacroFunctionExpander;
	}

	/**
	 * Setter for symbol {@link #compilerMacroFunctionExpander} property.
	 *
	 * @param compilerMacroFunctionExpander
	 * 		new symbol {@link #compilerMacroFunctionExpander} property value
	 */
	public void setCompilerMacroFunctionExpander(final CompilerMacroFunctionExpander<?> compilerMacroFunctionExpander) {
		this.compilerMacroFunctionExpander = compilerMacroFunctionExpander;
	}

	/**
	 * Getter for symbol {@link #symbolMacroExpander} property.
	 *
	 * @return symbol {@link #symbolMacroExpander} property
	 */
	public SymbolMacroExpander<?> getSymbolMacroExpander() {
		return symbolMacroExpander;
	}

	/**
	 * Setter for symbol {@link #symbolMacroExpander} property.
	 *
	 * @param symbolMacroExpander
	 * 		new symbol {@link #symbolMacroExpander} property value
	 */
	public void setSymbolMacroExpander(final SymbolMacroExpander<?> symbolMacroExpander) {
		this.symbolMacroExpander = symbolMacroExpander;
	}

	/**
	 * Retrieves the property from the symbol {@link #properties} associated with the provided {@code key}. If the
	 * property is not found, {@link NIL#INSTANCE} is returned.
	 *
	 * @param key
	 * 		the key for the property to retrieve
	 *
	 * @return the property from the symbol {@link #properties} or {@link NIL#INSTANCE} if the property cannot be found.
	 */
	public LispStruct getProperty(final LispStruct key) {
		for (int i = 0; i < properties.size(); i += 2) {
			final LispStruct current = properties.get(i);
			if (key.equals(current)) {
				final int valueIndex = i + 1;
				return properties.get(valueIndex);
			}
		}
		return NIL.INSTANCE;
	}

	/**
	 * Sets the property in the symbol {@link #properties} associated with the provided {@code key} to the provided
	 * {@code value}.
	 *
	 * @param key
	 * 		the key for the property to set
	 * @param value
	 * 		the value of the property
	 */
	public void setProperty(final LispStruct key, final LispStruct value) {
		for (int i = 0; i < properties.size(); i += 2) {
			final LispStruct current = properties.get(i);
			if (key.equals(current)) {
				final int valueIndex = i + 1;
				properties.remove(valueIndex);
				properties.add(valueIndex, value);
			}
		}
	}

	/**
	 * Copies the symbol and possibly its {@link #properties}.
	 *
	 * @param copyProperties
	 * 		whether or not to copy the symbol's {@link #properties}
	 *
	 * @return the newly copied symbol
	 */
	public SymbolStruct<TYPE> copySymbol(final boolean copyProperties) {
		if (copyProperties) {
			final SymbolStruct<TYPE> newSymbol = new SymbolStruct<>(name, symbolPackage, value, function);
			newSymbol.properties.addAll(properties);
			return newSymbol;
		} else {
			return new SymbolStruct<>(name);
		}
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
