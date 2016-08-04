package jcl.lang;

import java.util.Stack;
import java.util.function.Supplier;

import com.ibm.icu.lang.UCharacter;
import jcl.lang.internal.StringStructImpl;
import jcl.lang.internal.CharacterStructImpl;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.expander.CompilerMacroFunctionExpanderInter;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.statics.PackageVariables;
import jcl.type.LispType;
import jcl.type.SymbolType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The {@link SymbolStructImpl} is the object representation of a Lisp 'symbol' type.
 */
public class SymbolStructImpl extends BuiltInClassStruct {

	protected final String name;

	protected ListStruct properties;

	protected PackageStruct symbolPackage;

	protected Stack<LispStruct> lexicalValueStack = new Stack<>();

	protected Stack<LispStruct> dynamicValueStack = new Stack<>();

	protected Stack<FunctionStruct> functionStack = new Stack<>();

	protected MacroFunctionExpanderInter macroFunctionExpander;

	protected CompilerMacroFunctionExpanderInter compilerMacroFunctionExpander;

	protected Stack<SymbolMacroExpanderInter> symbolMacroExpanderStack = new Stack<>();

	protected StructureClassStruct structureClass;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 */
	private SymbolStructImpl(final String name) {
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
	protected SymbolStructImpl(final String name, final PackageStruct symbolPackage) {
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
	protected SymbolStructImpl(final String name, final LispStruct value) {
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
	protected SymbolStructImpl(final String name, final FunctionStruct function) {
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
	protected SymbolStructImpl(final String name, final PackageStruct symbolPackage, final LispStruct value) {
		this(SymbolType.INSTANCE, name, symbolPackage, value, null);
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
	protected SymbolStructImpl(final String name, final PackageStruct symbolPackage, final LispStruct value, final FunctionStruct function) {
		this(SymbolType.INSTANCE, name, symbolPackage, value, function);
	}

	/**
	 * Protected constructor.
	 *
	 * @param lispType
	 * 		the type of the symbol object
	 * @param name
	 * 		the symbol name
	 * @param symbolPackage
	 * 		the symbol package
	 * @param value
	 * 		the symbol value
	 * @param function
	 * 		the symbol function
	 */
	protected SymbolStructImpl(final LispType lispType,
	                           final String name, final PackageStruct symbolPackage, final LispStruct value, final FunctionStruct function) {
		super(lispType, null, null);
		this.name = name;

		this.symbolPackage = symbolPackage;
		if (value != null) {
			lexicalValueStack.push(value);
		}
		if (function != null) {
			functionStack.push(function);
		}

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

	public static SymbolStructImpl valueOf(final String name) {
		return new SymbolStructImpl(name);
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

	@Override
	public Supplier<CharacterStruct> asCharacter() {
		return () -> {
			if (name.length() != 1) {
				throw new SimpleErrorException("Symbol name is not of length one: " + name);
			}
			return CharacterStructImpl.valueOf(name.charAt(0));
		};
	}

	@Override
	public Supplier<CharacterStruct> asNamedCharacter() {
		return () -> CharacterStructImpl.valueOf(UCharacter.getCharFromName(name));
	}

	/**
	 * {@inheritDoc}
	 * Returns the PackageStruct with the {@link PackageStruct#name} that matches the {@link #name} value on the
	 * instance via {@link PackageStruct#findPackage(String)}.
	 *
	 * @return the PackageStruct with the {@link PackageStruct#name} that matches the {@link #name} value on the
	 * instance
	 */
	@Override
	public Supplier<PackageStruct> asPackage() {
		return () -> PackageStruct.findPackage(name);
	}

	@Override
	public Supplier<StringStruct> asString() {
		return () -> StringStructImpl.valueOf(name);
	}

	public boolean hasValue() {
		return !lexicalValueStack.isEmpty() || !dynamicValueStack.isEmpty();
	}

	//	/**
//	 * Getter for symbol {@link #value} property.
//	 *
//	 * @return symbol {@link #value} property
//	 */
	public LispStruct getValue() {
		final LispStruct value;
		if (lexicalValueStack.isEmpty()) {
			value = getDynamicValue();
		} else {
			value = getLexicalValue();
		}

		if (value == null) {
			return handleUnboundValue();
		}

		return value;
	}

	public LispStruct getLexicalValue() {
		if (lexicalValueStack.isEmpty()) {
			return handleUnboundValue();
		}

		final LispStruct value = lexicalValueStack.peek();
		if (value == null) {
			return handleUnboundValue();
		}

		return value;
	}

	public LispStruct getDynamicValue() {
		if (dynamicValueStack.isEmpty()) {
			handleUnboundValue();
		}

		final LispStruct value = dynamicValueStack.peek();
		if (value == null) {
			return handleUnboundValue();
		}

		return value;
	}

	private LispStruct handleUnboundValue() {
		String variableName = name;
		final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();

		if (!currentPackage.equals(symbolPackage)) {
			if (symbolPackage == null) {
				variableName = "#:" + name;
			} else {
				final String packageName = symbolPackage.getName();
				if (currentPackage.getExternalSymbols().containsKey(name)) {
					variableName = packageName + ':' + name;
				} else {
					variableName = packageName + "::" + name;
				}
			}
		}

		throw new ErrorException("Unbound variable: " + variableName);
	}

	//	/**
//	 * Setter for symbol {@link #value} property.
//	 *
//	 * @param value
//	 * 		new symbol {@link #value} property value
//	 */
	public void setValue(final LispStruct value) {
		if (lexicalValueStack.isEmpty()) {
			if (dynamicValueStack.isEmpty()) {
				dynamicValueStack.push(value);
			} else {
				dynamicValueStack.pop();
				dynamicValueStack.push(value);
			}
		} else {
			lexicalValueStack.pop();
			lexicalValueStack.push(value);
		}
	}

	public void setLexicalValue(final LispStruct value) {
		if (lexicalValueStack.isEmpty()) {
			lexicalValueStack.push(value);
		} else {
			lexicalValueStack.pop();
			lexicalValueStack.push(value);
		}
	}

	public void setDynamicValue(final LispStruct value) {
		if (dynamicValueStack.isEmpty()) {
			dynamicValueStack.push(value);
		} else {
			dynamicValueStack.pop();
			dynamicValueStack.push(value);
		}
	}

	public void bindLexicalValue(final LispStruct value) {
		lexicalValueStack.push(value);
	}

	public void unbindLexicalValue() {
		lexicalValueStack.pop();
	}

	public void bindDynamicValue(final LispStruct value) {
		dynamicValueStack.push(value);
	}

	public void unbindDynamicValue() {
		dynamicValueStack.pop();
	}

	public boolean hasFunction() {
		return !functionStack.isEmpty();
	}

	//	/**
//	 * Getter for symbol {@link #function} property.
//	 *
//	 * @return symbol {@link #function} property
//	 */
	public FunctionStruct getFunction() {
		if (functionStack.isEmpty()) {
			return handleUnboundFunction();
		}
		return functionStack.peek();
	}

	private FunctionStruct handleUnboundFunction() {
		String variableName = name;
		final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();

		if (!currentPackage.equals(symbolPackage)) {
			if (symbolPackage == null) {
				variableName = "#:" + name;
			} else {
				final String packageName = symbolPackage.getName();
				if (currentPackage.getExternalSymbols().containsKey(name)) {
					variableName = packageName + ':' + name;
				} else {
					variableName = packageName + "::" + name;
				}
			}
		}

		throw new ErrorException("Undefined function: " + variableName);
	}

	//	/**
//	 * Setter for symbol {@link #function} property.
//	 *
//	 * @param function
//	 * 		new symbol {@link #function} property value
//	 */
	public void setFunction(final FunctionStruct function) {
		if (functionStack.isEmpty()) {
			functionStack.push(function);
		} else {
			functionStack.pop();
			functionStack.push(function);
		}
	}

	public void bindFunction(final FunctionStruct function) {
		functionStack.push(function);
	}

	public FunctionStruct unbindFunction() {
		return functionStack.pop();
	}

	/**
	 * Getter for symbol {@link #macroFunctionExpander} property.
	 *
	 * @return symbol {@link #macroFunctionExpander} property
	 */
	public MacroFunctionExpanderInter getMacroFunctionExpander() {
		return macroFunctionExpander;
	}

	/**
	 * Setter for symbol {@link #macroFunctionExpander} property.
	 *
	 * @param macroFunctionExpander
	 * 		new symbol {@link #macroFunctionExpander} property value
	 */
	public void setMacroFunctionExpander(final MacroFunctionExpanderInter macroFunctionExpander) {
		this.macroFunctionExpander = macroFunctionExpander;
	}

	/**
	 * Getter for symbol {@link #compilerMacroFunctionExpander} property.
	 *
	 * @return symbol {@link #compilerMacroFunctionExpander} property
	 */
	public CompilerMacroFunctionExpanderInter getCompilerMacroFunctionExpander() {
		return compilerMacroFunctionExpander;
	}

	/**
	 * Setter for symbol {@link #compilerMacroFunctionExpander} property.
	 *
	 * @param compilerMacroFunctionExpander
	 * 		new symbol {@link #compilerMacroFunctionExpander} property value
	 */
	public void setCompilerMacroFunctionExpander(final CompilerMacroFunctionExpanderInter compilerMacroFunctionExpander) {
		this.compilerMacroFunctionExpander = compilerMacroFunctionExpander;
	}

	//	/**
//	 * Getter for symbol {@link #symbolMacroExpander} property.
//	 *
//	 * @return symbol {@link #symbolMacroExpander} property
//	 */
	public SymbolMacroExpanderInter getSymbolMacroExpander() {
		if (symbolMacroExpanderStack.isEmpty()) {
			return null;
		}
		return symbolMacroExpanderStack.peek();
	}

	//	/**
//	 * Setter for symbol {@link #symbolMacroExpander} property.
//	 *
//	 * @param symbolMacroExpander
//	 * 		new symbol {@link #symbolMacroExpander} property value
//	 */
	public void setSymbolMacroExpander(final SymbolMacroExpanderInter symbolMacroExpander) {
		symbolMacroExpanderStack.pop();
		symbolMacroExpanderStack.push(symbolMacroExpander);
	}

	public void bindSymbolMacroExpander(final SymbolMacroExpanderInter symbolMacroExpander) {
		symbolMacroExpanderStack.push(symbolMacroExpander);
	}

	public void unbindSymbolMacroExpander() {
		symbolMacroExpanderStack.pop();
	}

	/**
	 * Getter for symbol {@link #properties} property.
	 *
	 * @return symbol {@link #properties} property
	 */
	public ListStruct getProperties() {
		if (properties == null) {
			// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
			//      the constant NIL symbol with a dependence on its existence.
			properties = NILStruct.INSTANCE;
		}
		return properties;
	}

	public void setProperties(final ListStruct properties) {
		this.properties = properties;
	}

	/**
	 * Getter for symbol {@link #structureClass} property.
	 *
	 * @return symbol {@link #structureClass} property
	 */
	public StructureClassStruct getStructureClass() {
		return structureClass;
	}

	/**
	 * Setter for symbol {@link #structureClass} property.
	 *
	 * @param structureClass
	 * 		new symbol {@link #structureClass} property value
	 */
	public void setStructureClass(final StructureClassStruct structureClass) {
		this.structureClass = structureClass;
	}

	/**
	 * Retrieves the property from the symbol {@link #properties} associated with the provided {@code indicator}. If the
	 * property is not found, {@code null} is returned.
	 *
	 * @param indicator
	 * 		the key for the property to retrieve
	 *
	 * @return the property from the symbol {@link #properties} or {@code null} if the property cannot be found.
	 */
	public LispStruct getProperty(final LispStruct indicator, final LispStruct defaultValue) {
		if (properties == null) {
			// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
			//      the constant NIL symbol with a dependence on its existence.
			properties = NILStruct.INSTANCE;
		}
		return properties.getProperty(indicator, defaultValue);
	}

	/**
	 * Sets the property in the symbol {@link #properties} associated with the provided {@code indicator} to the provided
	 * {@code value}.
	 *
	 * @param indicator
	 * 		the key for the property to set
	 * @param newValue
	 * 		the value of the property
	 */
	public ListStruct setProperty(final LispStruct indicator, final LispStruct newValue) {
		if (properties == null) {
			// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
			//      the constant NIL symbol with a dependence on its existence.
			properties = NILStruct.INSTANCE;
		}
		return properties.setProperty(indicator, newValue);
	}

	/**
	 * Removes the first property in the symbol {@link #properties} associated with the provided {@code indicator}.
	 *
	 * @param indicator
	 * 		the key for the property to remove
	 *
	 * @return whether or not the property was removed
	 */
	public boolean removeProperty(final LispStruct indicator) {
		if (properties == null) {
			// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
			//      the constant NIL symbol with a dependence on its existence.
			properties = NILStruct.INSTANCE;
		}
		return properties.removeProperty(indicator);
	}

	/**
	 * Copies the symbol and possibly its {@link #properties}.
	 *
	 * @param copyProperties
	 * 		whether or not to copy the symbol's {@link #properties}
	 *
	 * @return the newly copied symbol
	 */
	public SymbolStructImpl copySymbol(final boolean copyProperties) {
		if (copyProperties) {
			final SymbolStructImpl newSymbol = new SymbolStructImpl(name);
			newSymbol.lexicalValueStack.addAll(lexicalValueStack);
			newSymbol.dynamicValueStack.addAll(dynamicValueStack);
			newSymbol.functionStack.addAll(functionStack);
			if (properties == null) {
				// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
				//      the constant NIL symbol with a dependence on its existence.
				properties = NILStruct.INSTANCE;
			}
			newSymbol.properties = properties.copyList();
			return newSymbol;
		} else {
			return new SymbolStructImpl(name);
		}
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(name)
//		                            .append(symbolPackage)
//		                            .append(functionStack)
                                    .append(properties)
//		                            .append(macroFunctionExpander)
//		                            .append(compilerMacroFunctionExpander)
//		                            .append(symbolMacroExpanderStack)
                                    .toHashCode();
//		                            .append(lexicalValueStack) TODO: why does this cause explosions???
//		                            .append(dynamicValueStack) TODO: why does this cause explosions???
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final SymbolStructImpl rhs = (SymbolStructImpl) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(name, rhs.name)
		                          .append(symbolPackage, rhs.symbolPackage)
		                          .append(lexicalValueStack, rhs.lexicalValueStack)
		                          .append(dynamicValueStack, rhs.dynamicValueStack)
		                          .append(functionStack, rhs.functionStack)
		                          .append(properties, rhs.properties)
		                          .append(macroFunctionExpander, rhs.macroFunctionExpander)
		                          .append(compilerMacroFunctionExpander, rhs.compilerMacroFunctionExpander)
		                          .append(symbolMacroExpanderStack, rhs.symbolMacroExpanderStack)
		                          .isEquals();
	}

	@Override
	public String toString() {
//		final BooleanStruct printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue();

		// TODO: deal with *PRINT-CASE* and *PRINT-ESCAPE*

		if (symbolPackage == null) {
			return "#:" + name;
		}

		// TODO: look into symbols with '|x| pattern...

		if (GlobalPackageStruct.KEYWORD.equals(symbolPackage)) {
			return ':' + name;
		}

		// TODO: the following isn't right. It's more like the symbol is not "accessible" in the current package...
		// TODO: probably by use of 'findSymbol'

		final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();

		PackageSymbolStruct symbol = currentPackage.findSymbol(name);
		if (symbol == null) {
			symbol = symbolPackage.findSymbol(name);

			final String packageName = symbolPackage.getName();

			final boolean externalSymbol = PackageStruct.EXTERNAL_KEYWORD.equals(symbol.getPackageSymbolType());
			if (externalSymbol) {
				// TODO: verify it is a single colon for external symbols when printing...
				return packageName + ':' + name;
			} else {
				return packageName + "::" + name;
			}
		}
		return name;
	}
}
