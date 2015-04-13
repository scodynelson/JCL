package jcl.symbols;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.classes.StructureObjectStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.CompilerMacroFunctionExpander;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.functions.expanders.SymbolMacroExpander;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.types.SymbolType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
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

	protected final List<LispStruct> properties = new ArrayList<>();

	protected PackageStruct symbolPackage;

	protected Stack<TYPE> lexicalValueStack = new Stack<>();

	protected Stack<TYPE> dynamicValueStack = new Stack<>();

	protected Stack<FunctionStruct> functionStack = new Stack<>();

	protected MacroFunctionExpander<?> macroFunctionExpander;

	protected CompilerMacroFunctionExpander<?> compilerMacroFunctionExpander;

	protected Stack<SymbolMacroExpander<?>> symbolMacroExpanderStack = new Stack<>();

	protected StructureObjectStruct structureObject;

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
	public SymbolStruct(final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		this(SymbolType.INSTANCE, name, symbolPackage, value, function);
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
	protected SymbolStruct(final SymbolType symbolType,
	                       final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		super(symbolType, null, null);
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

	public boolean hasValue() {
		return !lexicalValueStack.isEmpty() || !dynamicValueStack.isEmpty();
	}

//	/**
//	 * Getter for symbol {@link #value} property.
//	 *
//	 * @return symbol {@link #value} property
//	 */
	public TYPE getValue() {
		if (lexicalValueStack.isEmpty()) {
			if (dynamicValueStack.isEmpty()) {

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
			return dynamicValueStack.peek();
		}
		return lexicalValueStack.peek();
	}

	public TYPE getLexicalValue() {
		if (lexicalValueStack.isEmpty()) {

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
		return lexicalValueStack.peek();
	}

	public TYPE getDynamicValue() {
		if (dynamicValueStack.isEmpty()) {

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
		return dynamicValueStack.peek();
	}

//	/**
//	 * Setter for symbol {@link #value} property.
//	 *
//	 * @param value
//	 * 		new symbol {@link #value} property value
//	 */
	public void setValue(final TYPE value) {
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

	public void setLexicalValue(final TYPE value) {
		if (lexicalValueStack.isEmpty()) {
			lexicalValueStack.push(value);
		} else {
			lexicalValueStack.pop();
			lexicalValueStack.push(value);
		}
	}

	public void setDynamicValue(final TYPE value) {
		if (dynamicValueStack.isEmpty()) {
			dynamicValueStack.push(value);
		} else {
			dynamicValueStack.pop();
			dynamicValueStack.push(value);
		}
	}

	public void bindLexicalValue(final TYPE value) {
		lexicalValueStack.push(value);
	}

	public void unbindLexicalValue() {
		lexicalValueStack.pop();
	}

	public void bindDynamicValue(final TYPE value) {
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

			throw new ErrorException("Undefined function: " + variableName);
		}
		return functionStack.peek();
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

	public void unbindFunction() {
		functionStack.pop();
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

//	/**
//	 * Getter for symbol {@link #symbolMacroExpander} property.
//	 *
//	 * @return symbol {@link #symbolMacroExpander} property
//	 */
	public SymbolMacroExpander<?> getSymbolMacroExpander() {
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
	public void setSymbolMacroExpander(final SymbolMacroExpander<?> symbolMacroExpander) {
		symbolMacroExpanderStack.pop();
		symbolMacroExpanderStack.push(symbolMacroExpander);
	}

	public void bindSymbolMacroExpander(final SymbolMacroExpander<?> symbolMacroExpander) {
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
	public List<LispStruct> getProperties() {
		return properties;
	}

	/**
	 * Getter for symbol {@link #structureObject} property.
	 *
	 * @return symbol {@link #structureObject} property
	 */
	public StructureObjectStruct getStructureObject() {
		return structureObject;
	}

	/**
	 * Setter for symbol {@link #structureObject} property.
	 *
	 * @param structureObject
	 * 		new symbol {@link #structureObject} property value
	 */
	public void setStructureObject(final StructureObjectStruct structureObject) {
		this.structureObject = structureObject;
	}

	/**
	 * Retrieves the property from the symbol {@link #properties} associated with the provided {@code key}. If the
	 * property is not found, {@link NullStruct#INSTANCE} is returned.
	 *
	 * @param key
	 * 		the key for the property to retrieve
	 *
	 * @return the property from the symbol {@link #properties} or {@link NullStruct#INSTANCE} if the property cannot be
	 * found.
	 */
	public LispStruct getProperty(final LispStruct key) {
		for (int i = 0; i < properties.size(); i += 2) {
			final LispStruct current = properties.get(i);
			if (key.equals(current)) {
				final int valueIndex = i + 1;
				return properties.get(valueIndex);
			}
		}
		return NullStruct.INSTANCE;
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
			final SymbolStruct<TYPE> newSymbol = new SymbolStruct<>(name, symbolPackage);
			newSymbol.lexicalValueStack.addAll(lexicalValueStack);
			newSymbol.dynamicValueStack.addAll(dynamicValueStack);
			newSymbol.functionStack.addAll(functionStack);
			newSymbol.properties.addAll(properties);
			return newSymbol;
		} else {
			return new SymbolStruct<>(name);
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
		final SymbolStruct<?> rhs = (SymbolStruct) obj;
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
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(name)
		                                                                .append(symbolPackage)
		                                                                .append(lexicalValueStack)
		                                                                .append(dynamicValueStack)
		                                                                .append(functionStack)
		                                                                .append(properties)
		                                                                .append(macroFunctionExpander)
		                                                                .append(compilerMacroFunctionExpander)
		                                                                .append(symbolMacroExpanderStack)
		                                                                .toString();
	}
}
