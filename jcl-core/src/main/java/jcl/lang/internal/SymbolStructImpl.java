package jcl.lang.internal;

import java.util.Stack;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.StructureClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.expander.CompilerMacroFunctionExpanderInter;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.statics.PackageVariables;
import jcl.type.LispType;
import jcl.type.SymbolType;
import lombok.EqualsAndHashCode;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link SymbolStructImpl} is the object representation of a Lisp 'symbol' type.
 */
@EqualsAndHashCode(of = {"name", "symbolPackage"}, callSuper = false)
public class SymbolStructImpl extends BuiltInClassStruct implements SymbolStruct {

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

	public static SymbolStruct valueOf(final String name) {
		return new SymbolStructImpl(name);
	}

	/**
	 * Getter for symbol {@link #name} property.
	 *
	 * @return symbol {@link #name} property
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * Getter for symbol {@link #symbolPackage} property.
	 *
	 * @return symbol {@link #symbolPackage} property
	 */
	@Override
	public PackageStruct getSymbolPackage() {
		return symbolPackage;
	}

	/**
	 * Setter for symbol {@link #symbolPackage} property.
	 *
	 * @param symbolPackage
	 * 		new symbol {@link #symbolPackage} property value
	 */
	@Override
	public void setSymbolPackage(final PackageStruct symbolPackage) {
		this.symbolPackage = symbolPackage;
	}

	@Override
	public boolean hasValue() {
		return !lexicalValueStack.isEmpty() || !dynamicValueStack.isEmpty();
	}

	//	/**
//	 * Getter for symbol {@link #value} property.
//	 *
//	 * @return symbol {@link #value} property
//	 */
	@Override
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

	@Override
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

	@Override
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

		if (!currentPackage.eq(symbolPackage)) {
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
	@Override
	public void setValue(final LispStruct value) {
		if (lexicalValueStack.isEmpty()) {
			setDynamicValue(value);
		} else {
			setLexicalValue(value);
		}
	}

	@Override
	public void setLexicalValue(final LispStruct value) {
		if (lexicalValueStack.isEmpty()) {
			lexicalValueStack.push(value);
		} else {
			lexicalValueStack.pop();
			lexicalValueStack.push(value);
		}
	}

	@Override
	public void setDynamicValue(final LispStruct value) {
		if (dynamicValueStack.isEmpty()) {
			dynamicValueStack.push(value);
		} else {
			dynamicValueStack.pop();
			dynamicValueStack.push(value);
		}
	}

	@Override
	public void bindLexicalValue(final LispStruct value) {
		lexicalValueStack.push(value);
	}

	@Override
	public void unbindLexicalValue() {
		lexicalValueStack.pop();
	}

	@Override
	public void bindDynamicValue(final LispStruct value) {
		dynamicValueStack.push(value);
	}

	@Override
	public void unbindDynamicValue() {
		dynamicValueStack.pop();
	}

	@Override
	public boolean hasFunction() {
		return !functionStack.isEmpty();
	}

	//	/**
//	 * Getter for symbol {@link #function} property.
//	 *
//	 * @return symbol {@link #function} property
//	 */
	@Override
	public FunctionStruct getFunction() {
		if (functionStack.isEmpty()) {
			return handleUnboundFunction();
		}
		return functionStack.peek();
	}

	private FunctionStruct handleUnboundFunction() {
		String variableName = name;
		final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();

		if (!currentPackage.eq(symbolPackage)) {
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
	@Override
	public void setFunction(final FunctionStruct function) {
		if (functionStack.isEmpty()) {
			functionStack.push(function);
		} else {
			functionStack.pop();
			functionStack.push(function);
		}
	}

	@Override
	public void bindFunction(final FunctionStruct function) {
		functionStack.push(function);
	}

	@Override
	public FunctionStruct unbindFunction() {
		return functionStack.pop();
	}

	/**
	 * Getter for symbol {@link #macroFunctionExpander} property.
	 *
	 * @return symbol {@link #macroFunctionExpander} property
	 */
	@Override
	public MacroFunctionExpanderInter getMacroFunctionExpander() {
		return macroFunctionExpander;
	}

	/**
	 * Setter for symbol {@link #macroFunctionExpander} property.
	 *
	 * @param macroFunctionExpander
	 * 		new symbol {@link #macroFunctionExpander} property value
	 */
	@Override
	public void setMacroFunctionExpander(final MacroFunctionExpanderInter macroFunctionExpander) {
		this.macroFunctionExpander = macroFunctionExpander;
	}

	/**
	 * Getter for symbol {@link #compilerMacroFunctionExpander} property.
	 *
	 * @return symbol {@link #compilerMacroFunctionExpander} property
	 */
	@Override
	public CompilerMacroFunctionExpanderInter getCompilerMacroFunctionExpander() {
		return compilerMacroFunctionExpander;
	}

	/**
	 * Setter for symbol {@link #compilerMacroFunctionExpander} property.
	 *
	 * @param compilerMacroFunctionExpander
	 * 		new symbol {@link #compilerMacroFunctionExpander} property value
	 */
	@Override
	public void setCompilerMacroFunctionExpander(final CompilerMacroFunctionExpanderInter compilerMacroFunctionExpander) {
		this.compilerMacroFunctionExpander = compilerMacroFunctionExpander;
	}

	//	/**
//	 * Getter for symbol {@link #symbolMacroExpander} property.
//	 *
//	 * @return symbol {@link #symbolMacroExpander} property
//	 */
	@Override
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
	@Override
	public void setSymbolMacroExpander(final SymbolMacroExpanderInter symbolMacroExpander) {
		symbolMacroExpanderStack.pop();
		symbolMacroExpanderStack.push(symbolMacroExpander);
	}

	@Override
	public void bindSymbolMacroExpander(final SymbolMacroExpanderInter symbolMacroExpander) {
		symbolMacroExpanderStack.push(symbolMacroExpander);
	}

	@Override
	public void unbindSymbolMacroExpander() {
		symbolMacroExpanderStack.pop();
	}

	/**
	 * Getter for symbol {@link #properties} property.
	 *
	 * @return symbol {@link #properties} property
	 */
	@Override
	public ListStruct getProperties() {
		if (properties == null) {
			// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
			//      the constant NIL symbol with a dependence on its existence.
			properties = NILStruct.INSTANCE;
		}
		return properties;
	}

	@Override
	public void setProperties(final ListStruct properties) {
		this.properties = properties;
	}

	/**
	 * Getter for symbol {@link #structureClass} property.
	 *
	 * @return symbol {@link #structureClass} property
	 */
	@Override
	public StructureClassStruct getStructureClass() {
		return structureClass;
	}

	/**
	 * Setter for symbol {@link #structureClass} property.
	 *
	 * @param structureClass
	 * 		new symbol {@link #structureClass} property value
	 */
	@Override
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
	@Override
	public LispStruct getProperty(final LispStruct indicator, final LispStruct defaultValue) {
		if (properties == null) {
			// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
			//      the constant NIL symbol with a dependence on its existence.
			properties = NILStruct.INSTANCE;
		}
		return properties.getf(indicator, defaultValue);
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
	@Override
	public ListStruct setProperty(final LispStruct indicator, final LispStruct newValue) {
		if (properties == null) {
			// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
			//      the constant NIL symbol with a dependence on its existence.
			properties = NILStruct.INSTANCE;
		}
		return properties.putf(indicator, newValue);
	}

	/**
	 * Removes the first property in the symbol {@link #properties} associated with the provided {@code indicator}.
	 *
	 * @param indicator
	 * 		the key for the property to remove
	 *
	 * @return whether or not the property was removed
	 */
	@Override
	public boolean removeProperty(final LispStruct indicator) {
		if (properties == null) {
			// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
			//      the constant NIL symbol with a dependence on its existence.
			properties = NILStruct.INSTANCE;
		}
		return properties.remf(indicator);
	}

	/**
	 * Copies the symbol and possibly its {@link #properties}.
	 *
	 * @param copyProperties
	 * 		whether or not to copy the symbol's {@link #properties}
	 *
	 * @return the newly copied symbol
	 */
	@Override
	public SymbolStruct copySymbol(final boolean copyProperties) {
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

	/*
	LISP-STRUCT
	 */

	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(this, generatorState, packageStore, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

		final Environment currentEnvironment = generatorState.getCurrentEnvironment();

		final boolean hasLexicalBinding = currentEnvironment.hasLexicalBinding(this);
		final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(this);

		if (hasLexicalBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_DESC,
			                   true);
		} else if (hasDynamicBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_DESC,
			                   true);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_VALUE_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_VALUE_METHOD_DESC,
			                   true);
		}
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
//		final BooleanStruct printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue();

		// TODO: deal with *PRINT-CASE* and *PRINT-ESCAPE*

		if (symbolPackage == null) {
			return "#:" + name;
		}

		// TODO: look into symbols with '|x| pattern...

		if (GlobalPackageStruct.KEYWORD.eq(symbolPackage)) {
			return ':' + name;
		}

		// TODO: the following isn't right. It's more like the symbol is not "accessible" in the current package...
		// TODO: probably by use of 'findSymbol'

		final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();

		PackageSymbolStruct symbol = currentPackage.findSymbol(name);
		if (symbol == null) {
			symbol = symbolPackage.findSymbol(name);

			final String packageName = symbolPackage.getName();

			final boolean externalSymbol = PackageStructImpl.EXTERNAL_KEYWORD.eq(symbol.getPackageSymbolType());
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
