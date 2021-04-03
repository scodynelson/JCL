package jcl.lang.internal;

import java.util.Stack;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.classes.StructureClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.expander.CompilerMacroFunctionExpanderInter;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.statics.PackageVariables;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link SymbolStructImpl} is the object representation of a Lisp 'symbol' type.
 */
public class SymbolStructImpl extends LispStructImpl implements SymbolStruct {

	protected final String name;

	protected ListStruct properties;

	protected PackageStruct symbolPackage;

	protected LispStruct value;

	protected FunctionStruct function;

	protected FunctionStruct setfFunction;

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
	public SymbolStructImpl(final String name) {
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
		this(name, symbolPackage, value, null);
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
			// TODO: some hacks
			final PackageStruct tempPkg = symbolPackage;
			symbolPackage = null;
			tempPkg.importSymbol(this);
			// TODO: we REALLY shouldn't be exporting here, BUT so we can test things right now, we will.
			tempPkg.export(this);
			symbolPackage = tempPkg;
		}
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
	public BooleanStruct hasValue() {
		return BooleanStruct.toLispBoolean(value != null);
	}

	//	/**
//	 * Getter for symbol {@link #value} property.
//	 *
//	 * @return symbol {@link #value} property
//	 */
	@Override
	public LispStruct getValue() {
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
				if (currentPackage.findExternalSymbol(name).isPresent()) {
					variableName = packageName + ':' + name;
				} else {
					variableName = packageName + "::" + name;
				}
			}
		}

		throw new ErrorException("Unbound variable: " + variableName);
	}

	@Override
	public void setValue(final LispStruct value) {
		this.value = value;
	}

	@Override
	public boolean hasFunction() {
		return function != null;
	}

	@Override
	public FunctionStruct getFunction() {
		if (function == null) {
			return handleUnboundFunction();
		}
		return function;
	}

	private FunctionStruct handleUnboundFunction() {
		String variableName = name;
		final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();

		if (!currentPackage.eq(symbolPackage)) {
			if (symbolPackage == null) {
				variableName = "#:" + name;
			} else {
				final String packageName = symbolPackage.getName();
				if (currentPackage.findExternalSymbol(name).isPresent()) {
					variableName = packageName + ':' + name;
				} else {
					variableName = packageName + "::" + name;
				}
			}
		}

		throw new ErrorException("Undefined function: " + variableName);
	}

	@Override
	public void setFunction(final FunctionStruct function) {
		this.function = function;
	}

	@Override
	public void setSetfFunction(final FunctionStruct function) {
		setfFunction = function;
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
	public BooleanStruct removeProperty(final LispStruct indicator) {
		if (properties == null) {
			// We MUST lazy load this. Because NIlStruct is a symbol and can have its own Plist, but we can't initialize
			//      the constant NIL symbol with a dependence on its existence.
			properties = NILStruct.INSTANCE;
		}
		return BooleanStruct.toLispBoolean(properties.remf(indicator));
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
	public SymbolStruct copySymbol(final BooleanStruct copyProperties) {
		if (copyProperties.toJavaPBoolean()) {
			final SymbolStructImpl newSymbol = new SymbolStructImpl(name);
			newSymbol.value = value;
			newSymbol.function = function;
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
		final JavaEnvironmentMethodBuilder methodBuilder = generatorState.getCurrentEnvironmentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(this, generatorState, packageStore, symbolStore);

		final int environmentStore = methodBuilder.getEnvironmentStore();

		mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

		if (generatorState.getLexicalSymbols().contains(this)) {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.ENVIRONMENT_NAME,
			                   GenerationConstants.ENVIRONMENT_GET_LEXICAL_SYMBOL_VALUE_METHOD_NAME,
			                   GenerationConstants.ENVIRONMENT_GET_LEXICAL_SYMBOL_VALUE_METHOD_DESC,
			                   false);
		} else if (generatorState.getDynamicSymbols().contains(this)) {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.ENVIRONMENT_NAME,
			                   GenerationConstants.ENVIRONMENT_GET_DYNAMIC_SYMBOL_VALUE_METHOD_NAME,
			                   GenerationConstants.ENVIRONMENT_GET_DYNAMIC_SYMBOL_VALUE_METHOD_DESC,
			                   false);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.ENVIRONMENT_NAME,
			                   GenerationConstants.ENVIRONMENT_GET_SYMBOL_VALUE_METHOD_NAME,
			                   GenerationConstants.ENVIRONMENT_GET_SYMBOL_VALUE_METHOD_DESC,
			                   false);
		}
	}

	@Override
	public LispStruct typeOf() {
		if (symbolPackage == GlobalPackageStruct.KEYWORD) {
			return CommonLispSymbols.KEYWORD;
		}
		if (this == TStruct.INSTANCE) {
			return CommonLispSymbols.BOOLEAN;
		}
		return CommonLispSymbols.SYMBOL;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.SYMBOL;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.SYMBOL) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SYMBOL) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.KEYWORD) {
			return (symbolPackage == GlobalPackageStruct.KEYWORD) ? TStruct.INSTANCE : NILStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.BOOLEAN) {
			return (this == TStruct.INSTANCE) ? TStruct.INSTANCE : NILStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
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

		final PackageSymbolStruct symbol = currentPackage.findSymbol(name);
		if (symbol.notFound()) {
			final String packageName = symbolPackage.getName();

			if (currentPackage.findExternalSymbol(name).isPresent()) {
				// TODO: verify it is a single colon for external symbols when printing...
				return packageName + ':' + name;
			} else {
				return packageName + "::" + name;
			}
		}
		return name;
	}
}
