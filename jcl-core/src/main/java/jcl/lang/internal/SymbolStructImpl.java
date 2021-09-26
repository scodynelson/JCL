package jcl.lang.internal;

import java.util.Optional;

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
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.UnboundVariableException;
import jcl.lang.condition.exception.UndefinedFunctionException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link SymbolStructImpl} is the object representation of a Lisp 'symbol' type.
 */
public class SymbolStructImpl extends LispStructImpl implements SymbolStruct {

	/**
	 * The name of the symbol. A symbol will always have a name, even if the name is auto-generated.
	 */
	protected final String name;

	/**
	 * The property list of the symbol.
	 * <p>
	 * NOTE: We MUST lazy load this. Because {@link NILStruct} is a symbol and can have its own Plist, but we can't
	 * initialize the constant NIL symbol with a dependence on its existence.
	 */
	protected ListStruct properties;

	/**
	 * The package of the symbol.
	 */
	protected PackageStruct symbolPackage;

	/**
	 * The value of the symbol.
	 */
	protected LispStruct value;

	/**
	 * The function value of the symbol.
	 */
	protected FunctionStruct function;

	/**
	 * Whether or not the symbol is a constant.
	 */
	protected boolean isConstant;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 */
	public SymbolStructImpl(final String name) {
		this.name = name;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public Optional<PackageStruct> getSymbolPackage() {
		return Optional.ofNullable(symbolPackage);
	}

	@Override
	public void setSymbolPackage(final PackageStruct symbolPackage) {
		this.symbolPackage = symbolPackage;
	}

	@Override
	public void setConstant() {
		isConstant = true;
	}

	@Override
	public StringStruct symbolName() {
		return StringStruct.toLispString(name);
	}

	@Override
	public LispStruct symbolPackage() {
		return (symbolPackage == null) ? NILStruct.INSTANCE : symbolPackage;
	}

	@Override
	public BooleanStruct boundP() {
		return BooleanStruct.toLispBoolean(value != null);
	}

	@Override
	public SymbolStruct makunbound() {
		setfSymbolValue(null);
		return this;
	}

	@Override
	public LispStruct symbolValue() {
		if (value == null) {
			throw new UnboundVariableException("Unbound variable: " + this);
		}
		return value;
	}

	@Override
	public LispStruct setfSymbolValue(final LispStruct newValue) {
		if (isConstant) {
			throw new ProgramErrorException("Can't set value for constant " + name + '.');
		}
		value = newValue;
		return newValue;
	}

	@Override
	public BooleanStruct fBoundP() {
		return BooleanStruct.toLispBoolean(function != null);
	}

	@Override
	public SymbolStruct fMakunbound() {
		setfSymbolFunction(null);
		return this;
	}

	@Override
	public FunctionStruct symbolFunction() {
		if (function == null) {
			throw new UndefinedFunctionException("Undefined function: " + this);
		}
		return function;
	}

	@Override
	public FunctionStruct setfSymbolFunction(final FunctionStruct newFunction) {
		function = newFunction;
		return newFunction;
	}

	@Override
	public ListStruct symbolPlist() {
		if (properties == null) {
			properties = NILStruct.INSTANCE;
		}
		return properties;
	}

	@Override
	public ListStruct setfSymbolPlist(final ListStruct newPlist) {
		properties = newPlist;
		return newPlist;
	}

	@Override
	public LispStruct getProp(final LispStruct indicator, final LispStruct defaultValue) {
		if (properties == null) {
			return defaultValue;
		}
		return properties.getf(indicator, defaultValue);
	}

	@Override
	public LispStruct setProp(final LispStruct indicator, final LispStruct newValue) {
		if (properties == null) {
			properties = NILStruct.INSTANCE;
		}
		properties = properties.putf(indicator, newValue);
		return newValue;
	}

	@Override
	public BooleanStruct remProp(final LispStruct indicator) {
		if (properties == null) {
			return NILStruct.INSTANCE;
		}
		return BooleanStruct.toLispBoolean(properties.remf(indicator));
	}

	@Override
	public SymbolStruct copySymbol(final BooleanStruct copyProperties) {
		if (copyProperties.toJavaPBoolean()) {
			final SymbolStructImpl newSymbol = new SymbolStructImpl(name);
			newSymbol.value = value;
			newSymbol.function = function;
			if (properties == null) {
				// We MUST lazy load this. Because NILStruct is a symbol and can have its own Plist, but we can't initialize
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
//		final BooleanStruct printEscape = CommonLispSymbols.PRINT_ESCAPE_VAR.getVariableValue();

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

		final PackageStruct currentPackage = CommonLispSymbols.PACKAGE_VAR.getVariableValue();

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
