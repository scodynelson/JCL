package jcl.symbols;

import jcl.LispStruct;
import jcl.compiler.real.sa.analyzer.expander.CompilerMacroFunctionExpander;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.sa.analyzer.expander.SymbolMacroExpander;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.PackageStruct;
import jcl.types.Symbol;

public class ConstantStruct<TYPE extends LispStruct> extends SymbolStruct<TYPE> {

	private static final long serialVersionUID = -2257699556001691329L;

	protected ConstantStruct(final Symbol symbolType,
	                         final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		super(symbolType, name, symbolPackage, value, function);
	}

	public ConstantStruct(final String name, final PackageStruct symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
	}

	@Override
	public void setSymbolPackage(final PackageStruct symbolPackage) {
		throw new ProgramErrorException("Can't set package for constant " + name + '.');
	}

	@Override
	public void setValue(final TYPE value) {
		throw new ProgramErrorException("Can't set value for constant " + name + '.');
	}

	@Override
	public void bindLexicalValue(final TYPE value) {
		throw new ProgramErrorException("Can't bind value for constant " + name + '.');
	}

	@Override
	public void unbindLexicalValue() {
		throw new ProgramErrorException("Can't unbind value for constant " + name + '.');
	}

	@Override
	public void bindDynamicValue(final TYPE value) {
		throw new ProgramErrorException("Can't bind value for constant " + name + '.');
	}

	@Override
	public void unbindDynamicValue() {
		throw new ProgramErrorException("Can't unbind value for constant " + name + '.');
	}

	@Override
	public void setFunction(final FunctionStruct function) {
		throw new ProgramErrorException("Can't set function for constant " + name + '.');
	}

	@Override
	public void bindFunction(final FunctionStruct function) {
		throw new ProgramErrorException("Can't bind function for constant " + name + '.');
	}

	@Override
	public void unbindFunction() {
		throw new ProgramErrorException("Can't unbind function for constant " + name + '.');
	}

	@Override
	public void setMacroFunctionExpander(final MacroFunctionExpander<?> macroFunctionExpander) {
		throw new ProgramErrorException("Can't set macro function expansion for constant " + name + '.');
	}

	@Override
	public void setCompilerMacroFunctionExpander(final CompilerMacroFunctionExpander<?> compilerMacroFunctionExpander) {
		throw new ProgramErrorException("Can't set compiler macro function expansion for constant " + name + '.');
	}

	@Override
	public void setSymbolMacroExpander(final SymbolMacroExpander<?> symbolMacroExpander) {
		throw new ProgramErrorException("Can't set symbol macro expansion for constant " + name + '.');
	}

	@Override
	public void setProperty(final LispStruct key, final LispStruct value) {
		throw new ProgramErrorException("Can't set properties for constant " + name + '.');
	}
}
