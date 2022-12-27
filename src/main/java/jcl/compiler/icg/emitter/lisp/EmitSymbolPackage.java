package jcl.compiler.icg.emitter.lisp;

import java.util.Optional;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EmitSymbolPackage extends BuiltInFunctionStructImpl {

	public EmitSymbolPackage() {
		super("Emitter function for emitting a symbol package",
		      EmitterSymbols.EMIT_SYMBOL_PACKAGE.getName(),
		      Parameters.forFunction(EmitterSymbols.EMIT_SYMBOL_PACKAGE.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("symbol")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final SymbolStruct symbol = arguments.getRequiredArgument("symbol", SymbolStruct.class);

		final Optional<PackageStruct> pkg = symbol.getSymbolPackage();
		if (pkg.isPresent()) {
			methodDef.emitLdc(pkg.get().getName());
			methodDef.emitInvokestatic(GenerationConstants.PACKAGE_STRUCT_NAME,
			                           GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME,
			                           GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_DESC,
			                           true);
		}
		return methodDef;
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.EMIT_SYMBOL_PACKAGE;
	}
}