package jcl.compiler.icg.emitter.lisp;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EmitPackage extends BuiltInFunctionStructImpl {

	public EmitPackage() {
		super("Emitter function for emitting a lisp package",
		      EmitterSymbols.EMIT_PACKAGE.getName(),
		      Parameters.forFunction(EmitterSymbols.EMIT_PACKAGE.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("package")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final PackageStruct pkg = arguments.getRequiredArgument("package", PackageStruct.class);

		methodDef.emitLdc(pkg.getName());
		methodDef.emitInvokestatic(GenerationConstants.PACKAGE_STRUCT_NAME,
		                           GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME,
		                           GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_DESC,
		                           true);
		return methodDef;
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.EMIT_PACKAGE;
	}
}