package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EmitSourceFile extends BuiltInFunctionStructImpl {

	public EmitSourceFile() {
		super("Emitter function to emit the Java source file",
		      EmitterSymbols.EMIT_SOURCE_FILE.getName(),
		      Parameters.forFunction(EmitterSymbols.EMIT_SOURCE_FILE.getName())
		                .requiredParameter("class-def")
		                .requiredParameter("source")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ClassDef classDef = arguments.getRequiredArgument("class-def", ClassDef.class);
		final StringStruct source = arguments.getRequiredArgument("source", StringStruct.class);
		return classDef.emitSource(source.toJavaString());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.EMIT_SOURCE_FILE;
	}
}