package jcl.compiler.icg.emitter.opcodes;

import java.util.List;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.HandleWrapper;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Invokedynamic extends BuiltInFunctionStructImpl {

	public Invokedynamic() {
		super("Emitter function for Java opcode INVOKEDYNAMIC",
		      EmitterSymbols.INVOKEDYNAMIC.getName(),
		      Parameters.forFunction(EmitterSymbols.INVOKEDYNAMIC.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("name")
		                .requiredParameter("descriptor")
		                .requiredParameter("handle")
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct descriptor = arguments.getRequiredArgument("descriptor", StringStruct.class);
		final HandleWrapper handle = arguments.getRequiredArgument("handle", HandleWrapper.class);
		final List<LispStruct> bootstrapMethodArguments = arguments.getRestArgument();
		// TODO: validate method arguments (see JavaDoc of emitInvokedynamic for requirements)
		return methodDef.emitInvokedynamic(name.toJavaString(), descriptor.toJavaString(), handle.getHandle(),
		                                   bootstrapMethodArguments.toArray());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.INVOKEDYNAMIC;
	}
}