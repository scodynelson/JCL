package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Invokestatic extends BuiltInFunctionStructImpl {

	public Invokestatic() {
		super("Emitter function for Java opcode INVOKESTATIC",
		      EmitterSymbols.INVOKESTATIC.getName(),
		      Parameters.forFunction(EmitterSymbols.INVOKESTATIC.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("owner")
		                .requiredParameter("name")
		                .requiredParameter("descriptor")
		                .optionalParameter("interfacep").withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final StringStruct owner = arguments.getRequiredArgument("owner", StringStruct.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct descriptor = arguments.getRequiredArgument("descriptor", StringStruct.class);
		final BooleanStruct isInterface = arguments.getOptionalArgument("descriptor", BooleanStruct.class);
		return methodDef.emitInvokestatic(owner.toJavaString(), name.toJavaString(), descriptor.toJavaString(),
		                                  isInterface.toJavaPBoolean());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.INVOKESTATIC;
	}
}