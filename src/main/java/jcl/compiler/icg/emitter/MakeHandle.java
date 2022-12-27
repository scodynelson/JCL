package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.objectweb.asm.Handle;

public final class MakeHandle extends BuiltInFunctionStructImpl {

	public MakeHandle() {
		super("Emitter function to make a new handle",
		      EmitterSymbols.MAKE_HANDLE.getName(),
		      Parameters.forFunction(EmitterSymbols.MAKE_HANDLE.getName())
		                .requiredParameter("tag")
		                .requiredParameter("owner")
		                .requiredParameter("name")
		                .requiredParameter("descriptor")
		                .requiredParameter("interfacep")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct tag = arguments.getRequiredArgument("tag", IntegerStruct.class);
		final StringStruct owner = arguments.getRequiredArgument("owner", StringStruct.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct descriptor = arguments.getRequiredArgument("descriptor", StringStruct.class);
		final BooleanStruct isInterface = arguments.getRequiredArgument("interfacep", BooleanStruct.class);
		return new HandleWrapper(new Handle(tag.toJavaInt(), owner.toJavaString(), name.toJavaString(),
		                                    descriptor.toJavaString(), isInterface.toJavaPBoolean()));
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.MAKE_HANDLE;
	}
}