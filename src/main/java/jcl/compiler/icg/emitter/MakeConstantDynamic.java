package jcl.compiler.icg.emitter;

import java.util.List;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.objectweb.asm.ConstantDynamic;

public final class MakeConstantDynamic extends BuiltInFunctionStructImpl {

	public MakeConstantDynamic() {
		super("Emitter function to make a new constant-dynamic",
		      EmitterSymbols.MAKE_CONSTANT_DYNAMIC.getName(),
		      Parameters.forFunction(EmitterSymbols.MAKE_CONSTANT_DYNAMIC.getName())
		                .requiredParameter("name")
		                .requiredParameter("descriptor")
		                .requiredParameter("bootstrap-method")
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct descriptor = arguments.getRequiredArgument("descriptor", StringStruct.class);
		final HandleWrapper bootstrapMethod = arguments.getRequiredArgument("bootstrap-method", HandleWrapper.class);
		final List<LispStruct> bootstrapMethodArguments = arguments.getRestArgument();
		return new ConstantDynamicWrapper(new ConstantDynamic(
				name.toJavaString(), descriptor.toJavaString(), bootstrapMethod.getHandle(),
				bootstrapMethodArguments.toArray()
		));
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.MAKE_CONSTANT_DYNAMIC;
	}
}