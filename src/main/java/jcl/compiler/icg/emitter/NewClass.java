package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.objectweb.asm.ClassWriter;

public final class NewClass extends BuiltInFunctionStructImpl {

	public NewClass() {
		super("Emitter function for starting a new class for generation",
		      EmitterSymbols.NEW_CLASS.getName(),
		      Parameters.forFunction(EmitterSymbols.NEW_CLASS.getName())
		                .requiredParameter("access")
		                .requiredParameter("name")
		                .requiredParameter("signature")
		                .requiredParameter("super-name")
		                .requiredParameter("interfaces")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct access = arguments.getRequiredArgument("access", IntegerStruct.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct signature = arguments.getRequiredArgument("signature", StringStruct.class);
		final StringStruct superName = arguments.getRequiredArgument("super-name", StringStruct.class);
		final ListStruct interfaces = arguments.getRequiredArgument("interfaces", ListStruct.class);

		final String[] interfacesArray
				= interfaces.stream()
				            .map(StringStruct.class::cast)
				            .map(StringStruct::toJavaString)
				            .toArray(value -> new String[interfaces.length().toJavaInt()]);

		final ClassDef classDef = new ClassDef(new ClassWriter(ClassWriter.COMPUTE_MAXS));
		return classDef.visit(access.toJavaInt(), name.toJavaString(), signature.toJavaString(),
		                      superName.toJavaString(), interfacesArray);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.NEW_CLASS;
	}
}