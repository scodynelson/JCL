package jcl.lang;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.internal.SymbolStructImpl;
import jcl.lang.statics.GlobalPackageStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * The {@link TStruct} is the object representation of a Lisp 't' type.
 */
public final class TStruct extends SymbolStructImpl implements BooleanStruct {

	/**
	 * Global constant singleton instance of 't'.
	 */
	public static final TStruct INSTANCE = new TStruct();

	static {
		INSTANCE.setfSymbolValue(INSTANCE);
		INSTANCE.setConstant();

		GlobalPackageStruct.COMMON_LISP.importSymbol(INSTANCE);
		GlobalPackageStruct.COMMON_LISP.export(INSTANCE);
		INSTANCE.setSymbolPackage(GlobalPackageStruct.COMMON_LISP);
	}

	/**
	 * Private constructor.
	 */
	private TStruct() {
		super("T");
	}

	/*
	BOOLEAN-STRUCT
	 */

	@Override
	public boolean toJavaPBoolean() {
		return true;
	}
/*
	LISP-STRUCT
	 */

	/**
	 * Constant {@link String} containing the name for the {@link TStruct} class.
	 */
	private static final String T_STRUCT_NAME = Type.getInternalName(TStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link TStruct} class.
	 */
	private static final String T_STRUCT_DESC = Type.getDescriptor(TStruct.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@code TStruct} objects, by retrieving the static singleton {@link TStruct#INSTANCE}.
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  T_STRUCT_NAME,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  T_STRUCT_DESC);
	}
}
