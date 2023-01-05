package jcl.lang;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.internal.SymbolStructImpl;
import jcl.lang.statics.GlobalPackageStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

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
	 * {@inheritDoc} Generation method for {@code TStruct} objects, by retrieving the static singleton
	 * {@link TStruct#INSTANCE}.
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  GenerationConstants.T_STRUCT_NAME,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  GenerationConstants.T_STRUCT_DESC);
	}
}
