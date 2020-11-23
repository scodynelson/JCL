package jcl.lang.internal;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.KeywordStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.GlobalPackageStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link KeywordStructImpl} is the object representation of a Lisp 'keyword' type.
 */
public final class KeywordStructImpl extends ConstantStructImpl<KeywordStructImpl> implements KeywordStruct {

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 */
	public KeywordStructImpl(final String name) {
		super(name, GlobalPackageStruct.KEYWORD, null, null);
		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		dynamicValueStack.push(this);
		symbolPackage = KeywordPackageStructImpl.INSTANCE;
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * Constant {@link String} containing the name of the {@link GlobalPackageStruct#KEYWORD} field.
	 */
	private static final String KEYWORD_PACKAGE_NAME = "KEYWORD";

	/**
	 * {@inheritDoc}
	 * Generation method for {@link KeywordStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving the {@link GlobalPackageStruct#KEYWORD} singleton instance</li>
	 * <li>Retrieving the {@link PackageSymbolStruct} via the {@link PackageStruct#findSymbol(String)} method with the
	 * {@link KeywordStruct#getName()} value of the provided {@link KeywordStruct}</li>
	 * <li>Retrieving the {@link SymbolStruct} value via the {@literal PackageSymbolStruct#getSymbol()} method</li>
	 * <li>Casting the {@link SymbolStruct} to the appropriate {@link KeywordStruct} type</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  GenerationConstants.GLOBAL_PACKAGE_STRUCT_NAME,
		                  KEYWORD_PACKAGE_NAME,
		                  GenerationConstants.PACKAGE_STRUCT_DESC);
		final int packageStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, packageStore);

		mv.visitVarInsn(Opcodes.ALOAD, packageStore);
		final String keywordName = getName();
		mv.visitLdcInsn(keywordName);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.PACKAGE_STRUCT_NAME,
		                   GenerationConstants.PACKAGE_STRUCT_INTERN_METHOD_NAME,
		                   GenerationConstants.PACKAGE_STRUCT_INTERN_METHOD_DESC,
		                   true);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.PACKAGE_SYMBOL_STRUCT_NAME,
		                   GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME,
		                   GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_DESC,
		                   false);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.KEYWORD_STRUCT_NAME);
	}
}
