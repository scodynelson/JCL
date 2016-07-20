/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.GlobalPackageStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link KeywordStruct} objects dynamically by utilizing the {@link KeywordStruct#name} value to
 * retrieve the {@link KeywordStruct} instance from the global {@link GlobalPackageStruct#KEYWORD} package.
 */
@Component
final class KeywordCodeGenerator implements CodeGenerator<KeywordStruct> {

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
	 * {@link KeywordStruct#name} value of the provided {@link KeywordStruct}</li>
	 * <li>Retrieving the {@link SymbolStruct} value via the {@link PackageSymbolStruct#getSymbol()} method</li>
	 * <li>Casting the {@link SymbolStruct} to the appropriate {@link KeywordStruct} type</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link KeywordStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<KeywordStruct> event) {
		final KeywordStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  GenerationConstants.GLOBAL_PACKAGE_STRUCT_NAME,
		                  KEYWORD_PACKAGE_NAME,
		                  GenerationConstants.PACKAGE_STRUCT_DESC);
		final int packageStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, packageStore);

		mv.visitVarInsn(Opcodes.ALOAD, packageStore);
		final String keywordName = input.getName();
		mv.visitLdcInsn(keywordName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.PACKAGE_STRUCT_NAME,
		                   GenerationConstants.PACKAGE_STRUCT_INTERN_METHOD_NAME,
		                   GenerationConstants.PACKAGE_STRUCT_INTERN_METHOD_DESC,
		                   false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.PACKAGE_SYMBOL_STRUCT_NAME,
		                   GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME,
		                   GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_DESC,
		                   false);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.KEYWORD_STRUCT_NAME);
	}
}