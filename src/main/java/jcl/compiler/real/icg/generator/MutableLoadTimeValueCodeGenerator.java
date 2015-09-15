/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.compiler.real.struct.specialoperator.MutableLoadTimeValueStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'load-time-value' special operator code generation where the value is mutable, or not read-only.
 */
@Component
class MutableLoadTimeValueCodeGenerator implements CodeGenerator<MutableLoadTimeValueStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link MutableLoadTimeValueStruct#form} value.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link MutableLoadTimeValueStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link MutableLoadTimeValueStruct#form}</li>
	 * <li>Retrieving the primary value via {@link ValuesStruct#getPrimaryValue()} if the generated form is a {@link
	 * ValuesStruct}</li>
	 * </ol>
	 * As an example, it will transform {@code (load-time-value 1 nil)} into the following Java code:
	 * <pre>
	 * {@code
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP");
	 *      SymbolStruct var3 = var2.findSymbol("EVAL").getSymbol();
	 *      FunctionStruct var4 = var3.getFunction();
	 *
	 *      LispStruct[] var5 = new LispStruct[1];
	 *      BigInteger var7 = new BigInteger("1");
	 *      IntegerStruct var6 = new IntegerStruct(var7);
	 *      var5[0] = var6;
	 *
	 *      LispStruct var8 = var4.apply(var5);
	 *      if(var8 instanceof ValuesStruct) {
	 *          ValuesStruct var9 = (ValuesStruct)var8;
	 *          var8 = var9.getPrimaryValue();
	 *      }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link MutableLoadTimeValueStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final MutableLoadTimeValueStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final LispStruct form = input.getForm();
		codeGenerator.generate(form, generatorState);

		final int initFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

		CodeGenerators.generateValuesCheckAndStore(methodBuilder, initFormStore);
		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
	}
}
